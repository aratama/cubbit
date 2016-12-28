var BABYLON;
(function(BABYLON) {
    var WaterMaterialDefines = (function(_super) {
        __extends(WaterMaterialDefines, _super);

        function WaterMaterialDefines() {
            var _this = _super.call(this) || this;
            _this.BUMP = false;
            _this.REFLECTION = false;
            _this.CLIPPLANE = false;
            _this.ALPHATEST = false;
            _this.POINTSIZE = false;
            _this.FOG = false;
            _this.NORMAL = false;
            _this.UV1 = false;
            _this.UV2 = false;
            _this.VERTEXCOLOR = false;
            _this.VERTEXALPHA = false;
            _this.NUM_BONE_INFLUENCERS = 0;
            _this.BonesPerMesh = 0;
            _this.INSTANCES = false;
            _this.SPECULARTERM = false;
            _this.LOGARITHMICDEPTH = false;
            _this.FRESNELSEPARATE = false;
            _this.BUMPSUPERIMPOSE = false;
            _this.BUMPAFFECTSREFLECTION = false;
            _this.rebuild();
            return _this;
        }
        return WaterMaterialDefines;
    }(BABYLON.MaterialDefines));
    var WaterMaterial = (function(_super) {
        __extends(WaterMaterial, _super);
        /**
         * Constructor
         */
        function WaterMaterial(name, scene, renderTargetSize) {
            if (renderTargetSize === void 0) {
                renderTargetSize = new BABYLON.Vector2(512, 512);
            }
            var _this = _super.call(this, name, scene) || this;
            _this.renderTargetSize = renderTargetSize;
            _this.diffuseColor = new BABYLON.Color3(1, 1, 1);
            _this.specularColor = new BABYLON.Color3(0, 0, 0);
            _this.specularPower = 64;
            _this.disableLighting = false;
            _this.maxSimultaneousLights = 4;
            /**
             * @param {number}: Represents the wind force
             */
            _this.windForce = 6;
            /**
             * @param {Vector2}: The direction of the wind in the plane (X, Z)
             */
            _this.windDirection = new BABYLON.Vector2(0, 1);
            /**
             * @param {number}: Wave height, represents the height of the waves
             */
            _this.waveHeight = 0.4;
            /**
             * @param {number}: Bump height, represents the bump height related to the bump map
             */
            _this.bumpHeight = 0.4;
            /**
             * @param {boolean}: Add a smaller moving bump to less steady waves.
             */
            _this.bumpSuperimpose = false;
            /**
             * @param {boolean}: Color refraction and reflection differently with .waterColor2 and .colorBlendFactor2. Non-linear (physically correct) fresnel.
             */
            _this.fresnelSeparate = false;
            /**
             * @param {boolean}: bump Waves modify the reflection.
             */
            _this.bumpAffectsReflection = false;
            /**
             * @param {number}: The water color blended with the refraction (near)
             */
            _this.waterColor = new BABYLON.Color3(0.1, 0.1, 0.6);
            /**
             * @param {number}: The blend factor related to the water color
             */
            _this.colorBlendFactor = 0.2;
            /**
             * @param {number}: The water color blended with the reflection (far)
             */
            _this.waterColor2 = new BABYLON.Color3(0.1, 0.1, 0.6);
            /**
             * @param {number}: The blend factor related to the water color (reflection, far)
             */
            _this.colorBlendFactor2 = 0.2;
            /**
             * @param {number}: Represents the maximum length of a wave
             */
            _this.waveLength = 0.1;
            /**
             * @param {number}: Defines the waves speed
             */
            _this.waveSpeed = 1.0;
            /*
             * Private members
             */
            _this._mesh = null;
            _this._reflectionTransform = BABYLON.Matrix.Zero();
            _this._lastTime = 0;
            _this._defines = new WaterMaterialDefines();
            _this._cachedDefines = new WaterMaterialDefines();
            // Create render targets
            _this._createRenderTargets(scene, renderTargetSize);
            return _this;
        }
        Object.defineProperty(WaterMaterial.prototype, "useLogarithmicDepth", {
            get: function() {
                return this._useLogarithmicDepth;
            },
            set: function(value) {
                this._useLogarithmicDepth = value && this.getScene().getEngine().getCaps().fragmentDepthSupported;
            },
            enumerable: true,
            configurable: true
        });
        Object.defineProperty(WaterMaterial.prototype, "refractionTexture", {
            // Get / Set
            get: function() {
                return this._refractionRTT;
            },
            enumerable: true,
            configurable: true
        });
        Object.defineProperty(WaterMaterial.prototype, "reflectionTexture", {
            get: function() {
                return this._reflectionRTT;
            },
            enumerable: true,
            configurable: true
        });
        // Methods
        WaterMaterial.prototype.addToRenderList = function(node) {
            this._refractionRTT.renderList.push(node);
            this._reflectionRTT.renderList.push(node);
        };
        WaterMaterial.prototype.enableRenderTargets = function(enable) {
            var refreshRate = enable ? 1 : 0;
            this._refractionRTT.refreshRate = refreshRate;
            this._reflectionRTT.refreshRate = refreshRate;
        };
        WaterMaterial.prototype.getRenderList = function() {
            return this._refractionRTT.renderList;
        };
        Object.defineProperty(WaterMaterial.prototype, "renderTargetsEnabled", {
            get: function() {
                return !(this._refractionRTT.refreshRate === 0);
            },
            enumerable: true,
            configurable: true
        });
        WaterMaterial.prototype.needAlphaBlending = function() {
            return (this.alpha < 1.0);
        };
        WaterMaterial.prototype.needAlphaTesting = function() {
            return false;
        };
        WaterMaterial.prototype.getAlphaTestTexture = function() {
            return null;
        };
        WaterMaterial.prototype._checkCache = function(scene, mesh, useInstances) {
            if (!mesh) {
                return true;
            }
            if (this._defines.INSTANCES !== useInstances) {
                return false;
            }
            if (mesh._materialDefines && mesh._materialDefines.isEqual(this._defines)) {
                return true;
            }
            return false;
        };
        WaterMaterial.prototype.isReady = function(mesh, useInstances) {
            if (this.checkReadyOnlyOnce) {
                if (this._wasPreviouslyReady) {
                    return true;
                }
            }
            var scene = this.getScene();
            if (!this.checkReadyOnEveryCall) {
                if (this._renderId === scene.getRenderId()) {
                    if (this._checkCache(scene, mesh, useInstances)) {
                        return true;
                    }
                }
            }
            var engine = scene.getEngine();
            var needNormals = false;
            var needUVs = false;
            this._defines.reset();
            // Textures
            if (scene.texturesEnabled) {
                if (this.bumpTexture && BABYLON.StandardMaterial.BumpTextureEnabled) {
                    if (!this.bumpTexture.isReady()) {
                        return false;
                    } else {
                        needUVs = true;
                        this._defines.BUMP = true;
                    }
                }
                if (BABYLON.StandardMaterial.ReflectionTextureEnabled) {
                    this._defines.REFLECTION = true;
                }
            }
            // Effect
            if (scene.clipPlane) {
                this._defines.CLIPPLANE = true;
            }
            if (engine.getAlphaTesting()) {
                this._defines.ALPHATEST = true;
            }
            // Point size
            if (this.pointsCloud || scene.forcePointsCloud) {
                this._defines.POINTSIZE = true;
            }
            if (this.useLogarithmicDepth) {
                this._defines.LOGARITHMICDEPTH = true;
            }
            if (this.fresnelSeparate) {
                this._defines.FRESNELSEPARATE = true;
            }
            if (this.bumpSuperimpose) {
                this._defines.BUMPSUPERIMPOSE = true;
            }
            if (this.bumpAffectsReflection) {
                this._defines.BUMPAFFECTSREFLECTION = true;
            }
            // Fog
            if (scene.fogEnabled && mesh && mesh.applyFog && scene.fogMode !== BABYLON.Scene.FOGMODE_NONE && this.fogEnabled) {
                this._defines.FOG = true;
            }
            // Lights
            if (scene.lightsEnabled && !this.disableLighting) {
                needNormals = BABYLON.MaterialHelper.PrepareDefinesForLights(scene, mesh, this._defines, this.maxSimultaneousLights);
            }
            // Attribs
            if (mesh) {
                if (needNormals && mesh.isVerticesDataPresent(BABYLON.VertexBuffer.NormalKind)) {
                    this._defines.NORMAL = true;
                }
                if (needUVs) {
                    if (mesh.isVerticesDataPresent(BABYLON.VertexBuffer.UVKind)) {
                        this._defines.UV1 = true;
                    }
                    if (mesh.isVerticesDataPresent(BABYLON.VertexBuffer.UV2Kind)) {
                        this._defines.UV2 = true;
                    }
                }
                if (mesh.useVertexColors && mesh.isVerticesDataPresent(BABYLON.VertexBuffer.ColorKind)) {
                    this._defines.VERTEXCOLOR = true;
                    if (mesh.hasVertexAlpha) {
                        this._defines.VERTEXALPHA = true;
                    }
                }
                if (mesh.useBones && mesh.computeBonesUsingShaders) {
                    this._defines.NUM_BONE_INFLUENCERS = mesh.numBoneInfluencers;
                    this._defines.BonesPerMesh = (mesh.skeleton.bones.length + 1);
                }
                // Instances
                if (useInstances) {
                    this._defines.INSTANCES = true;
                }
            }
            this._mesh = mesh;
            // Get correct effect
            if (!this._defines.isEqual(this._cachedDefines)) {
                this._defines.cloneTo(this._cachedDefines);
                scene.resetCachedMaterial();
                // Fallbacks
                var fallbacks = new BABYLON.EffectFallbacks();
                if (this._defines.FOG) {
                    fallbacks.addFallback(1, "FOG");
                }
                if (this._defines.LOGARITHMICDEPTH) {
                    fallbacks.addFallback(0, "LOGARITHMICDEPTH");
                }
                BABYLON.MaterialHelper.HandleFallbacksForShadows(this._defines, fallbacks, this.maxSimultaneousLights);
                if (this._defines.NUM_BONE_INFLUENCERS > 0) {
                    fallbacks.addCPUSkinningFallback(0, mesh);
                }
                //Attributes
                var attribs = [BABYLON.VertexBuffer.PositionKind];
                if (this._defines.NORMAL) {
                    attribs.push(BABYLON.VertexBuffer.NormalKind);
                }
                if (this._defines.UV1) {
                    attribs.push(BABYLON.VertexBuffer.UVKind);
                }
                if (this._defines.UV2) {
                    attribs.push(BABYLON.VertexBuffer.UV2Kind);
                }
                if (this._defines.VERTEXCOLOR) {
                    attribs.push(BABYLON.VertexBuffer.ColorKind);
                }
                BABYLON.MaterialHelper.PrepareAttributesForBones(attribs, mesh, this._defines, fallbacks);
                BABYLON.MaterialHelper.PrepareAttributesForInstances(attribs, this._defines);
                // Legacy browser patch
                var shaderName = "water";
                var join = this._defines.toString();
                var uniforms = ["world", "view", "viewProjection", "vEyePosition", "vLightsType", "vDiffuseColor", "vSpecularColor",
                    "vFogInfos", "vFogColor", "pointSize",
                    "vNormalInfos",
                    "mBones",
                    "vClipPlane", "normalMatrix",
                    "logarithmicDepthConstant",
                    // Water
                    "worldReflectionViewProjection", "windDirection", "waveLength", "time", "windForce",
                    "cameraPosition", "bumpHeight", "waveHeight", "waterColor", "waterColor2", "colorBlendFactor", "colorBlendFactor2", "waveSpeed"
                ];
                var samplers = ["normalSampler",
                    // Water
                    "refractionSampler", "reflectionSampler"
                ];
                BABYLON.MaterialHelper.PrepareUniformsAndSamplersList(uniforms, samplers, this._defines, this.maxSimultaneousLights);
                this._effect = scene.getEngine().createEffect(shaderName, attribs, uniforms, samplers, join, fallbacks, this.onCompiled, this.onError, {
                    maxSimultaneousLights: this.maxSimultaneousLights
                });
            }
            if (!this._effect.isReady()) {
                return false;
            }
            this._renderId = scene.getRenderId();
            this._wasPreviouslyReady = true;
            if (mesh) {
                if (!mesh._materialDefines) {
                    mesh._materialDefines = new WaterMaterialDefines();
                }
                this._defines.cloneTo(mesh._materialDefines);
            }
            return true;
        };
        WaterMaterial.prototype.bindOnlyWorldMatrix = function(world) {
            this._effect.setMatrix("world", world);
        };
        WaterMaterial.prototype.bind = function(world, mesh) {
            var scene = this.getScene();
            // Matrices
            this.bindOnlyWorldMatrix(world);
            this._effect.setMatrix("viewProjection", scene.getTransformMatrix());
            // Bones
            BABYLON.MaterialHelper.BindBonesParameters(mesh, this._effect);
            if (scene.getCachedMaterial() !== this) {
                // Textures
                if (this.bumpTexture && BABYLON.StandardMaterial.BumpTextureEnabled) {
                    this._effect.setTexture("normalSampler", this.bumpTexture);
                    this._effect.setFloat2("vNormalInfos", this.bumpTexture.coordinatesIndex, this.bumpTexture.level);
                    this._effect.setMatrix("normalMatrix", this.bumpTexture.getTextureMatrix());
                }
                // Clip plane
                BABYLON.MaterialHelper.BindClipPlane(this._effect, scene);
                // Point size
                if (this.pointsCloud) {
                    this._effect.setFloat("pointSize", this.pointSize);
                }
                this._effect.setVector3("vEyePosition", scene._mirroredCameraPosition ? scene._mirroredCameraPosition : scene.activeCamera.position);
            }
            this._effect.setColor4("vDiffuseColor", this.diffuseColor, this.alpha * mesh.visibility);
            if (this._defines.SPECULARTERM) {
                this._effect.setColor4("vSpecularColor", this.specularColor, this.specularPower);
            }
            if (scene.lightsEnabled && !this.disableLighting) {
                BABYLON.MaterialHelper.BindLights(scene, mesh, this._effect, this._defines, this.maxSimultaneousLights);
            }
            // View
            if (scene.fogEnabled && mesh.applyFog && scene.fogMode !== BABYLON.Scene.FOGMODE_NONE) {
                this._effect.setMatrix("view", scene.getViewMatrix());
            }
            // Fog
            BABYLON.MaterialHelper.BindFogParameters(scene, mesh, this._effect);
            // Log. depth
            BABYLON.MaterialHelper.BindLogDepth(this._defines, this._effect, scene);
            // Water
            if (BABYLON.StandardMaterial.ReflectionTextureEnabled) {
                this._effect.setTexture("refractionSampler", this._refractionRTT);
                this._effect.setTexture("reflectionSampler", this._reflectionRTT);
            }
            var wrvp = this._mesh.getWorldMatrix().multiply(this._reflectionTransform).multiply(scene.getProjectionMatrix());
            this._lastTime += scene.getEngine().getDeltaTime();
            this._effect.setMatrix("worldReflectionViewProjection", wrvp);
            this._effect.setVector2("windDirection", this.windDirection);
            this._effect.setFloat("waveLength", this.waveLength);
            this._effect.setFloat("time", this._lastTime / 100000);
            this._effect.setFloat("windForce", this.windForce);
            this._effect.setFloat("waveHeight", this.waveHeight);
            this._effect.setFloat("bumpHeight", this.bumpHeight);
            this._effect.setColor4("waterColor", this.waterColor, 1.0);
            this._effect.setFloat("colorBlendFactor", this.colorBlendFactor);
            this._effect.setColor4("waterColor2", this.waterColor2, 1.0);
            this._effect.setFloat("colorBlendFactor2", this.colorBlendFactor2);
            this._effect.setFloat("waveSpeed", this.waveSpeed);
            _super.prototype.bind.call(this, world, mesh);
        };
        WaterMaterial.prototype._createRenderTargets = function(scene, renderTargetSize) {
            var _this = this;
            // Render targets
            this._refractionRTT = new BABYLON.RenderTargetTexture(name + "_refraction", {
                width: renderTargetSize.x,
                height: renderTargetSize.y
            }, scene, false, true);
            this._refractionRTT.wrapU = BABYLON.Texture.MIRROR_ADDRESSMODE;
            this._refractionRTT.wrapV = BABYLON.Texture.MIRROR_ADDRESSMODE;
            this._reflectionRTT = new BABYLON.RenderTargetTexture(name + "_reflection", {
                width: renderTargetSize.x,
                height: renderTargetSize.y
            }, scene, false, true);
            this._reflectionRTT.wrapU = BABYLON.Texture.MIRROR_ADDRESSMODE;
            this._reflectionRTT.wrapV = BABYLON.Texture.MIRROR_ADDRESSMODE;
            scene.customRenderTargets.push(this._refractionRTT);
            scene.customRenderTargets.push(this._reflectionRTT);
            var isVisible;
            var clipPlane = null;
            var savedViewMatrix;
            var mirrorMatrix = BABYLON.Matrix.Zero();
            this._refractionRTT.onBeforeRender = function() {
                if (_this._mesh) {
                    isVisible = _this._mesh.isVisible;
                    _this._mesh.isVisible = false;
                }
                // Clip plane
                clipPlane = scene.clipPlane;
                var positiony = _this._mesh ? _this._mesh.position.y : 0.0;
                scene.clipPlane = BABYLON.Plane.FromPositionAndNormal(new BABYLON.Vector3(0, positiony + 0.05, 0), new BABYLON.Vector3(0, 1, 0));
            };
            this._refractionRTT.onAfterRender = function() {
                if (_this._mesh) {
                    _this._mesh.isVisible = isVisible;
                }
                // Clip plane
                scene.clipPlane = clipPlane;
            };
            this._reflectionRTT.onBeforeRender = function() {
                if (_this._mesh) {
                    isVisible = _this._mesh.isVisible;
                    _this._mesh.isVisible = false;
                }
                // Clip plane
                clipPlane = scene.clipPlane;
                var positiony = _this._mesh ? _this._mesh.position.y : 0.0;
                scene.clipPlane = BABYLON.Plane.FromPositionAndNormal(new BABYLON.Vector3(0, positiony - 0.05, 0), new BABYLON.Vector3(0, -1, 0));
                // Transform
                BABYLON.Matrix.ReflectionToRef(scene.clipPlane, mirrorMatrix);
                savedViewMatrix = scene.getViewMatrix();
                mirrorMatrix.multiplyToRef(savedViewMatrix, _this._reflectionTransform);
                scene.setTransformMatrix(_this._reflectionTransform, scene.getProjectionMatrix());
                scene.getEngine().cullBackFaces = false;
                scene._mirroredCameraPosition = BABYLON.Vector3.TransformCoordinates(scene.activeCamera.position, mirrorMatrix);
            };
            this._reflectionRTT.onAfterRender = function() {
                if (_this._mesh) {
                    _this._mesh.isVisible = isVisible;
                }
                // Clip plane
                scene.clipPlane = clipPlane;
                // Transform
                scene.setTransformMatrix(savedViewMatrix, scene.getProjectionMatrix());
                scene.getEngine().cullBackFaces = true;
                scene._mirroredCameraPosition = null;
            };
        };
        WaterMaterial.prototype.getAnimatables = function() {
            var results = [];
            if (this.bumpTexture && this.bumpTexture.animations && this.bumpTexture.animations.length > 0) {
                results.push(this.bumpTexture);
            }
            if (this._reflectionRTT && this._reflectionRTT.animations && this._reflectionRTT.animations.length > 0) {
                results.push(this._reflectionRTT);
            }
            if (this._refractionRTT && this._refractionRTT.animations && this._refractionRTT.animations.length > 0) {
                results.push(this._refractionRTT);
            }
            return results;
        };
        WaterMaterial.prototype.dispose = function(forceDisposeEffect) {
            if (this.bumpTexture) {
                this.bumpTexture.dispose();
            }
            var index = this.getScene().customRenderTargets.indexOf(this._refractionRTT);
            if (index != -1) {
                this.getScene().customRenderTargets.splice(index, 1);
            }
            index = -1;
            index = this.getScene().customRenderTargets.indexOf(this._reflectionRTT);
            if (index != -1) {
                this.getScene().customRenderTargets.splice(index, 1);
            }
            if (this._reflectionRTT) {
                this._reflectionRTT.dispose();
            }
            if (this._refractionRTT) {
                this._refractionRTT.dispose();
            }
            _super.prototype.dispose.call(this, forceDisposeEffect);
        };
        WaterMaterial.prototype.clone = function(name) {
            var _this = this;
            return BABYLON.SerializationHelper.Clone(function() {
                return new WaterMaterial(name, _this.getScene());
            }, this);
        };
        WaterMaterial.prototype.serialize = function() {
            var serializationObject = BABYLON.SerializationHelper.Serialize(this);
            serializationObject.customType = "BABYLON.WaterMaterial";
            serializationObject.reflectionTexture.isRenderTarget = true;
            serializationObject.refractionTexture.isRenderTarget = true;
            return serializationObject;
        };
        // Statics
        WaterMaterial.Parse = function(source, scene, rootUrl) {
            return BABYLON.SerializationHelper.Parse(function() {
                return new WaterMaterial(source.name, scene);
            }, source, scene, rootUrl);
        };
        WaterMaterial.CreateDefaultMesh = function(name, scene) {
            var mesh = BABYLON.Mesh.CreateGround(name, 512, 512, 32, scene, false);
            return mesh;
        };
        return WaterMaterial;
    }(BABYLON.Material));
    /*
    __decorate([
        BABYLON.serializeAsTexture()
    ], WaterMaterial.prototype, "bumpTexture", void 0);
    __decorate([
        BABYLON.serializeAsColor3()
    ], WaterMaterial.prototype, "diffuseColor", void 0);
    __decorate([
        BABYLON.serializeAsColor3()
    ], WaterMaterial.prototype, "specularColor", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "specularPower", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "disableLighting", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "maxSimultaneousLights", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "windForce", void 0);
    __decorate([
        BABYLON.serializeAsVector2()
    ], WaterMaterial.prototype, "windDirection", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "waveHeight", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "bumpHeight", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "bumpSuperimpose", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "fresnelSeparate", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "bumpAffectsReflection", void 0);
    __decorate([
        BABYLON.serializeAsColor3()
    ], WaterMaterial.prototype, "waterColor", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "colorBlendFactor", void 0);
    __decorate([
        BABYLON.serializeAsColor3()
    ], WaterMaterial.prototype, "waterColor2", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "colorBlendFactor2", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "waveLength", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "waveSpeed", void 0);
    __decorate([
        BABYLON.serialize()
    ], WaterMaterial.prototype, "useLogarithmicDepth", null);
    */
    BABYLON.WaterMaterial = WaterMaterial;
})(BABYLON || (BABYLON = {}));