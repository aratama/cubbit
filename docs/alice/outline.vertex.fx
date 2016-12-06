﻿#ifdef GL_ES
precision highp float;
#endif

// Attributes
attribute vec3 position;
attribute vec3 normal;
attribute vec2 uv;

// Uniforms
uniform mat4 viewProjection;

// Normal
varying vec3 vPositionW;
varying vec3 vNormalW;
varying vec2 vUV;


#if NUM_BONE_INFLUENCERS>0
    uniform mat4 mBones[BonesPerMesh];
    attribute vec4 matricesIndices;
    attribute vec4 matricesWeights;
    #if NUM_BONE_INFLUENCERS>4
        attribute vec4 matricesIndicesExtra;
        attribute vec4 matricesWeightsExtra;
    #endif
#endif

#ifdef INSTANCES
    attribute vec4 world0;
    attribute vec4 world1;
    attribute vec4 world2;
    attribute vec4 world3;
#else
    uniform mat4 world;
#endif

varying vec4 vPosition;

void main(void) {



#ifdef INSTANCES
    mat4 finalWorld=mat4(world0,world1,world2,world3);
#else
    mat4 finalWorld=world;
#endif

#if NUM_BONE_INFLUENCERS>0
    mat4 influence;
    influence=mBones[int(matricesIndices[0])]*matricesWeights[0];
    #if NUM_BONE_INFLUENCERS>1
        influence+=mBones[int(matricesIndices[1])]*matricesWeights[1];
    #endif
    #if NUM_BONE_INFLUENCERS>2
        influence+=mBones[int(matricesIndices[2])]*matricesWeights[2];
    #endif
    #if NUM_BONE_INFLUENCERS>3
        influence+=mBones[int(matricesIndices[3])]*matricesWeights[3];
    #endif
    #if NUM_BONE_INFLUENCERS>4
        influence+=mBones[int(matricesIndicesExtra[0])]*matricesWeightsExtra[0];
    #endif
    #if NUM_BONE_INFLUENCERS>5
        influence+=mBones[int(matricesIndicesExtra[1])]*matricesWeightsExtra[1];
    #endif
    #if NUM_BONE_INFLUENCERS>6
        influence+=mBones[int(matricesIndicesExtra[2])]*matricesWeightsExtra[2];
    #endif
    #if NUM_BONE_INFLUENCERS>7
        influence+=mBones[int(matricesIndicesExtra[3])]*matricesWeightsExtra[3];
    #endif

    finalWorld = finalWorld * influence;
#endif

vec3 norm = normalize(normal);

#ifdef CUBEMAP
    vPosition=finalWorld*vec4(position,1.0);
    gl_Position=viewProjection*finalWorld*vec4(position,1.0);
#else
    vec4 pos = viewProjection * finalWorld * vec4(position + norm, 1.0);
    vPosition = viewProjection * finalWorld * vec4(position + norm * pos.z * -0.002, 1.0);
    gl_Position = vPosition;
#endif

	vPositionW = vec3(world * vec4(position, 1.0));
	vNormalW = normalize(vec3(world * vec4(normal, 0.0)));

	vUV = uv;
}