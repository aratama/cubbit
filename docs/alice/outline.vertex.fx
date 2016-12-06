﻿#ifdef GL_ES
precision highp float;
#endif

// Attributes
attribute vec3 position;
attribute vec3 normal;

// Uniforms
uniform mat4 viewProjection;

#if NUM_BONE_INFLUENCERS>0
    uniform mat4 mBones[BonesPerMesh];
    attribute vec4 matricesIndices;
    attribute vec4 matricesWeights;
    #if NUM_BONE_INFLUENCERS>4
        attribute vec4 matricesIndicesExtra;
        attribute vec4 matricesWeightsExtra;
    #endif
#endif

uniform mat4 world;

varying vec4 vPosition;

void main(void) {

    mat4 finalWorld = world;

#if NUM_BONE_INFLUENCERS>0
    mat4 influence;
    influence = mBones[int(matricesIndices[0])]*matricesWeights[0];
    #if NUM_BONE_INFLUENCERS>1
        influence += mBones[int(matricesIndices[1])]*matricesWeights[1];
    #endif
    #if NUM_BONE_INFLUENCERS>2
        influence += mBones[int(matricesIndices[2])]*matricesWeights[2];
    #endif
    #if NUM_BONE_INFLUENCERS>3
        influence += mBones[int(matricesIndices[3])]*matricesWeights[3];
    #endif
    #if NUM_BONE_INFLUENCERS>4
        influence += mBones[int(matricesIndicesExtra[0])]*matricesWeightsExtra[0];
    #endif
    #if NUM_BONE_INFLUENCERS>5
        influence += mBones[int(matricesIndicesExtra[1])]*matricesWeightsExtra[1];
    #endif
    #if NUM_BONE_INFLUENCERS>6
        influence += mBones[int(matricesIndicesExtra[2])]*matricesWeightsExtra[2];
    #endif
    #if NUM_BONE_INFLUENCERS>7
        influence += mBones[int(matricesIndicesExtra[3])]*matricesWeightsExtra[3];
    #endif

    finalWorld = finalWorld * influence;
#endif

    vec3 norm = normalize(normal);

    vec4 pos = viewProjection * finalWorld * vec4(position + norm, 1.0);
    vPosition = viewProjection * finalWorld * vec4(position + norm * pos.z * -0.002, 1.0);
    gl_Position = vPosition;
}