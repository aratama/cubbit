#ifdef GL_ES
precision highp float;
#endif

// Lights
varying vec3 vPositionW;
varying vec3 vNormalW;
varying vec2 vUV;


// Refs
uniform float ToonThresholds[4];
uniform float ToonBrightnessLevels[5];
uniform vec3 vLightPosition;
uniform vec3 vLightColor;

uniform sampler2D textureSampler;

void main(void) {
	gl_FragColor = vec4(140.0/ 255.0, 109.0 / 255.0, 102.0 / 255.0, 1.);
}