char InstancedRendererVertexShader[]
{
  R"(#version 430 core

	layout (location = 0) in vec3  position;
  layout (location = 1) in vec3  normal;
  layout (location = 2) in vec2  texCoord;
	layout (location = 3) in vec4  modelMtx1;
	layout (location = 4) in vec4  modelMtx2;
	layout (location = 5) in vec4  modelMtx3;
	layout (location = 6) in vec4  modelMtx4;
	layout (location = 7) in vec4  color;

  out vec4 FragPos;
  out vec3 outNormal;
  out vec2 Coord;
  out vec3 camPosition;
	out vec4 outColor;

  uniform float windIntensity;
  uniform float time;
  uniform float minY;
  uniform float windFreq;
  uniform float collidingDistance;
  uniform vec3 collidingBody;
  uniform float pushbackDist;
  uniform int uniformSwaying;

  layout (std140) uniform camViewProj
  {
    uniform mat4 ViewProj;
    uniform vec3 camPos;
  };

  vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
  vec2 mod289(vec2 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
  vec3 permute(vec3 x) { return mod289(((x*34.0)+1.0)*x); }

  float snoise(vec2 v) {

    // Precompute values for skewed triangular grid
    const vec4 C = vec4(0.211324865405187,
                        // (3.0-sqrt(3.0))/6.0
                        0.366025403784439,
                        // 0.5*(sqrt(3.0)-1.0)
                        -0.577350269189626,
                        // -1.0 + 2.0 * C.x
                        0.024390243902439);
                        // 1.0 / 41.0

    // First corner (x0)
    vec2 i  = floor(v + dot(v, C.yy));
    vec2 x0 = v - i + dot(i, C.xx);

    // Other two corners (x1, x2)
    vec2 i1 = vec2(0.0);
    i1 = (x0.x > x0.y)? vec2(1.0, 0.0):vec2(0.0, 1.0);
    vec2 x1 = x0.xy + C.xx - i1;
    vec2 x2 = x0.xy + C.zz;

    // Do some permutations to avoid
    // truncation effects in permutation
    i = mod289(i);
    vec3 p = permute(
            permute( i.y + vec3(0.0, i1.y, 1.0))
                + i.x + vec3(0.0, i1.x, 1.0 ));

    vec3 m = max(0.5 - vec3(
                        dot(x0,x0),
                        dot(x1,x1),
                        dot(x2,x2)
                        ), 0.0);

    m = m*m ;
    m = m*m ;

    // Gradients:
    //  41 pts uniformly over a line, mapped onto a diamond
    //  The ring size 17*17 = 289 is close to a multiple
    //      of 41 (41*7 = 287)

    vec3 x = 2.0 * fract(p * C.www) - 1.0;
    vec3 h = abs(x) - 0.5;
    vec3 ox = floor(x + 0.5);
    vec3 a0 = x - ox;

    // Normalise gradients implicitly by scaling m
    // Approximation of: m *= inversesqrt(a0*a0 + h*h);
    m *= 1.79284291400159 - 0.85373472095314 * (a0*a0+h*h);

    // Compute final noise value at P
    vec3 g = vec3(0.0);
    g.x  = a0.x  * x0.x  + h.x  * x0.y;
    g.yz = a0.yz * vec2(x1.x,x2.x) + h.yz * vec2(x1.y,x2.y);
    return 130.0 * dot(m, g);
  }


	void main()
	{
		mat4 modelMtx = mat4(modelMtx1, modelMtx2, modelMtx3, modelMtx4);
		modelMtx = transpose(modelMtx);
    vec4 vtx = modelMtx * vec4(position, 1.0f);
    vec3 worldPos = vec3(modelMtx[3][0],modelMtx[3][1], modelMtx[3][2]);

    vec3 collideVec = worldPos - collidingBody;
    float collideVecLen = length(collideVec);
    float distance = max(collidingDistance - collideVecLen, 0);
    collideVec /= collideVecLen;
    
    float vertFactor = (max(position.y-min(minY,0),0.f));
    if (uniformSwaying > 0) vertFactor = 1;

    vtx.x += windIntensity * vertFactor * sin(time*windFreq*snoise(worldPos.xz));
    vtx.z += windIntensity * vertFactor* sin(time*0.5f*windFreq*snoise(worldPos.xz));
	vtx += vec4(collideVec* pushbackDist * (distance/collidingDistance) * vertFactor,0.0f);

		gl_Position = ViewProj * vtx;
		FragPos = vtx;
		outNormal = normal;
		Coord = texCoord;
		camPosition = camPos;
		outColor = color;
	})"
};

char InstancedRendererFragmentShader[]
{
   R"(
    #version 430 core
    
	  layout (location = 0) out vec4  gPosition;
	  layout (location = 1) out vec4  gNormal;
	  layout (location = 2) out vec4  gDiffuse;
	  layout (location = 3) out vec4  gSpecular;
	  layout (location = 4) out vec4  gAmbient;
	  layout (location = 5) out vec4  gObjID;
		
    in vec4 FragPos;
    in vec3 outNormal;
    in vec2 Coord;
	  in vec3 camPosition;
	  in vec4 outColor;

	  uniform int hasTexture;
    uniform int hasAlpha;
	  uniform sampler2D texture_diffuse0;
    uniform sampler2D texture_alpha;
	  uniform float objID;

    void main()
    {
      if (hasAlpha > 0)
      {
        if (texture(texture_alpha, Coord).r <= 0.75f) discard;
      }
		  gPosition = FragPos;
		  gNormal = vec4(outNormal, 0.0f);
		  if (hasTexture >= 1) gDiffuse = outColor * vec4(texture(texture_diffuse0, Coord).xyz, 1.0f);
		  else gDiffuse = vec4(outColor.xyz, 1.0f);
		  gSpecular = vec4(0,0,0,0);
		  gAmbient = vec4(0,0,0,0);
		  gObjID = vec4(objID, 0.f, 0.f, 0.f);
    })"
};
