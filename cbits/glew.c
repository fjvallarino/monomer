// Based on code from cocreature's https://github.com/cocreature/nanovg-hs

#include <GL/glew.h>
#include <stdio.h>

void initGlew() {
  glewExperimental = GL_TRUE;
  GLenum err = glewInit();

  const GLubyte* renderer = glGetString (GL_RENDERER); // get renderer string
  const GLubyte* version = glGetString (GL_VERSION); // version as a string
  fprintf(stderr, "Renderer: %s\nVersion: %s\n", renderer, version);

  if(err != GLEW_OK) {
    fprintf(stderr, "Could not init GLEW: %s\n", glewGetErrorString(err));
    printf("\n");
  }
  else {
    glGetError();
  }
}
