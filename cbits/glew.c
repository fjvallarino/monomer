#include <GL/glew.h>
#include <stdio.h>

void initGlew() {
  glewExperimental = GL_TRUE;
  GLenum err = glewInit();

  if(err != GLEW_OK) {
    fprintf(stderr, "Could not init GLEW: %s\n", glewGetErrorString(err));
    printf("\n");
  }
  else {
    glGetError();
  }
}
