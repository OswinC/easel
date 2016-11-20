#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
/*#include <GL/glew.h>*/
#ifdef __APPLE__
#  include <GLUT/glut.h>
#else
#  include <GL/glut.h>
#endif

int do_draw(int *canvas, int w, int h, int x, int y);
void render(void);
void myglinit(void);

int *easel;
int W, H;

int draw_default() {
#define DW 960
#define DH 960 
    int c[DW][DH]; 
    int x, y;
    for (x = 0; x < DW; x++) {
        for (y = 0; y < DH; y++) { 
            if (x > DW/2 && y > DH/2)
                c[x][y] = 0xffffff00; // RGBA
            else
                c[x][y] = 0x00000000;
        }
    }
    do_draw((int *) c, DW, DH, 0, 0);
}

int do_draw(int *canvas, int w, int h, int x, int y) {
    char *fake_argv[1];
    int fake_argc = 1;
    fake_argv[0] = strdup("easel");

    easel = canvas;
    W = w;
    H = h;

    // initialize the glut system and create a window
    glutInitWindowSize(W, H);
    glutInit(&fake_argc, fake_argv);
    glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
    glutCreateWindow(fake_argv[0]);

    // initialize some OpenGL state, some might be redundant
    myglinit();

    // set callback functions.
    glutDisplayFunc(&render);

    // start the main glut loop, no code runs after this
    glutMainLoop();

    return 0;
}

void render(void) {
#ifdef _DEBUG
    printf("in render\n");
#endif // _DEBUG

#ifdef _DEBUG_VERB
    int x, y;
    for (y = 0; y < H; y++) {
        for (x = 0; x < W; x++) {
            printf("%d ", easel[(y * W + x)]);
        }
        printf("\n");
    }
#endif // _DEBUG_VERB

    // drawpixels draws the rgb data stored in 'easel' to the screen
    glDrawPixels(W, H, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, easel);

    // in double buffer mode so we swap to avoid a flicker
    glutSwapBuffers();

    // instruct event system to call 'render' again
    // glutPostRedisplay();
}

// set some OpenGL state variables
void myglinit() {
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    glOrtho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);
} 

