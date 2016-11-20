#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/*#include <GL/glew.h>*/
#ifdef __APPLE__
#  include <GLUT/glut.h>
#else
#  include <GL/glut.h>
#endif

char *basename(char*);
void render(void);
void keyin(unsigned char, int, int);
void myglinit(void);
unsigned char red(int, int);
unsigned char green(int, int);
unsigned char blue(int, int);

unsigned char *easel;
int W, H;

int main(int argc, char **argv) {
    // width and height of the window
    W = 960;
    H = 960;

    // create a char buffer, 3 bytes per pixel of the window
    easel = calloc(W * H * 3, sizeof(unsigned char));

    // initialize the glut system and create a window
    glutInitWindowSize(W, H);
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
    glutCreateWindow(basename(argv[0]));

    // initialize some OpenGL state, some might be redundant
    myglinit();

    // set callback functions.
    glutDisplayFunc(&render);
    glutKeyboardFunc(&keyin);

    // start the main glut loop, no code runs after this
    glutMainLoop();

    free(easel);

    return 0;
}

char *basename(char *s)
{
    char *cur;
    if (s != NULL) {
        cur = s;
        while (*cur != '\0') {
            if (*cur == '/') s = cur + 1;
            cur++;
        }
    }
    return s;
}

void setpixel(unsigned char *buf, int x, int y, int r, int g, int b)
{
    buf[(y * W + x) * 3 + 0] = r;
    buf[(y * W + x) * 3 + 1] = g;
    buf[(y * W + x) * 3 + 2] = b;
}

// main draw function, gets called over and over, as fast as possible
void render(void) {
    int x, y;

    printf("in render\n");

    // set first half of buffer to red
    for (x = 0; x < W; x++) {
        for (y = 0; y < H; y++) {
            // set pixel at coord x,y to 'red'
            setpixel(easel, x, y, red(x, y), green(x, y), blue(x, y));
        }
    }

    // drawpixels draws the rgb data stored in 'easel' to the screen
    glDrawPixels(W, H, GL_RGB, GL_UNSIGNED_BYTE, easel);

    // in double buffer mode so we swap to avoid a flicker
    glutSwapBuffers();

    // instruct event system to call 'render' again
    // glutPostRedisplay();
}

void keyin(unsigned char k, int x, int y) {
    printf("in keyin\n");

    switch (k) {
        case 27:
            // escape key
            exit(0);
            break;
        case GLUT_KEY_UP:
            // up arrow
            break;
        case 'a':
            // 'a' key
            break;
    }
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

unsigned char red(int x, int y)
{
    double a = 0, b = 0, c, d, n = 0;
    while((c = a * a) + (d = b * b) < 4 && n++ < 880) {
        b = 2 * a * b + y * 8e-9 - .645411;
        a = c - d + x * 8e-9 + .356888;
    }
    return (unsigned char) (255 * pow((n - 80)/800, 3.));
}

unsigned char green(int x, int y)
{
    double a = 0, b = 0, c, d, n = 0;
    while((c = a * a) + (d = b * b) < 4 && n++ < 880) {
        b = 2 * a * b + y * 8e-9 - .645411;
        a = c - d + x * 8e-9 + .356888;
    }
    return (unsigned char) (255 * pow((n - 80)/800, .7));
}

unsigned char blue(int x, int y)
{
    double a = 0, b = 0, c, d, n = 0;
    while((c = a * a) + (d = b * b) < 4 && n++ < 880) {
        b = 2 * a * b + y * 8e-9 - .645411;
        a = c - d + x * 8e-9 + .356888;
    }
    return (unsigned char) (255 * pow((n - 80)/800, .5));
}

