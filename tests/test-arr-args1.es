function pix foo(pix[30][] canvas)
{
    canvas[3][4] = 56;
    return canvas[3][4];
}

function pix bar() {
    pix l_c[30][20];
    return foo(l_c);
}

printp(bar());
