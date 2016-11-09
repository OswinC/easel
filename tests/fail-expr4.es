function pix my_func1(pix p, int i) {
	p = p + i;
	return p;
}

function pix my_func2(pix p2, int i2) {
	p2 = p2 - i2;
	return p2;
}

my_func1 > my_func2; /* functions cannot be compared */
