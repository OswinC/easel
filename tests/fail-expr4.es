function pix pix(pix p, int i) {
	p = p + i;
	return p;
}

function pix pix2(pix p2, int i2) {
	p2 = p2 - i2;
	return p2;
}

pix > pix2; /* functions cannot be compared */
