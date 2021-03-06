#ifndef TK_CTYPE_H
#define TK_CTYPE_H 1

#include <lt/lt.h>

#define NUMH (1 << 0)
#define NUMB (1 << 1)
#define NUMR (NUMH | NUMB)

#define IDNH (1 << 2)
#define IDNB (1 << 3)
#define IDNT (IDNH | IDNB)

#define IDNR (IDNT | NUMB)
#define IBNR (IDNB | NUMR)

#define WHSP (1 << 4)
#define OPER (1 << 5)
#define PARN (1 << 6)
#define PUNC (1 << 7)

static
const u8 ctype_table[256] = {
	/*00*/ 0, 0, 0, 0, /*04*/ 0, 0, 0, 0, /*08*/ 0, WHSP, WHSP, WHSP, /*0C*/ WHSP, WHSP, 0, 0,
	/*10*/ 0, 0, 0, 0, /*14*/ 0, 0, 0, 0, /*18*/ 0, 0, 0, 0, /*1C*/ 0, 0, 0, 0,
	/*20*/ WHSP, OPER|PUNC, 0, 0, /*24*/ 0, OPER, OPER, 0, /*28*/ PARN|PUNC, PARN|PUNC, OPER, OPER, /*2C*/ 0, OPER, PUNC|NUMB, OPER,
	/*30*/ IBNR, IBNR, IBNR, IBNR, /*34*/ IBNR, IBNR, IBNR, IBNR, /*38*/ IBNR, IBNR, PUNC, PUNC, /*3C*/ OPER, OPER, OPER, OPER|PUNC,
	/*40*/ 0, IDNR, IDNR, IDNR, /*44*/ IDNR, IDNR, IDNR, IDNR, /*48*/ IDNR, IDNR, IDNR, IDNR, /*4C*/ IDNR, IDNR, IDNR, IDNR,
	/*50*/ IDNR, IDNR, IDNR, IDNR, /*54*/ IDNR, IDNR, IDNR, IDNR, /*58*/ IDNR, IDNR, IDNR, PARN|PUNC, /*5C*/ 0, PARN|PUNC, OPER, IDNR,
	/*60*/ 0, IDNR, IDNR, IDNR, /*64*/ IDNR, IDNR, IDNR, IDNR, /*68*/ IDNR, IDNR, IDNR, IDNR, /*6C*/ IDNR, IDNR, IDNR, IDNR,
	/*70*/ IDNR, IDNR, IDNR, IDNR, /*74*/ IDNR, IDNR, IDNR, IDNR, /*78*/ IDNR, IDNR, IDNR, PARN|PUNC, /*7C*/ OPER, PARN|PUNC, OPER, 0,

	/*80*/ IDNR, IDNR, IDNR, IDNR, /*84*/ IDNR, IDNR, IDNR, IDNR, /*88*/ IDNR, IDNR, IDNR, IDNR, /*8C*/ IDNR, IDNR, IDNR, IDNR,
	/*90*/ IDNR, IDNR, IDNR, IDNR, /*94*/ IDNR, IDNR, IDNR, IDNR, /*98*/ IDNR, IDNR, IDNR, IDNR, /*9C*/ IDNR, IDNR, IDNR, IDNR,
	/*A0*/ IDNR, IDNR, IDNR, IDNR, /*A4*/ IDNR, IDNR, IDNR, IDNR, /*A8*/ IDNR, IDNR, IDNR, IDNR, /*AC*/ IDNR, IDNR, IDNR, IDNR,
	/*B0*/ IDNR, IDNR, IDNR, IDNR, /*B4*/ IDNR, IDNR, IDNR, IDNR, /*B8*/ IDNR, IDNR, IDNR, IDNR, /*BC*/ IDNR, IDNR, IDNR, IDNR,
	/*C0*/ IDNR, IDNR, IDNR, IDNR, /*C4*/ IDNR, IDNR, IDNR, IDNR, /*C8*/ IDNR, IDNR, IDNR, IDNR, /*CC*/ IDNR, IDNR, IDNR, IDNR,
	/*D0*/ IDNR, IDNR, IDNR, IDNR, /*D4*/ IDNR, IDNR, IDNR, IDNR, /*D8*/ IDNR, IDNR, IDNR, IDNR, /*DC*/ IDNR, IDNR, IDNR, IDNR,
	/*E0*/ IDNR, IDNR, IDNR, IDNR, /*E4*/ IDNR, IDNR, IDNR, IDNR, /*E8*/ IDNR, IDNR, IDNR, IDNR, /*EC*/ IDNR, IDNR, IDNR, IDNR,
	/*F0*/ IDNR, IDNR, IDNR, IDNR, /*F4*/ IDNR, IDNR, IDNR, IDNR, /*F8*/ IDNR, IDNR, IDNR, IDNR, /*FC*/ IDNR, IDNR, IDNR, IDNR,
};

static
const u8 digit_val_table[256] = {
	/*00*/ 0, 0, 0, 0, /*04*/ 0, 0, 0, 0, /*08*/ 0, 0, 0, 0, /*0C*/ 0, 0, 0, 0,
	/*10*/ 0, 0, 0, 0, /*14*/ 0, 0, 0, 0, /*18*/ 0, 0, 0, 0, /*1C*/ 0, 0, 0, 0,
	/*20*/ 0, 0, 0, 0, /*24*/ 0, 0, 0, 0, /*28*/ 0, 0, 0, 0, /*2C*/ 0, 0, 0, 0,
	/*30*/ 0, 1, 2, 3, /*34*/ 4, 5, 6, 7, /*38*/ 8, 9, 0, 0, /*3C*/ 0, 0, 0, 0,
	/*40*/ 0, 10, 11, 12, /*44*/ 13, 14, 15, 0, /*48*/ 0, 0, 0, 0, /*4C*/ 0, 0, 0, 0,
	/*50*/ 0, 0, 0, 0, /*54*/ 0, 0, 0, 0, /*58*/ 0, 0, 0, 0, /*5C*/ 0, 0, 0, 0,
	/*60*/ 0, 10, 11, 12, /*64*/ 13, 14, 15, 0, /*68*/ 0, 0, 0, 0, /*6C*/ 0, 0, 0, 0,
	/*70*/ 0, 0, 0, 0, /*74*/ 0, 0, 0, 0, /*78*/ 0, 0, 0, 0, /*7C*/ 0, 0, 0, 0,

	/*80*/ 0, 0, 0, 0, /*84*/ 0, 0, 0, 0, /*88*/ 0, 0, 0, 0, /*8C*/ 0, 0, 0, 0,
	/*90*/ 0, 0, 0, 0, /*94*/ 0, 0, 0, 0, /*98*/ 0, 0, 0, 0, /*9C*/ 0, 0, 0, 0,
	/*A0*/ 0, 0, 0, 0, /*A4*/ 0, 0, 0, 0, /*A8*/ 0, 0, 0, 0, /*AC*/ 0, 0, 0, 0,
	/*B0*/ 0, 0, 0, 0, /*B4*/ 0, 0, 0, 0, /*B8*/ 0, 0, 0, 0, /*BC*/ 0, 0, 0, 0,
	/*C0*/ 0, 0, 0, 0, /*C4*/ 0, 0, 0, 0, /*C8*/ 0, 0, 0, 0, /*CC*/ 0, 0, 0, 0,
	/*D0*/ 0, 0, 0, 0, /*D4*/ 0, 0, 0, 0, /*D8*/ 0, 0, 0, 0, /*DC*/ 0, 0, 0, 0,
	/*E0*/ 0, 0, 0, 0, /*E4*/ 0, 0, 0, 0, /*E8*/ 0, 0, 0, 0, /*EC*/ 0, 0, 0, 0,
	/*F0*/ 0, 0, 0, 0, /*F4*/ 0, 0, 0, 0, /*F8*/ 0, 0, 0, 0, /*FC*/ 0, 0, 0, 0,
};


static LT_INLINE
u8 is_ident_head(u8 c) {
	return ctype_table[c] & IDNH;
}

static LT_INLINE
u8 is_ident_body(u8 c) {
	return ctype_table[c] & IDNB;
}

static LT_INLINE
u8 is_numeric_head(u8 c) {
	return ctype_table[c] & NUMH;
}

static LT_INLINE
u8 is_digit(u8 c) {
	return ctype_table[c] & NUMH;
}

static LT_INLINE
u8 is_hex_digit(u8 c) {
	switch (c) {
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
		return 1;
	default:
		return 0;
	}
}

static LT_INLINE
u8 is_oct_digit(u8 c) {
	switch (c) {
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
		return 1;
	default:
		return 0;
	}
}

static LT_INLINE
u8 is_numeric_body(u8 c) {
	return ctype_table[c] & NUMB;
}

static LT_INLINE
u8 is_space(u8 c) {
	return ctype_table[c] & WHSP;
}

static LT_INLINE
u8 is_operator(u8 c) {
	return ctype_table[c] & OPER;
}

static LT_INLINE
u8 is_paren(u8 c) {
	return ctype_table[c] & PARN;
}

static LT_INLINE
u8 is_punc(u8 c) {
	return ctype_table[c] & PUNC;
}

static LT_INLINE
u8 digit_uint_val(u8 c) {
	return digit_val_table[c];
}

#endif
