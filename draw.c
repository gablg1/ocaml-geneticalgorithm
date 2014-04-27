CAMLprim value caml_gr_set_color(value vcolor)
{
		int color = Long_val(vcolor);

        int  r = (color & 0xFF0000) >> 16,
        g = (color & 0x00FF00) >> 8 ,
        b =  color & 0x0000FF;
		queue(qSetColor, r, g, b,0); // 0-255
        return Val_unit;
}
