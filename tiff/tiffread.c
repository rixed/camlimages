/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            François Pessaux, projet Cristal, INRIA Rocquencourt     */
/*            Pierre Weis, projet Cristal, INRIA Rocquencourt          */
/*            Jun Furuse, projet Cristal, INRIA Rocquencourt           */
/*                                                                     */
/*  Copyright 1999,2000                                                */
/*  Institut National de Recherche en Informatique et en Automatique.  */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/
#include <string.h>
#include <config.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#if HAVE_TIFF

/* These are defined in caml/config.h */
/*#define int16 int16tiff
#define uint16 uint16tiff
#define int32 int32tiff
#define uint32 uint32tiff*/

#include <tiffio.h>

/*#undef int16
#undef uint16
#undef int32
#undef uint32*/

extern value *imglib_error;

value open_tiff_file_for_read( name )
     value name;
{
  CAMLparam1(name);
  CAMLlocal1(res);
  CAMLlocalN(r,5);

  char *filename; 
  TIFF* tif;
  
  filename = String_val( name );
  
  tif = TIFFOpen(filename, "r");
  if (tif) {
    uint32 imagelength;
    uint32 imagewidth;
    uint16 imagesample;
    uint16 imagebits;
    tdata_t buf;
    int i;
    uint16 runit;
    float xres, yres;
    uint16 photometric;

    TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &imagelength);
    TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &imagewidth);
    TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &imagebits);
    TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &imagesample);
    TIFFGetField(tif, TIFFTAG_RESOLUTIONUNIT, &runit);
    TIFFGetField(tif, TIFFTAG_XRESOLUTION, &xres);
    TIFFGetField(tif, TIFFTAG_YRESOLUTION, &yres);
    TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &photometric);

    if( imagesample == 3 && photometric == PHOTOMETRIC_RGB ){
      if( imagebits != 8 ){
	failwith("Sorry, tiff rgb file must be 24bit-color");
      }
      r[3] = Val_int(0); /* RGB */
    } else if( imagesample == 4 && photometric == PHOTOMETRIC_SEPARATED ){
      if( imagebits != 8 ){
	failwith("Sorry, tiff cmyk file must be 32bit-color");
      }
      r[3] = Val_int(1); /* CMYK */
    } else if( imagesample == 1 && imagebits == 1 ) { /* BW */
      r[3] = Val_int (photometric == PHOTOMETRIC_MINISWHITE ? 2 : 3);
    } else {
      fprintf(stderr, "photometric=%d, imagesample=%d, imagebits=%d\n", photometric, imagesample, imagebits);
      failwith("Sorry, unsupported tiff format");
    }

    buf = _TIFFmalloc(TIFFScanlineSize(tif));

    r[0] = Val_int(imagewidth);
    r[1] = Val_int(imagelength);
    if ( runit == RESUNIT_INCH &&
	 xres == yres ){
      r[2] = copy_double( xres );
    } else {
      r[2] = copy_double ( -1.0 );
    }
    /* r[3] is defined above */ 
    r[4] = (value)tif;
    res = alloc_tuple(5);
    for(i=0; i<5; i++) Field(res, i) = r[i];

    CAMLreturn(res);
  } else {
    failwith("failed to open tiff file");
  }
}

value read_tiff_scanline( tiffh, buf, row )
     value tiffh;
     value buf;
     value row;
{
  CAMLparam3(tiffh,buf,row);
  TIFFReadScanline((TIFF*)tiffh, String_val(buf), Int_val(row), 0);
  CAMLreturn(Val_unit);
}

value close_tiff_file( tiffh )
     value tiffh;
{
  CAMLparam1(tiffh);
  TIFFClose((TIFF*)tiffh);
  CAMLreturn(Val_unit);
}

/* Not supported anymore */
#ifdef OLDCODE
value read_tiff_file( name )
     value name;
{
  CAMLparam1(name);
  CAMLlocal1(res);
  char *filename; 
  TIFF* tif;
  
  filename = String_val( name );
  
  tif = TIFFOpen(filename, "r");
  if (tif) {
    uint32 imagelength;
    uint32 imagewidth;
    uint16 imagesample;
    uint16 imagebits;
    tdata_t buf;
    char *buffer;
    uint32 row;
    int i;
    int runit;
    float xres, yres;

    TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &imagelength);
    TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &imagewidth);
    TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &imagebits);
    TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &imagesample);
    TIFFGetField(tif, TIFFTAG_RESOLUTIONUNIT, &runit);
    TIFFGetField(tif, TIFFTAG_XRESOLUTION, &xres);
    TIFFGetField(tif, TIFFTAG_YRESOLUTION, &yres);
    if( imagesample != 3 || imagebits != 8 ) {
      failwith("tiff file is not in the 24 bit RGB format");
    }
    buf = _TIFFmalloc(TIFFScanlineSize(tif));
    {
      CAMLlocalN(r,4);
      r[0] = Val_int(imagewidth);
      r[1] = Val_int(imagelength);
      if ( runit == RESUNIT_INCH && xres == yres ){
  	r[2] = copy_double( xres );
      } else {
  	r[2] = copy_double ( -1.0 );
      }
      r[3] = alloc_string( imagewidth * imagelength * 3 );
      buffer = String_val(r[3]);
      res = alloc_tuple(4);
      for(i=0; i<4; i++) Field(res, i) = r[i];
    }
    for (row = 0; row < imagelength; row++) {
	TIFFReadScanline(tif, buf, row, 0);
	/* hope it is RGBRGB */
	memcpy( (buffer + imagewidth * 3 * row),
		buf, TIFFScanlineSize(tif) );
    }
    _TIFFfree(buf);
    TIFFClose(tif);
    CAMLreturn(res);
  } else {
    failwith("failed to open tiff file");
  }
}
#endif /* OLDCODE */

#else

#define tiff_not_supported() \
  failwith( "tiff is not supported" ); \
  return(Val_unit)

#define nodef(f) value f(){tiff_not_supported();}

nodef(read_tiff_file)
nodef(open_tiff_file_for_read)
nodef(read_tiff_scanline)
nodef(close_tiff_file)

#endif /* HAVE_TIFF */
