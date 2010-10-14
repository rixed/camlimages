/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Jun Furuse, projet Cristal, INRIA Rocquencourt           */
/*                                                                     */
/*  Copyright 1999,2000                                                */
/*  Institut National de Recherche en Informatique et en Automatique.  */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/
#include <config.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#if HAVE_FREETYPE 

#include <freetype.h>
#include <ftxcmap.h>

value init_FreeType( unit )
     value unit;
{
   TT_Engine *engine;
   char *palette = "\000\001\002\003\004";

  if( (engine = stat_alloc( sizeof(TT_Engine) )) == NULL ){
    failwith( "Memory over" );
  }
  if( TT_Init_FreeType( engine ) ){
    failwith( "TT_Init_FreeType failed" );
  }
  TT_Set_Raster_Gray_Palette( *engine, palette );
  return (value) engine;
}

value done_FreeType( engine )
     value engine;
{
  TT_Done_FreeType( *(TT_Engine *)engine );
  stat_free( (void *) engine );
  return Val_unit;
}

value open_Face( engine, fontpath )
     value engine;
     value fontpath;
{
  TT_Face *face;
  
  if( (face = stat_alloc( sizeof(TT_Face) )) == NULL ){
    failwith( "Memory over" );
  }
  if( TT_Open_Face( *(TT_Engine *)engine, String_val( fontpath ), face ) ){
    failwith( "Could not open face" );
  }
  return (value) face;
}

value close_Face( face )
     value face;
{
  TT_Close_Face( *(TT_Face *) face );
  return Val_unit;
}

value flush_Face( face )
     value face;
{
  TT_Flush_Face( *(TT_Face *) face);
  return Val_unit;
}

value new_Instance( face )
     value face;
{
  TT_Instance *instance;
  
  if( (instance = stat_alloc( sizeof(TT_Instance) )) == NULL ){
    failwith( "Memory over" );
  }
  if( TT_New_Instance( *(TT_Face *)face, instance ) ){
    failwith( "Could not initialize instance" );
  }
  return (value) instance;
}
     
value set_Instance_Resolutions( instance, x, y )
     value instance, x, y;
{
  TT_Set_Instance_Resolutions( *(TT_Instance *)instance,
			       Int_val(x),
			       Int_val(y) ); 
  return Val_unit;
}

value set_Instance_CharSizes( instance, w, h )
     value instance;
     value w; /* 26.6 == 1 = 1/64 pt */
     value h;
{
  TT_Set_Instance_CharSizes( *(TT_Instance *)instance,
			    Int_val(w),
			    Int_val(h) ); 
  return Val_unit;
}

value set_Instance_PixelSizes( instance, pixw, pixh, pointsize )
     value instance, pixw, pixh, pointsize;
{
  CAMLparam4(instance, pixw, pixh, pointsize);
  TT_Set_Instance_PixelSizes( *(TT_Instance *)instance,
		      Int_val(pixw),
		      Int_val(pixh),
		      Int_val(pointsize));
  CAMLreturn(Val_unit);
}

value done_Instance( instance )
     value instance;
{
  TT_Done_Instance( *(TT_Instance *)instance );
  return Val_unit;
}

value get_Instance_Metrics( instance )
     value instance;
{
  CAMLparam1(instance);
  CAMLlocal1(res);

  TT_Instance_Metrics imetrics;
  TT_Get_Instance_Metrics( *(TT_Instance *)instance, &imetrics);

  /*
  res = alloc_tuple(7);
  Field(res,0) = Val_int(imetrics.pointSize);
  Field(res,1) = Val_int(imetrics.x_ppem);
  Field(res,2) = Val_int(imetrics.y_ppem);
  Field(res,3) = Val_int(imetrics.x_scale);
  Field(res,4) = Val_int(imetrics.y_scale);
  Field(res,5) = Val_int(imetrics.x_resolution);
  Field(res,6) = Val_int(imetrics.y_resolution);
  */
  res = alloc_tuple(4);
  Field(res,0) = Val_int(imetrics.x_ppem);
  Field(res,1) = Val_int(imetrics.y_ppem);
  Field(res,2) = Val_int(imetrics.x_scale);
  Field(res,3) = Val_int(imetrics.y_scale);

  CAMLreturn(res);
}

value new_Glyph( face )
     value face;
{
  TT_Glyph *glyph;
  
  if( (glyph = stat_alloc( sizeof(TT_Glyph) )) == NULL ){
    failwith( "Memory over" );
  }
  if( TT_New_Glyph( *(TT_Face *)face, glyph ) ){
    failwith( "Could not initialize glyph" );
  }
  return (value) glyph;
}

value done_Glyph( glyph )
     value glyph;
{
  TT_Done_Glyph( *(TT_Glyph *)glyph );
  return Val_unit;
}

value load_Glyph( instance, glyph, glyph_index, scale, hint )
     value instance;
     value glyph;
     value glyph_index;
     value scale; /* usually true */
     value hint;  /* usually true */
{
  int load_flags = 0;

  if( Bool_val(scale) ) load_flags |= TTLOAD_SCALE_GLYPH;
  if( Bool_val(hint)  ) load_flags |= TTLOAD_HINT_GLYPH;

  TT_Load_Glyph( *(TT_Instance*)instance,
		 *(TT_Glyph*)glyph,
		Int_val(glyph_index),
		load_flags );
  return Val_unit;
}

value get_Glyph_Outline( glyph )
value glyph;
{
  TT_Outline *outline;
  
  if( (outline = stat_alloc( sizeof(TT_Outline) )) == NULL ){
    failwith( "Memory over" );
  }
  TT_Get_Glyph_Outline( *(TT_Glyph*)glyph, outline );
  return (value) outline;
}

value get_Glyph_Metrics( glyph )
value glyph;
{
  CAMLparam1(glyph);
  CAMLlocal2(res1,res);
  
  TT_Glyph_Metrics metrics;
  
  TT_Get_Glyph_Metrics( *(TT_Glyph*)glyph, &metrics );
 
  res1 = alloc_tuple(4);
  Field(res1,0) = Val_int(metrics.bbox.xMin);
  Field(res1,1) = Val_int(metrics.bbox.yMin);
  Field(res1,2) = Val_int(metrics.bbox.xMax);
  Field(res1,3) = Val_int(metrics.bbox.yMax);

  res = alloc_tuple(4);
  Field(res,0) = res1;
  Field(res,1) = Val_int(metrics.bearingX);
  Field(res,2) = Val_int(metrics.bearingY);
  Field(res,3) = Val_int(metrics.advance);

  CAMLreturn(res);
}

value get_Big_Glyph_Metrics( glyph )
value glyph;
{
  CAMLparam1(glyph);
  CAMLlocal4(res0,res1,res2,res);
  
  TT_Big_Glyph_Metrics big_metrics;
  
  TT_Get_Glyph_Big_Metrics( *(TT_Glyph*)glyph, &big_metrics );
 
  res0 = alloc_tuple(4);
  Field(res0,0) = Val_int(big_metrics.bbox.xMin);
  Field(res0,1) = Val_int(big_metrics.bbox.yMin);
  Field(res0,2) = Val_int(big_metrics.bbox.xMax);
  Field(res0,3) = Val_int(big_metrics.bbox.yMax);

  res1 = alloc_tuple(3);
  Field(res1,0) = Val_int(big_metrics.horiBearingX);
  Field(res1,1) = Val_int(big_metrics.horiBearingY);
  Field(res1,2) = Val_int(big_metrics.horiAdvance);

  res2 = alloc_tuple(3);
  Field(res2,0) = Val_int(big_metrics.vertBearingX);
  Field(res2,1) = Val_int(big_metrics.vertBearingY);
  Field(res2,2) = Val_int(big_metrics.vertAdvance);

  res = alloc_tuple(3);
  Field(res,0) = res0;
  Field(res,1) = res1;
  Field(res,2) = res2;

  CAMLreturn(res);
}

value done_Outline( outline )
     value outline;
{
  TT_Done_Outline( (TT_Outline *)outline );
  return Val_unit;
}

value transform_Outline( outline, xx, xy, yx, yy )
     value outline;
     value xx,xy,yx,yy; /* 16.16 */
{
  TT_Matrix matrix = { Int_val(xx), 
		         Int_val(xy), 
		       Int_val(yx), 
		         Int_val(yy) };

  TT_Transform_Outline( (TT_Outline *) outline, &matrix );
  return Val_unit;
}

value translate_Outline( outline, x, y )
     value outline;
     value x, y; /* 26.6 */
{
  TT_Translate_Outline( (TT_Outline *) outline, Int_val(x), Int_val(y) );
  return Val_unit;
}

value get_Outline_BBox( outline )
     value outline;
{
  CAMLparam1(outline);
  CAMLlocal1(res);

  TT_BBox bbox;
  TT_Get_Outline_BBox( (TT_Outline *) outline, &bbox ); 

  res = alloc_tuple(4);
  Field(res,0) = Val_int(bbox.xMin);
  Field(res,1) = Val_int(bbox.yMin);
  Field(res,2) = Val_int(bbox.xMax);
  Field(res,3) = Val_int(bbox.yMax);

  CAMLreturn(res);
}


/* ********************************************************************
   
  additional C functions which are necessary to obtain 
  outlines of glyphs.

   Koji Kagawa, RISE, Kagawa University
   (kagawa@eng.kagawa-u.ac.jp)
  
 ******************************************************************** */

value get_Face_Properties(face)
     value face;
{
  CAMLparam1(face);
  CAMLlocal1(res);
  
  TT_Face_Properties  properties;
  TT_Get_Face_Properties( *(TT_Face*)face, &properties );

  res = alloc_tuple(8);
  Store_field(res,0, Val_int(properties.num_Glyphs));
  Store_field(res,1, Val_int(properties.max_Points));
  Store_field(res,2, Val_int(properties.max_Contours));
  Store_field(res,3, Val_int(properties.num_CharMaps));
  Store_field(res,4, Val_int(properties.num_Names));
  Store_field(res,5, Val_int(properties.num_Faces));
  
  Store_field(res,6, Val_bool(properties.horizontal == NULL ? 0 : 1));
  Store_field(res,7, Val_bool(properties.vertical == NULL ? 0 : 1));

  CAMLreturn(res);
}

value get_Outline_Contents(value vol) {
/* *****************************************************************
    
   Concrete definitions of TT_Outline might vary from version to
   version.

   This definition assumes freetype 1.3.1.
     
 ***************************************************************** */
  CAMLparam1(vol);
  CAMLlocal5(points, flags, contours, res, tmp);
  int i;

  TT_Outline* outline = (TT_Outline*)vol;
  int n_contours = outline->n_contours;
  int n_points   = outline->n_points;

  points   = alloc_tuple(n_points);
  flags    = alloc_tuple(n_points);
  contours = alloc_tuple(n_contours);

  for( i=0; i<n_points; i++ ) {
    TT_Vector* raw_points = outline->points;
    TT_Byte*   raw_flags  = outline->flags;
    tmp = alloc_tuple(2);
    /* caution: 26.6 fixed into 31 bit */
    Store_field(tmp, 0, Val_int(raw_points[i].x)); 
    Store_field(tmp, 1, Val_int(raw_points[i].y)); 
    Store_field(points, i, tmp);
    if ( raw_flags[i] & 1 /* = FT_Curve_Tag_On (version 2.0) */ ) {  
      Store_field(flags, i, Val_int(0)); /* On point */
    } else if ( 1 /* oops... raw_flags[i] & FT_Curve_Tag_Cubic */ ) {  
      Store_field(flags, i, Val_int(2)); /* Off point, cubic */ 
    } else {
      Store_field(flags, i, Val_int(1)); /* Off point, conic */
    }
  }

  for( i=0; i<n_contours; i++ ) {
    TT_UShort* raw_contours = outline->contours;
    Store_field(contours, i, Val_int(raw_contours[i]));
  }
  
  res = alloc_tuple(5);
  Store_field(res, 0, Val_int(n_contours));
  Store_field(res, 1, Val_int(n_points));
  Store_field(res, 2, points);
  Store_field(res, 3, flags);
  Store_field(res, 4, contours);
  
  CAMLreturn(res);
}


/* End of the additions by Koji Kagawa */

value new_Pixmap( width, height )
     value width, height;
{
  TT_Raster_Map *map;

  if( (map = stat_alloc( sizeof(TT_Raster_Map) )) == NULL ){
    failwith( "Memory over" );
  }
  map->rows = Int_val(height);
  map->width = Int_val(width);
  map->flow = TT_Flow_Up;
  map->cols = (map->width+3) & -4;
  map->size = map->cols * map->rows;
  if( (map->bitmap = malloc( (int) map->size )) == NULL ){
    failwith( "Memory over" );
  }
  /* and clear */
  memset( map->bitmap, 0, map->size );
  return (value) map;
}

value new_Bitmap( width, height )
     value width, height;
{
  TT_Raster_Map *map;

  if( (map = stat_alloc( sizeof(TT_Raster_Map) )) == NULL ){
    failwith( "Memory over" );
  }
  map->rows = Int_val(height);
  map->width = Int_val(width);
  map->flow = TT_Flow_Up;
  map->cols = (map->width+7)/8;
  map->size = map->cols * map->rows;
  if( (map->bitmap = malloc( (int) map->size )) == NULL ){
    failwith( "Memory over" );
  }
  /* and clear */
  memset( map->bitmap, 0, map->size );
  return (value) map;
}

value clear_Map( pixmap )
     value pixmap;
{
  TT_Raster_Map *map;

  map = (TT_Raster_Map*) pixmap;

  memset( map->bitmap, 0, map->size );
  return Val_unit;
}

value done_Map( map )
     value map;
{
  free( ((TT_Raster_Map *)map)->bitmap );
  free( (TT_Raster_Map *)map );
  return Val_unit;
}

#define RAWMAP(map) ((TT_Raster_Map *)(map))

value get_pixmap_pixel( map, x, y )
     value map, x, y;
{
  CAMLparam3(map, x, y);
  int ix, iy;
  ix = Int_val(x);
  iy = Int_val(y);
  CAMLreturn(Val_int(
     ((char*)(RAWMAP(map)->bitmap))[RAWMAP(map)->cols * iy + ix]));
}
 
value get_bitmap_pixel( map, x, y )
     value map, x, y;
{
  CAMLparam3(map, x, y);
  int ix, iy;
  ix = Int_val(x);
  iy = Int_val(y);
  CAMLreturn(Val_int(
   (((((char*)(RAWMAP(map)->bitmap))[RAWMAP(map)->cols * iy + (ix >> 3)] )
     & (128 >> (ix & 7))) ? 4 : 0)));
}
 
value get_Outline_Pixmap( engine, outline, pixmap )
     value engine;
     value outline;
     value pixmap;
{
  TT_Get_Outline_Pixmap( *(TT_Engine *)engine, 
			(TT_Outline *) outline, (TT_Raster_Map *) pixmap );
  return Val_unit;
}

value get_Outline_Bitmap( engine, outline, pixmap )
     value engine;
     value outline;
     value pixmap;
{
  TT_Get_Outline_Bitmap( *(TT_Engine *)engine, 
			(TT_Outline *) outline, (TT_Raster_Map *) pixmap );
  return Val_unit;
}

value get_Glyph_Pixmap( glyph, pixmap, x, y )
     value glyph, pixmap, x, y;
{
  TT_Get_Glyph_Pixmap( *(TT_Glyph*)glyph,
		      (TT_Raster_Map*)pixmap,
		       Int_val(x), Int_val (y));
  return Val_unit;
}

value get_Glyph_Bitmap( glyph, pixmap, x, y )
     value glyph, pixmap, x, y;
{
  TT_Get_Glyph_Bitmap( *(TT_Glyph*)glyph,
		      (TT_Raster_Map*)pixmap,
		       Int_val(x), Int_val (y));
  return Val_unit;
}

value get_Maximum_Bbox( face )
     value face;
{
  CAMLparam1(face);
  CAMLlocal1(res);
  TT_Face_Properties prop;

  TT_Get_Face_Properties( *(TT_Face*)face, &prop );
  res = alloc_tuple(4);
  Field(res,0) = Val_int(prop.header->xMin);
  Field(res,1) = Val_int(prop.header->yMin);
  Field(res,2) = Val_int(prop.header->xMax);
  Field(res,3) = Val_int(prop.header->yMax);

  CAMLreturn(res);
}


value get_CharMap_Count( face )
     value face;
{
  /* return Val_int(TT_Get_CharMap_Count ( *(TT_Face*)face )); */
  /* This TT_Get_CharMap_Count is deprecated. */

  TT_Face_Properties prop;

  TT_Get_Face_Properties( *(TT_Face*)face, &prop );
  return Val_int(prop.num_CharMaps);
}

value get_CharMap_ID( face, index )
     value face, index;
{
  CAMLparam2(face,index);
  CAMLlocal1(res);

  short platform, encoding;

  TT_Get_CharMap_ID( *(TT_Face*)face, Int_val(index), 
		    &platform, &encoding );
  
  res = alloc_tuple(2);
  Field(res,0) = Val_int(platform);
  Field(res,1) = Val_int(encoding);

  CAMLreturn(res);
}

value get_CharMap( face, index )
     value face, index;
{
  TT_CharMap *charmap; 

  if( (charmap = stat_alloc( sizeof(TT_CharMap) )) == NULL ){
    failwith( "Memory over" );
  }
  TT_Get_CharMap( *(TT_Face*)face, Int_val(index), charmap );
  return (value)charmap;
}

value done_CharMap( charmap )
     value charmap;
{
  stat_free( (void *) charmap );
  return Val_unit;
}

value char_Index( charmap, code )
     value charmap, code;
{
  return Val_int(TT_Char_Index( *(TT_CharMap*)charmap, Int_val(code) ));
}

value get_num_Glyphs( face )
     value face;
{
  TT_Face_Properties prop;

  TT_Get_Face_Properties( *(TT_Face*)face, &prop );
  return Val_int(prop.num_Glyphs);
}

#else

void freetype_not_supported()
{
  failwith( "freetype is not supported" );
}
#define nodef(f) value f(){freetype_not_supported(); return(Val_unit);}

nodef(init_FreeType)
nodef(done_FreeType)
nodef(open_Face)
nodef(close_Face)
nodef(flush_Face)
nodef(new_Instance)
nodef(set_Instance_Resolutions)
nodef(set_Instance_CharSizes)
nodef(done_Instance)
nodef(get_Instance_Metrics)
nodef(new_Glyph)
nodef(done_Glyph)
nodef(load_Glyph)
nodef(get_Glyph_Outline)
nodef(get_Glyph_Metrics)
nodef(done_Outline)
nodef(transform_Outline)
nodef(translate_Outline)
nodef(new_Pixmap)
nodef(new_Bitmap)
nodef(clear_Map)
nodef(done_Map)
nodef(get_pixmap_pixel)
nodef(get_bitmap_pixel)
nodef(get_Outline_Pixmap)
nodef(get_Glyph_Pixmap)
nodef(get_Outline_Bitmap)
nodef(get_Glyph_Bitmap)
nodef(get_CharMap_Count)
nodef(get_CharMap_ID)
nodef(get_CharMap)
nodef(done_CharMap)
nodef(char_Index)
nodef(get_num_Glyphs)
nodef(get_Maximum_Bbox)
nodef(set_Instance_PixelSizes)
nodef(get_Big_Glyph_Metrics)
nodef(get_Outline_BBox)
nodef(get_Face_Properties)
nodef(get_Outline_Contents)
#endif


