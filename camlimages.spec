Release: 1
Version: 2.12
Name: camlimages
Summary: camlimages library
Source: ftp://ftp.inria.fr/lang/caml-light/bazar-ocaml/camlimages-2.12.tgz
Group: Development/Libraries
URL: http://pauillac.inria.fr/camlimages/
Copyright: LGPL
Packager: <Jun.Furuse@inria.fr>
Provides: camlimages
Prefix: /usr

%description 
CamlImages is an image processing library for Objective Caml

%prep

%setup

%build
./configure
make all opt

%install
make install

%clean
make clean

%files
%dir /usr/lib/ocaml/camlimages
/usr/lib/ocaml/camlimages/Makefile.config
/usr/lib/ocaml/camlimages/bitmap.cmi
/usr/lib/ocaml/camlimages/bitmap.mli
/usr/lib/ocaml/camlimages/blend.cmi
/usr/lib/ocaml/camlimages/blend.mli
/usr/lib/ocaml/camlimages/bmp.cmi
/usr/lib/ocaml/camlimages/bmp.mli
/usr/lib/ocaml/camlimages/camlimages.cmi
/usr/lib/ocaml/camlimages/ci_bmp.a
/usr/lib/ocaml/camlimages/ci_bmp.cma
/usr/lib/ocaml/camlimages/ci_bmp.cmxa
/usr/lib/ocaml/camlimages/ci_core.a
/usr/lib/ocaml/camlimages/ci_core.cma
/usr/lib/ocaml/camlimages/ci_core.cmxa
/usr/lib/ocaml/camlimages/ci_freetype.a
/usr/lib/ocaml/camlimages/ci_freetype.cma
/usr/lib/ocaml/camlimages/ci_freetype.cmxa
/usr/lib/ocaml/camlimages/ci_gif.a
/usr/lib/ocaml/camlimages/ci_gif.cma
/usr/lib/ocaml/camlimages/ci_gif.cmxa
/usr/lib/ocaml/camlimages/ci_graphics.a
/usr/lib/ocaml/camlimages/ci_graphics.cma
/usr/lib/ocaml/camlimages/ci_graphics.cmxa
/usr/lib/ocaml/camlimages/ci_jpeg.a
/usr/lib/ocaml/camlimages/ci_jpeg.cma
/usr/lib/ocaml/camlimages/ci_jpeg.cmxa
/usr/lib/ocaml/camlimages/ci_lablgtk.a
/usr/lib/ocaml/camlimages/ci_lablgtk.cma
/usr/lib/ocaml/camlimages/ci_lablgtk.cmxa
/usr/lib/ocaml/camlimages/ci_png.a
/usr/lib/ocaml/camlimages/ci_png.cma
/usr/lib/ocaml/camlimages/ci_png.cmxa
/usr/lib/ocaml/camlimages/ci_ppm.a
/usr/lib/ocaml/camlimages/ci_ppm.cma
/usr/lib/ocaml/camlimages/ci_ppm.cmxa
/usr/lib/ocaml/camlimages/ci_ps.a
/usr/lib/ocaml/camlimages/ci_ps.cma
/usr/lib/ocaml/camlimages/ci_ps.cmxa
/usr/lib/ocaml/camlimages/ci_tiff.a
/usr/lib/ocaml/camlimages/ci_tiff.cma
/usr/lib/ocaml/camlimages/ci_tiff.cmxa
/usr/lib/ocaml/camlimages/ci_xpm.a
/usr/lib/ocaml/camlimages/ci_xpm.cma
/usr/lib/ocaml/camlimages/ci_xpm.cmxa
/usr/lib/ocaml/camlimages/ci_xvthumb.a
/usr/lib/ocaml/camlimages/ci_xvthumb.cma
/usr/lib/ocaml/camlimages/ci_xvthumb.cmxa
/usr/lib/ocaml/camlimages/cmyk32.cmi
/usr/lib/ocaml/camlimages/cmyk32.mli
/usr/lib/ocaml/camlimages/color.cmi
/usr/lib/ocaml/camlimages/color.mli
/usr/lib/ocaml/camlimages/colorhist.cmi
/usr/lib/ocaml/camlimages/config.h
/usr/lib/ocaml/camlimages/dllci_freetype.so
/usr/lib/ocaml/camlimages/dllci_gif.so
/usr/lib/ocaml/camlimages/dllci_jpeg.so
/usr/lib/ocaml/camlimages/dllci_png.so
/usr/lib/ocaml/camlimages/dllci_tiff.so
/usr/lib/ocaml/camlimages/dllci_xpm.so
/usr/lib/ocaml/camlimages/freetype.cmi
/usr/lib/ocaml/camlimages/freetype.mli
/usr/lib/ocaml/camlimages/ftlow.cmi
/usr/lib/ocaml/camlimages/ftlow.mli
/usr/lib/ocaml/camlimages/fttext.cmi
/usr/lib/ocaml/camlimages/fttext.mli
/usr/lib/ocaml/camlimages/genimage.cmi
/usr/lib/ocaml/camlimages/genimage.mli
/usr/lib/ocaml/camlimages/geometry.cmi
/usr/lib/ocaml/camlimages/gif.cmi
/usr/lib/ocaml/camlimages/gif.mli
/usr/lib/ocaml/camlimages/graphic_image.cmi
/usr/lib/ocaml/camlimages/graphic_image.mli
/usr/lib/ocaml/camlimages/image.cmi
/usr/lib/ocaml/camlimages/image.mli
/usr/lib/ocaml/camlimages/index16.cmi
/usr/lib/ocaml/camlimages/index16.mli
/usr/lib/ocaml/camlimages/index8.cmi
/usr/lib/ocaml/camlimages/index8.mli
/usr/lib/ocaml/camlimages/info.cmi
/usr/lib/ocaml/camlimages/info.mli
/usr/lib/ocaml/camlimages/jis_table.cmi
/usr/lib/ocaml/camlimages/jis_unicode.cmi
/usr/lib/ocaml/camlimages/jis_unicode.mli
/usr/lib/ocaml/camlimages/jpeg.cmi
/usr/lib/ocaml/camlimages/jpeg.mli
/usr/lib/ocaml/camlimages/libci_freetype.a
/usr/lib/ocaml/camlimages/libci_gif.a
/usr/lib/ocaml/camlimages/libci_jpeg.a
/usr/lib/ocaml/camlimages/libci_png.a
/usr/lib/ocaml/camlimages/libci_tiff.a
/usr/lib/ocaml/camlimages/libci_xpm.a
/usr/lib/ocaml/camlimages/mstring.cmi
/usr/lib/ocaml/camlimages/mstring.mli
/usr/lib/ocaml/camlimages/oBmp.cmi
/usr/lib/ocaml/camlimages/oColor.cmi
/usr/lib/ocaml/camlimages/oColor.mli
/usr/lib/ocaml/camlimages/oFreetype.cmi
/usr/lib/ocaml/camlimages/oGif.cmi
/usr/lib/ocaml/camlimages/oGraphic.cmi
/usr/lib/ocaml/camlimages/oImage.cmi
/usr/lib/ocaml/camlimages/oImage.mli
/usr/lib/ocaml/camlimages/oJpeg.cmi
/usr/lib/ocaml/camlimages/oPng.cmi
/usr/lib/ocaml/camlimages/oPpm.cmi
/usr/lib/ocaml/camlimages/oPs.cmi
/usr/lib/ocaml/camlimages/oTiff.cmi
/usr/lib/ocaml/camlimages/oXimage.cmi
/usr/lib/ocaml/camlimages/oXimage.mli
/usr/lib/ocaml/camlimages/oXimage2.cmi
/usr/lib/ocaml/camlimages/oXimage2.mli
/usr/lib/ocaml/camlimages/oXpm.cmi
/usr/lib/ocaml/camlimages/oXvthumb.cmi
/usr/lib/ocaml/camlimages/oXvthumb.mli
/usr/lib/ocaml/camlimages/png.cmi
/usr/lib/ocaml/camlimages/png.mli
/usr/lib/ocaml/camlimages/ppm.cmi
/usr/lib/ocaml/camlimages/ppm.mli
/usr/lib/ocaml/camlimages/ps.cmi
/usr/lib/ocaml/camlimages/ps.mli
/usr/lib/ocaml/camlimages/reduce.cmi
/usr/lib/ocaml/camlimages/reduce.mli
/usr/lib/ocaml/camlimages/region.cmi
/usr/lib/ocaml/camlimages/region.mli
/usr/lib/ocaml/camlimages/rgb24.cmi
/usr/lib/ocaml/camlimages/rgb24.mli
/usr/lib/ocaml/camlimages/rgba32.cmi
/usr/lib/ocaml/camlimages/rgba32.mli
/usr/lib/ocaml/camlimages/tiff.cmi
/usr/lib/ocaml/camlimages/tiff.mli
/usr/lib/ocaml/camlimages/tmpfile.cmi
/usr/lib/ocaml/camlimages/tmpfile.mli
/usr/lib/ocaml/camlimages/ximage.cmi
/usr/lib/ocaml/camlimages/ximage.mli
/usr/lib/ocaml/camlimages/ximage2.cmi
/usr/lib/ocaml/camlimages/ximage2.mli
/usr/lib/ocaml/camlimages/xpm.cmi
/usr/lib/ocaml/camlimages/xpm.mli
/usr/lib/ocaml/camlimages/xvthumb.cmi
/usr/lib/ocaml/camlimages/xvthumb.mli

%changelog

* Tue Apr 06 2004 <Pierre.Weis@inria.fr>

Packaged in a rpm. Here is the modus operandi:
cd /usr/src/redhat/SPECS/
cp camlimages.spec ./
rpm -ba --clean camlimages.spec
