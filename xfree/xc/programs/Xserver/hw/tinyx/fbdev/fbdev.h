/*
 * Copyright � 1999 Keith Packard
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Keith Packard not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Keith Packard makes no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * KEITH PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL KEITH PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */
/* $XFree86: xc/programs/Xserver/hw/tinyx/fbdev/fbdev.h,v 1.3 2005/01/29 00:19:33 dawes Exp $ */

#ifndef _FBDEV_H_
#define _FBDEV_H_
#include <stdio.h>
#include <sys/types.h>
#include <linux/fb.h>
#include <unistd.h>
#include <sys/mman.h>
#include "tinyx.h"
#include "layer.h"

#ifdef RANDR
#include "randrstr.h"
#endif

typedef struct _fbdevPriv {
    struct fb_var_screeninfo	var;
    struct fb_fix_screeninfo	fix;
    unsigned short		red[256];
    unsigned short		green[256];
    unsigned short		blue[256];
    int				fd;
    char			*fb;
    char			*fb_base;
} FbdevPriv;
    
typedef struct _fbdevScrPriv {
    Rotation			randr;
    Bool			shadow;
    LayerPtr			pLayer;
} FbdevScrPriv;

extern KdCardFuncs  fbdevFuncs;

Bool
fbdevInitialize (KdCardInfo *card, FbdevPriv *priv);

Bool
fbdevCardInit (KdCardInfo *card);

Bool
fbdevScreenInit (KdScreenInfo *screen);

Bool
fbdevScreenInitialize (KdScreenInfo *screen, FbdevScrPriv *scrpriv);
    
Bool
fbdevInitScreen (ScreenPtr pScreen);

void
fbdevPreserve (KdCardInfo *card);

Bool
fbdevEnable (ScreenPtr pScreen);

Bool
fbdevDPMS (ScreenPtr pScreen, int mode);

void
fbdevDisable (ScreenPtr pScreen);

void
fbdevRestore (KdCardInfo *card);

void
fbdevScreenFini (KdScreenInfo *screen);

void
fbdevCardFini (KdCardInfo *card);

void
fbdevGetColors (ScreenPtr pScreen, int fb, int n, xColorItem *pdefs);

void
fbdevPutColors (ScreenPtr pScreen, int fb, int n, xColorItem *pdefs);


#endif /* _FBDEV_H_ */
