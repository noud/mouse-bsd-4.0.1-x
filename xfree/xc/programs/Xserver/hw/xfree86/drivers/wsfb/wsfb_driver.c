/* $OpenBSD: wsfb_driver.c,v 1.18 2003/04/02 16:42:13 jason Exp $ */
/*
 * Copyright (c) 2001 Matthieu Herrb
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *    - Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    - Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following
 *      disclaimer in the documentation and/or other materials provided
 *      with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */

/*
 * Based on fbdev.c written by:
 *
 * Authors:  Alan Hourihane, <alanh@fairlite.demon.co.uk>
 *	     Michel D�nzer, <michdaen@iiic.ethz.ch>
 */

#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <dev/wscons/wsconsio.h>

/* all driver need this */
#include "xf86.h"
#include "xf86_OSproc.h"
#include "xf86_ansic.h"

#include "mipointer.h"
#include "mibstore.h"
#include "micmap.h"
#include "colormapst.h"
#include "xf86cmap.h"
#include "shadow.h"
#include "dgaproc.h"

/* for visuals */
#include "xf1bpp.h"
#include "xf4bpp.h"
#include "fb.h"

#include "xf86Resources.h"
#include "xf86RAC.h"

#ifdef XvExtension
#include "xf86xv.h"
#endif

#include "wsfb.h"

/* #include "wsconsio.h" */

#ifndef XFree86LOADER
#include <sys/mman.h>
#endif

#if defined(__NetBSD__)
#define WSFB_DEFAULT_DEV "/dev/ttyE0"
#else
#define WSFB_DEFAULT_DEV "/dev/ttyC0"
#endif

#ifndef DEBUG
#define DEBUG 0
#endif

#if DEBUG
# define TRACE_ENTER(str)       ErrorF("wsfb: " str " %d\n",pScrn->scrnIndex)
# define TRACE_EXIT(str)        ErrorF("wsfb: " str " done\n")
# define TRACE(str)             ErrorF("wsfb trace: " str "\n")
#else
# define TRACE_ENTER(str)
# define TRACE_EXIT(str)
# define TRACE(str)
#endif

/* Prototypes */
#ifdef XFree86LOADER
static pointer WsfbSetup(pointer, pointer, int *, int *);
#endif
static Bool WsfbGetRec(ScrnInfoPtr);
static void WsfbFreeRec(ScrnInfoPtr);
static const OptionInfoRec * WsfbAvailableOptions(int, int);
static void WsfbIdentify(int);
static Bool WsfbProbe(DriverPtr, int);
static Bool WsfbPreInit(ScrnInfoPtr, int);
static Bool WsfbScreenInit(int, ScreenPtr, int, char **);
static Bool WsfbCloseScreen(int, ScreenPtr);
static void *WsfbWindowLinear(ScreenPtr, CARD32, CARD32, int, CARD32 *,
			      void *);
static Bool WsfbEnterVT(int, int);
static void WsfbLeaveVT(int, int);
static Bool WsfbSwitchMode(int, DisplayModePtr, int);
static int WsfbValidMode(int, DisplayModePtr, Bool, int);
static void WsfbLoadPalette(ScrnInfoPtr, int, int *, LOCO *, VisualPtr);
static Bool WsfbSaveScreen(ScreenPtr, int);
static void WsfbSave(ScrnInfoPtr);
static void WsfbRestore(ScrnInfoPtr);

/* dga stuff */
#ifdef XFreeXDGA
static Bool WsfbDGAOpenFramebuffer(ScrnInfoPtr, char **, unsigned int *,
				   unsigned int *, unsigned int *,
				   unsigned int *);
static Bool WsfbDGASetMode(ScrnInfoPtr, DGAModePtr);
static void WsfbDGASetViewport(ScrnInfoPtr, int, int, int);
static Bool WsfbDGAInit(ScrnInfoPtr, ScreenPtr);
#endif

/* helper functions */
static int wsfb_open(char *);
static pointer wsfb_mmap(size_t, off_t, int);

/*
 * This is intentionally screen-independent.  It indicates the binding
 * choice made in the first PreInit.
 */
static int pix24bpp = 0;

#define VERSION			4000
#define WSFB_NAME		"wsfb"
#define WSFB_DRIVER_NAME	"wsfb"
#define WSFB_MAJOR_VERSION	0
#define WSFB_MINOR_VERSION	1

DriverRec WSFB = {
	VERSION,
	WSFB_DRIVER_NAME,
	WsfbIdentify,
	WsfbProbe,
	WsfbAvailableOptions,
	NULL,
	0
};

/* Supported "chipsets" */
static SymTabRec WsfbChipsets[] = {
	{ 0, "wsfb" },
	{ -1, NULL }
};

/* Supported options */
typedef enum {
	OPTION_SHADOW_FB,
	OPTION_HW_CURSOR,
	OPTION_SW_CURSOR
} WsfbOpts;

static const OptionInfoRec WsfbOptions[] = {
	{ OPTION_SHADOW_FB, "ShadowFB", OPTV_BOOLEAN, {0}, FALSE},
	{ OPTION_SW_CURSOR, "SWcursor",	OPTV_BOOLEAN,	{0}, FALSE },
	{ OPTION_HW_CURSOR, "HWcursor",	OPTV_BOOLEAN,	{0}, FALSE },
	{ -1, NULL, OPTV_NONE, {0}, FALSE}
};

/* Symbols needed from other modules */
static const char *fbSymbols[] = {
	"fbPictureInit",
	"fbScreenInit",
	NULL
};
static const char *shadowSymbols[] = {
	"shadowAlloc",
	"shadowInit",
	"shadowUpdatePacked",
	NULL
};

static const char *ramdacSymbols[] = {
	"xf86CreateCursorInfoRec",
	"xf86DestroyCursorInfoRec",
	"xf86InitCursor",
	NULL
};

#ifdef XFree86LOADER
static XF86ModuleVersionInfo WsfbVersRec = {
	"wsfb",
	MODULEVENDORSTRING,
	MODINFOSTRING1,
	MODINFOSTRING2,
	XF86_VERSION_CURRENT,
	WSFB_MAJOR_VERSION, WSFB_MINOR_VERSION, 0,
	ABI_CLASS_VIDEODRV,
	ABI_VIDEODRV_VERSION,
	NULL,
	{0, 0, 0, 0}
};

XF86ModuleData wsfbModuleData = { &WsfbVersRec, WsfbSetup, NULL };

static pointer
WsfbSetup(pointer module, pointer opts, int *errmaj, int *errmin)
{
	static Bool setupDone = FALSE;
	const char *osname;

	/* Check that we're being loaded on a OpenBSD or NetBSD system */
	LoaderGetOS(&osname, NULL, NULL, NULL);
	if (!osname 
	    || ((strcmp(osname, "openbsd") != 0) 
	    && (strcmp(osname, "netbsd") != 0))) {
		if (errmaj)
			*errmaj = LDR_BADOS;
		if (errmin)
			*errmin = 0;
		return NULL;
	}
	if (!setupDone) {
		setupDone = TRUE;
		xf86AddDriver(&WSFB, module, 0);
		LoaderRefSymLists(fbSymbols, shadowSymbols, NULL);
		return (pointer)1;
	} else {
		if (errmaj != NULL)
			*errmaj = LDR_ONCEONLY;
		return NULL;
	}
}
#endif /* XFree86LOADER */

static Bool
WsfbGetRec(ScrnInfoPtr pScrn)
{

	if (pScrn->driverPrivate != NULL)
		return TRUE;

	pScrn->driverPrivate = xnfcalloc(sizeof(WsfbRec), 1);
	return TRUE;
}

static void
WsfbFreeRec(ScrnInfoPtr pScrn)
{

	if (pScrn->driverPrivate == NULL)
		return;
	xfree(pScrn->driverPrivate);
	pScrn->driverPrivate = NULL;
}

static const OptionInfoRec *
WsfbAvailableOptions(int chipid, int busid)
{
	return WsfbOptions;
}

static void
WsfbIdentify(int flags)
{
	xf86PrintChipsets(WSFB_NAME, "driver for wsdisplay framebuffer",
			  WsfbChipsets);
}


#define priv_open_device(n)	open(n,O_RDWR|O_NONBLOCK|O_EXCL)

/* Open the framebuffer device */
static int
wsfb_open(char *dev)
{
	int fd = -1;

	/* try argument from XF86Config first */
	if (dev == NULL || ((fd = priv_open_device(dev)) == -1)) {
		/* second: environment variable */
		dev = getenv("XDEVICE");
		if (dev == NULL || ((fd = priv_open_device(dev)) == -1)) {
			/* last try: default device */
			dev = WSFB_DEFAULT_DEV;
			if ((fd = priv_open_device(dev)) == -1) {
				return -1;
			}
		}
	}
	return fd;
}

/* Map the framebuffer's memory */
static pointer
wsfb_mmap(size_t len, off_t off, int fd)
{
	int pagemask, mapsize;
	caddr_t addr;
	pointer mapaddr;

	pagemask = getpagesize() - 1;
	mapsize = ((int) len + pagemask) & ~pagemask;
	addr = 0;

	/*
	 * try and make it private first, that way once we get it, an
	 * interloper, e.g. another server, can't get this frame buffer,
	 * and if another server already has it, this one won't.
	 */
	mapaddr = (pointer) mmap(addr, mapsize,
				 PROT_READ | PROT_WRITE, MAP_SHARED,
				 fd, off);
	if (mapaddr == (pointer) -1) {
		mapaddr = NULL;
	}
#if DEBUG
	ErrorF("mmap returns: addr %p len 0x%x\n", mapaddr, mapsize);
#endif
	return mapaddr;
}

static Bool
WsfbProbe(DriverPtr drv, int flags)
{
	int i, fd, entity;
       	GDevPtr *devSections;
	int numDevSections;
	char *dev;
	Bool foundScreen = FALSE;

	TRACE("probe start");

	/* For now, just bail out for PROBE_DETECT. */
	if (flags & PROBE_DETECT)
		return FALSE;

	if ((numDevSections = xf86MatchDevice(WSFB_DRIVER_NAME,
					      &devSections)) <= 0)
		return FALSE;

	for (i = 0; i < numDevSections; i++) {
		ScrnInfoPtr pScrn = NULL;

		dev = xf86FindOptionValue(devSections[i]->options, "device");
		if ((fd = wsfb_open(dev)) >= 0) {
			entity = xf86ClaimFbSlot(drv, 0, devSections[i], TRUE);
			pScrn = xf86ConfigFbEntity(NULL,0,entity,
						   NULL,NULL,NULL,NULL);
			if (pScrn != NULL) {
				foundScreen = TRUE;
				pScrn->driverVersion = VERSION;
				pScrn->driverName = WSFB_DRIVER_NAME;
				pScrn->name = WSFB_NAME;
				pScrn->Probe = WsfbProbe;
				pScrn->PreInit = WsfbPreInit;
				pScrn->ScreenInit = WsfbScreenInit;
				pScrn->SwitchMode = WsfbSwitchMode;
				pScrn->AdjustFrame = NULL;
				pScrn->EnterVT = WsfbEnterVT;
				pScrn->LeaveVT = WsfbLeaveVT;
				pScrn->ValidMode = WsfbValidMode;

				xf86DrvMsg(pScrn->scrnIndex, X_INFO,
				    "using %s\n", dev != NULL ? dev :
				    "default device");
			}
		}
	}
	xfree(devSections);
	TRACE("probe done");
	return foundScreen;
}

static Bool
WsfbPreInit(ScrnInfoPtr pScrn, int flags)
{
	WsfbPtr fPtr;
	int default_depth, wstype;
	char *dev;
	char *mod = NULL;
	const char *reqSym = NULL;
	Gamma zeros = {0.0, 0.0, 0.0};
	DisplayModePtr mode;
	MessageType from;

	if (flags & PROBE_DETECT) return FALSE;

	TRACE_ENTER("PreInit");

	if (pScrn->numEntities != 1) return FALSE;

	pScrn->monitor = pScrn->confScreen->monitor;

	WsfbGetRec(pScrn);
	fPtr = WSFBPTR(pScrn);

	fPtr->pEnt = xf86GetEntityInfo(pScrn->entityList[0]);

	pScrn->racMemFlags = RAC_FB | RAC_COLORMAP | RAC_CURSOR | RAC_VIEWPORT;
	pScrn->racIoFlags = pScrn->racMemFlags;

	dev = xf86FindOptionValue(fPtr->pEnt->device->options, "device");
	fPtr->fd = wsfb_open(dev);
	if (fPtr->fd == -1) {
		return FALSE;
	}

	if (ioctl(fPtr->fd, WSDISPLAYIO_GINFO, &fPtr->info) == -1) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "ioctl WSDISPLAY_GINFO: %s\n",
			   strerror(errno));
		return FALSE;
	}
	if (ioctl(fPtr->fd, WSDISPLAYIO_GTYPE, &wstype) == -1) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "ioctl WSDISPLAY_GTYPE: %s\n",
			   strerror(errno));
		return FALSE;
	}

	if (ioctl(fPtr->fd, WSDISPLAYIO_LINEBYTES, &fPtr->linebytes) == -1) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "ioctl WSDISPLAYIO_LINEBYTES: %s\n",
			   strerror(errno));
		return FALSE;
	}
        
	/* Handle depth */
	default_depth = fPtr->info.depth <= 24 ? fPtr->info.depth : 24;
	if (!xf86SetDepthBpp(pScrn, default_depth, default_depth,
			     fPtr->info.depth, Support24bppFb|Support32bppFb))
		return FALSE;
	xf86PrintDepthBpp(pScrn);

	/* Get the depth24 pixmap format */
	if (pScrn->depth == 24 && pix24bpp == 0)
		pix24bpp = xf86GetBppFromDepth(pScrn, 24);

	/* color weight */
	if (pScrn->depth > 8) {
		rgb zeros = { 0, 0, 0 }, masks;

		if (wstype == WSDISPLAY_TYPE_SUN24 ||
		    wstype == WSDISPLAY_TYPE_SUNCG12 ||
		    wstype == WSDISPLAY_TYPE_SUNCG14 ||
		    wstype == WSDISPLAY_TYPE_SUNTCX ||
		    wstype == WSDISPLAY_TYPE_SUNFFB) {
			masks.red = 0x0000ff;
			masks.green = 0x00ff00;
			masks.blue = 0xff0000;
		} else {
			masks.red = 0;
			masks.green = 0;
			masks.blue = 0;
		}

		if (!xf86SetWeight(pScrn, zeros, masks))
			return FALSE;
	}

	/* visual init */
	if (!xf86SetDefaultVisual(pScrn, -1))
		return FALSE;

	/* We don't currently support DirectColor at > 8bpp */
	if (pScrn->depth > 8 && pScrn->defaultVisual != TrueColor) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR, "Given default visual"
			   " (%s) is not supported at depth %d\n",
			   xf86GetVisualName(pScrn->defaultVisual),
			   pScrn->depth);
		return FALSE;
	}

	xf86SetGamma(pScrn,zeros);

	pScrn->progClock = TRUE;
	pScrn->rgbBits   = 8;
	pScrn->chipset   = "wsfb";
	pScrn->videoRam  = fPtr->linebytes * fPtr->info.height;

	xf86DrvMsg(pScrn->scrnIndex, X_INFO, "Vidmem: %dk\n",
		   pScrn->videoRam/1024);

	/* handle options */
	xf86CollectOptions(pScrn, NULL);
	if (!(fPtr->Options = xalloc(sizeof(WsfbOptions))))
		return FALSE;
	memcpy(fPtr->Options, WsfbOptions, sizeof(WsfbOptions));
	xf86ProcessOptions(pScrn->scrnIndex, fPtr->pEnt->device->options,
			   fPtr->Options);
	
	/* use shadow framebuffer by default, on dpeth >= 8 */
	if (pScrn->depth >= 8)
		fPtr->shadowFB = xf86ReturnOptValBool(fPtr->Options,
						      OPTION_SHADOW_FB, TRUE);
	else
		if (xf86ReturnOptValBool(fPtr->Options,
					 OPTION_SHADOW_FB, FALSE)) {
			xf86DrvMsg(pScrn->scrnIndex, X_WARNING,
				   "Shadow FB option ignored on depth 1");
		}

	/* fake video mode struct */
	mode = (DisplayModePtr)xalloc(sizeof(DisplayModeRec));
	mode->prev = mode;
	mode->next = mode;
	mode->name = "wsfb current mode";
	mode->status = MODE_OK;
	mode->type = M_T_BUILTIN;
	mode->Clock = 0;
	mode->HDisplay = fPtr->info.width;
	mode->HSyncStart = 0;
	mode->HSyncEnd = 0;
	mode->HTotal = 0;
	mode->HSkew = 0;
	mode->VDisplay = fPtr->info.height;
	mode->VSyncStart = 0;
	mode->VSyncEnd = 0;
	mode->VTotal = 0;
	mode->VScan = 0;
	mode->Flags = 0;
	if (pScrn->modes != NULL) {
		xf86DrvMsg(pScrn->scrnIndex, X_INFO,
		   "Ignoring mode specification from screen section\n");
	}
	pScrn->currentMode = pScrn->modes = mode;
	pScrn->virtualX = fPtr->info.width;
	pScrn->virtualY = fPtr->info.height;
	pScrn->displayWidth = pScrn->virtualX;

	/* Set the display resolution */
	xf86SetDpi(pScrn, 0, 0);

	from = X_DEFAULT;
	fPtr->HWCursor = TRUE;
	if (xf86GetOptValBool(fPtr->Options, OPTION_HW_CURSOR, &fPtr->HWCursor))
		from = X_CONFIG;
	if (xf86ReturnOptValBool(fPtr->Options, OPTION_SW_CURSOR, FALSE)) {
		from = X_CONFIG;
		fPtr->HWCursor = FALSE;
	}
	xf86DrvMsg(pScrn->scrnIndex, from, "Using %s cursor\n",
		fPtr->HWCursor ? "HW" : "SW");

	/* Load bpp-specific modules */
	switch(pScrn->bitsPerPixel) {
	case 1:
		mod = "xf1bpp";
		reqSym = "xf1bppScreenInit";
		break;
	case 4:
		mod = "xf4bpp";
		reqSym = "xf4bppScreenInit";
		break;
	default:
		mod = "fb";
		break;
	}


	/* Load shadow if needed */
	if (fPtr->shadowFB) {
		xf86DrvMsg(pScrn->scrnIndex, X_CONFIG,
			   "Using \"Shadow Framebuffer\"\n");
		if (xf86LoadSubModule(pScrn, "shadow") == NULL) {
			WsfbFreeRec(pScrn);
			return FALSE;
		}
		xf86LoaderReqSymLists(shadowSymbols, NULL);
	}
	if (mod && xf86LoadSubModule(pScrn, mod) == NULL) {
		WsfbFreeRec(pScrn);
		return FALSE;
	}
	if (mod) {
		if (reqSym) {
			xf86LoaderReqSymbols(reqSym, NULL);
		} else {
			xf86LoaderReqSymLists(fbSymbols, NULL);
		}
	}
	if (xf86LoadSubModule(pScrn, "ramdac") == NULL) {
		WsfbFreeRec(pScrn);
		return FALSE;
	}
	xf86LoaderReqSymLists(ramdacSymbols, NULL);
	
	TRACE_EXIT("PreInit");
	return TRUE;
}

static Bool
WsfbScreenInit(int scrnIndex, ScreenPtr pScreen, int argc, char **argv)
{
	ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
	WsfbPtr fPtr = WSFBPTR(pScrn);
	VisualPtr visual;
	int ret, flags, width, height, i;
	int wsmode = WSDISPLAYIO_MODE_DUMBFB;
	size_t len;

	TRACE_ENTER("WsfbScreenInit");
#if DEBUG
	ErrorF("\tbitsPerPixel=%d, depth=%d, defaultVisual=%s\n"
	       "\tmask: %x,%x,%x, offset: %d,%d,%d\n",
	       pScrn->bitsPerPixel,
	       pScrn->depth,
	       xf86GetVisualName(pScrn->defaultVisual),
	       pScrn->mask.red,pScrn->mask.green,pScrn->mask.blue,
	       pScrn->offset.red,pScrn->offset.green,pScrn->offset.blue);
#endif
	switch (fPtr->info.depth) {
	case 1:
	case 8:
		len = fPtr->linebytes*fPtr->info.height;
		break;
	case 16:
		if (fPtr->linebytes == fPtr->info.width) {
			len = fPtr->info.width*fPtr->info.height*sizeof(short);
		} else {
			len = fPtr->linebytes*fPtr->info.height;
		}
		break;
	case 32:
		if (fPtr->linebytes == fPtr->info.width) {
			len = fPtr->info.width*fPtr->info.height*sizeof(int);
		} else {
			len = fPtr->linebytes*fPtr->info.height;
		}
		break;
	default:
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "unsupported depth %d\n", fPtr->info.depth);
		return FALSE;
	}
	/* Switch to graphics mode - required before mmap */
	if (ioctl(fPtr->fd, WSDISPLAYIO_SMODE, &wsmode) == -1) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "ioctl WSDISPLAYIO_SMODE: %s\n",
			   strerror(errno));
		return FALSE;
	}
	fPtr->fbmem = wsfb_mmap(len, 0, fPtr->fd);

	if (fPtr->fbmem == NULL) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "wsfb_mmap: %s\n", strerror(errno));
		return FALSE;
	}
	fPtr->fbmem_len = len;

	WsfbSave(pScrn);
	pScrn->vtSema = TRUE;

	/* mi layer */
	miClearVisualTypes();
	if (pScrn->bitsPerPixel > 8) {
		if (!miSetVisualTypes(pScrn->depth, TrueColorMask,
				      pScrn->rgbBits, TrueColor))
			return FALSE;
	} else {
		if (!miSetVisualTypes(pScrn->depth,
				      miGetDefaultVisualMask(pScrn->depth),
				      pScrn->rgbBits, pScrn->defaultVisual))
			return FALSE;
	}
	if (!miSetPixmapDepths())
		return FALSE;

	height = pScrn->virtualY;
	width = pScrn->virtualX;

	/* shadowfb */
	if (fPtr->shadowFB) {
		if ((fPtr->shadowmem = shadowAlloc(width, height,
					   pScrn->bitsPerPixel)) == NULL)
		return FALSE;

		fPtr->fbstart   = fPtr->shadowmem;
	} else {
		fPtr->shadowmem = NULL;
		fPtr->fbstart   = fPtr->fbmem;
	}
	switch (pScrn->bitsPerPixel) {
	case 1:
		ret = xf1bppScreenInit(pScreen, fPtr->fbstart,
				       width, height,
				       pScrn->xDpi, pScrn->yDpi,
				       pScrn->displayWidth);
		break;
	case 4:
		ret = xf4bppScreenInit(pScreen, fPtr->fbstart,
				       width, height,
				       pScrn->xDpi, pScrn->yDpi,
				       pScrn->displayWidth);
		break;
	case 8:
	case 16:
	case 32:
		ret = fbScreenInit(pScreen,
				   fPtr->fbstart,
				   width, height,
				   pScrn->xDpi, pScrn->yDpi,
				   pScrn->displayWidth,
				   pScrn->bitsPerPixel);
		break;
	default:
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "Unsupported bpp: %d", pScrn->bitsPerPixel);
		return FALSE;
	} /* case */

	if (!ret)
		return FALSE;

	if (pScrn->bitsPerPixel > 8) {
		/* Fixup RGB ordering */
		visual = pScreen->visuals + pScreen->numVisuals;
		while (--visual >= pScreen->visuals) {
			if ((visual->class | DynamicClass) == DirectColor) {
				visual->offsetRed   = pScrn->offset.red;
				visual->offsetGreen = pScrn->offset.green;
				visual->offsetBlue  = pScrn->offset.blue;
				visual->redMask     = pScrn->mask.red;
				visual->greenMask   = pScrn->mask.green;
				visual->blueMask    = pScrn->mask.blue;
			}
		}
	}

	if (pScrn->bitsPerPixel > 8) {
		if (!fbPictureInit(pScreen, NULL, 0))
			xf86DrvMsg(pScrn->scrnIndex, X_WARNING,
				   "RENDER extension initialisation failed.");
	}
	if (fPtr->shadowFB) {
		if (pScrn->bitsPerPixel < 8) {
			xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
				   "Shadow FB not available on < 8 depth");
		} else {
			if (!shadowInit(pScreen, shadowUpdatePacked,
					WsfbWindowLinear))
			return FALSE;
		}
	}

#ifdef XFreeXDGA
	WsfbDGAInit(pScrn, pScreen);
#endif

	xf86SetBlackWhitePixels(pScreen);
	miInitializeBackingStore(pScreen);
	xf86SetBackingStore(pScreen);

	/* software cursor */
	miDCInitialize(pScreen, xf86GetPointerScreenFuncs());
	
	/* check for hardware cursor support */
	if (fPtr->HWCursor)
		WsfbSetupCursor(pScreen);
	
	/* colormap */
	if (!miCreateDefColormap(pScreen))
		return FALSE;
		flags = CMAP_RELOAD_ON_MODE_SWITCH;
	if(!xf86HandleColormaps(pScreen, 256, 8, WsfbLoadPalette,
				NULL, flags))
		return FALSE;

	pScreen->SaveScreen = WsfbSaveScreen;

#ifdef XvExtension
	{
		XF86VideoAdaptorPtr *ptr;

		int n = xf86XVListGenericAdaptors(pScrn,&ptr);
		if (n) {
			xf86XVScreenInit(pScreen,ptr,n);
		}
	}
#endif

	/* Wrap the current CloseScreen function */
	fPtr->CloseScreen = pScreen->CloseScreen;
	pScreen->CloseScreen = WsfbCloseScreen;

	TRACE_EXIT("WsfbScreenInit");
	return TRUE;
}

static Bool
WsfbCloseScreen(int scrnIndex, ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86Screens[scrnIndex];
	WsfbPtr fPtr = WSFBPTR(pScrn);

	TRACE_ENTER("WsfbCloseScreen");

	if (pScrn->vtSema) {
		WsfbRestore(pScrn);
		if (munmap(fPtr->fbmem, fPtr->fbmem_len) == -1) {
			xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
				   "munmap: %s\n", strerror(errno));
		}

		fPtr->fbmem = NULL;
	}
	if (fPtr->shadowmem)
		xfree(fPtr->shadowmem);
#ifdef XFreeXDGA
	if (fPtr->pDGAMode) {
		xfree(fPtr->pDGAMode);
		fPtr->pDGAMode = NULL;
		fPtr->nDGAMode = 0;
	}
#endif
	pScrn->vtSema = FALSE;

	/* unwrap CloseScreen */
	pScreen->CloseScreen = fPtr->CloseScreen;
	return (*pScreen->CloseScreen)(scrnIndex, pScreen);
}

static void *
WsfbWindowLinear(ScreenPtr pScreen, CARD32 row, CARD32 offset, int mode,
		CARD32 *size, void *closure)
{
	ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
	WsfbPtr fPtr = WSFBPTR(pScrn);

	if (fPtr->linebytes)
		*size = fPtr->linebytes;
	else {
		if (ioctl(fPtr->fd, WSDISPLAYIO_LINEBYTES, size) == -1)
			return NULL;
		fPtr->linebytes = *size;
	}
	return ((CARD8 *)fPtr->fbmem + row *fPtr->linebytes + offset);
}

static Bool
WsfbEnterVT(int scrnIndex, int flags)
{
	ScrnInfoPtr pScrn = xf86Screens[scrnIndex];

	TRACE_ENTER("EnterVT");
	pScrn->vtSema = TRUE;
	return TRUE;
}

static void
WsfbLeaveVT(int scrnIndex, int flags)
{
#if DEBUG
	ScrnInfoPtr pScrn = xf86Screens[scrnIndex];
#endif

	TRACE_ENTER("LeaveVT");
}

static Bool
WsfbSwitchMode(int scrnIndex, DisplayModePtr mode, int flags)
{
#if DEBUG
	ScrnInfoPtr pScrn = xf86Screens[scrnIndex];
#endif

	TRACE_ENTER("SwitchMode");
	/* Nothing else to do */
	return TRUE;
}

static int
WsfbValidMode(int scrnIndex, DisplayModePtr mode, Bool verbose, int flags)
{
#if DEBUG
	ScrnInfoPtr pScrn = xf86Screens[scrnIndex];
#endif

	TRACE_ENTER("ValidMode");
	return MODE_OK;
}

static void
WsfbLoadPalette(ScrnInfoPtr pScrn, int numColors, int *indices,
	       LOCO *colors, VisualPtr pVisual)
{
	WsfbPtr fPtr = WSFBPTR(pScrn);
	struct wsdisplay_cmap cmap;
	unsigned char red[256],green[256],blue[256];
	int i, indexMin=256, indexMax=0;

	TRACE_ENTER("LoadPalette");

	cmap.count   = 1;
	cmap.red   = red;
	cmap.green = green;
	cmap.blue  = blue;

	if (numColors == 1) {
		/* Optimisation */
		cmap.index = indices[0];
		red[0]   = colors[indices[0]].red;
		green[0] = colors[indices[0]].green;
		blue[0]  = colors[indices[0]].blue;
		if (ioctl(fPtr->fd,WSDISPLAYIO_PUTCMAP, &cmap) == -1)
			ErrorF("ioctl FBIOPUTCMAP: %s\n", strerror(errno));
	} else {
		/* Change all colors in 2 syscalls */
		/* and limit the data to be transfered */
		for (i = 0; i < numColors; i++) {
			if (indices[i] < indexMin)
				indexMin = indices[i];
			if (indices[i] > indexMax)
				indexMax = indices[i];
		}
		cmap.index = indexMin;
		cmap.count = indexMax - indexMin + 1;
		cmap.red = &red[indexMin];
		cmap.green = &green[indexMin];
		cmap.blue = &blue[indexMin];
		/* Get current map */
		if (ioctl(fPtr->fd, WSDISPLAYIO_GETCMAP, &cmap) == -1)
			ErrorF("ioctl FBIOGETCMAP: %s\n", strerror(errno));
		/* Change the colors that require updating */
		for (i = 0; i < numColors; i++) {
			red[indices[i]]   = colors[indices[i]].red;
			green[indices[i]] = colors[indices[i]].green;
			blue[indices[i]]  = colors[indices[i]].blue;
		}
		/* Write the colormap back */
		if (ioctl(fPtr->fd,WSDISPLAYIO_PUTCMAP, &cmap) == -1)
			ErrorF("ioctl FBIOPUTCMAP: %s\n", strerror(errno));
	}
}

static Bool
WsfbSaveScreen(ScreenPtr pScreen, int mode)
{
	ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
	WsfbPtr fPtr = WSFBPTR(pScrn);
	int state;

	TRACE_ENTER("SaveScreen");

	if (!pScrn->vtSema)
		return TRUE;

	if (mode != SCREEN_SAVER_FORCER) {
		state = xf86IsUnblank(mode)?WSDISPLAYIO_VIDEO_ON:
		                            WSDISPLAYIO_VIDEO_OFF;
		ioctl(fPtr->fd,
		      WSDISPLAYIO_SVIDEO, &state);
	}
	return TRUE;
}


static void
WsfbSave(ScrnInfoPtr pScrn)
{
	WsfbPtr fPtr = WSFBPTR(pScrn);

	TRACE_ENTER("WsfbSave");
	fPtr->saved_cmap.index = 0;
	fPtr->saved_cmap.count = 256;
	fPtr->saved_cmap.red = fPtr->saved_red;
	fPtr->saved_cmap.green = fPtr->saved_green;
	fPtr->saved_cmap.blue = fPtr->saved_blue;
	if (ioctl(fPtr->fd, WSDISPLAYIO_GETCMAP,
		  &(fPtr->saved_cmap)) == -1) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "error saving colormap %s\n", strerror(errno));
	}

}

static void
WsfbRestore(ScrnInfoPtr pScrn)
{
	WsfbPtr fPtr = WSFBPTR(pScrn);
	int mode;

	TRACE_ENTER("WsfbRestore");

	/* reset colormap for text mode */
	fPtr->saved_cmap.index = 0;
	fPtr->saved_cmap.count = 256;
	fPtr->saved_cmap.red = fPtr->saved_red;
	fPtr->saved_cmap.green = fPtr->saved_green;
	fPtr->saved_cmap.blue = fPtr->saved_blue;
	if (ioctl(fPtr->fd, WSDISPLAYIO_PUTCMAP,
		  &(fPtr->saved_cmap)) == -1) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "error restoring colormap %s\n", strerror(errno));
	}

	/* Clear the screen */
	memset(fPtr->fbmem, 0, fPtr->fbmem_len);

	/* Restore the text mode */
	mode = WSDISPLAYIO_MODE_EMUL;
	if (ioctl(fPtr->fd, WSDISPLAYIO_SMODE, &mode) == -1) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "error setting text mode %s\n", strerror(errno));
	}
	TRACE_EXIT("WsfbRestore");
}

#ifdef XFreeXDGA
/***********************************************************************
 * DGA stuff
 ***********************************************************************/

static Bool
WsfbDGAOpenFramebuffer(ScrnInfoPtr pScrn, char **DeviceName,
		       unsigned int *ApertureBase, unsigned int *ApertureSize,
		       unsigned int *ApertureOffset, unsigned int *flags)
{
	*DeviceName = NULL;		/* No special device */
	*ApertureBase = (unsigned int)(pScrn->memPhysBase);
	*ApertureSize = pScrn->videoRam;
	*ApertureOffset = pScrn->fbOffset;
	*flags = 0;

	return TRUE;
}

static Bool
WsfbDGASetMode(ScrnInfoPtr pScrn, DGAModePtr pDGAMode)
{
	DisplayModePtr pMode;
	int scrnIdx = pScrn->pScreen->myNum;
	int frameX0, frameY0;

	if (pDGAMode) {
		pMode = pDGAMode->mode;
		frameX0 = frameY0 = 0;
	} else {
		if (!(pMode = pScrn->currentMode))
			return TRUE;

		frameX0 = pScrn->frameX0;
		frameY0 = pScrn->frameY0;
	}

	if (!(*pScrn->SwitchMode)(scrnIdx, pMode, 0))
		return FALSE;
	(*pScrn->AdjustFrame)(scrnIdx, frameX0, frameY0, 0);

	return TRUE;
}

static void
WsfbDGASetViewport(ScrnInfoPtr pScrn, int x, int y, int flags)
{
	(*pScrn->AdjustFrame)(pScrn->pScreen->myNum, x, y, flags);
}

static int
WsfbDGAGetViewport(ScrnInfoPtr pScrn)
{
	return (0);
}

static DGAFunctionRec WsfbDGAFunctions =
{
	WsfbDGAOpenFramebuffer,
	NULL,       /* CloseFramebuffer */
	WsfbDGASetMode,
	WsfbDGASetViewport,
	WsfbDGAGetViewport,
	NULL,       /* Sync */
	NULL,       /* FillRect */
	NULL,       /* BlitRect */
	NULL,       /* BlitTransRect */
};

static void
WsfbDGAAddModes(ScrnInfoPtr pScrn)
{
	WsfbPtr fPtr = WSFBPTR(pScrn);
	DisplayModePtr pMode = pScrn->modes;
	DGAModePtr pDGAMode;

	do {
		pDGAMode = xrealloc(fPtr->pDGAMode,
				    (fPtr->nDGAMode + 1) * sizeof(DGAModeRec));
		if (!pDGAMode)
			break;

		fPtr->pDGAMode = pDGAMode;
		pDGAMode += fPtr->nDGAMode;
		(void)memset(pDGAMode, 0, sizeof(DGAModeRec));

		++fPtr->nDGAMode;
		pDGAMode->mode = pMode;
		pDGAMode->flags = DGA_CONCURRENT_ACCESS | DGA_PIXMAP_AVAILABLE;
		pDGAMode->byteOrder = pScrn->imageByteOrder;
		pDGAMode->depth = pScrn->depth;
		pDGAMode->bitsPerPixel = pScrn->bitsPerPixel;
		pDGAMode->red_mask = pScrn->mask.red;
		pDGAMode->green_mask = pScrn->mask.green;
		pDGAMode->blue_mask = pScrn->mask.blue;
		pDGAMode->visualClass = pScrn->bitsPerPixel > 8 ?
			TrueColor : PseudoColor;
		pDGAMode->xViewportStep = 1;
		pDGAMode->yViewportStep = 1;
		pDGAMode->viewportWidth = pMode->HDisplay;
		pDGAMode->viewportHeight = pMode->VDisplay;

		if (fPtr->linebytes)
			pDGAMode->bytesPerScanline = fPtr->linebytes;
		else {
			ioctl(fPtr->fd, WSDISPLAYIO_LINEBYTES,
			      &fPtr->linebytes);
			pDGAMode->bytesPerScanline = fPtr->linebytes;
		}

		pDGAMode->imageWidth = pMode->HDisplay;
		pDGAMode->imageHeight =  pMode->VDisplay;
		pDGAMode->pixmapWidth = pDGAMode->imageWidth;
		pDGAMode->pixmapHeight = pDGAMode->imageHeight;
		pDGAMode->maxViewportX = pScrn->virtualX -
			pDGAMode->viewportWidth;
		pDGAMode->maxViewportY = pScrn->virtualY -
			pDGAMode->viewportHeight;

		pDGAMode->address = fPtr->fbstart;

		pMode = pMode->next;
	} while (pMode != pScrn->modes);
}

static Bool
WsfbDGAInit(ScrnInfoPtr pScrn, ScreenPtr pScreen)
{
	WsfbPtr fPtr = WSFBPTR(pScrn);

	if (pScrn->depth < 8)
		return FALSE;

	if (!fPtr->nDGAMode)
		WsfbDGAAddModes(pScrn);

	return (DGAInit(pScreen, &WsfbDGAFunctions,
			fPtr->pDGAMode, fPtr->nDGAMode));
}
#endif
