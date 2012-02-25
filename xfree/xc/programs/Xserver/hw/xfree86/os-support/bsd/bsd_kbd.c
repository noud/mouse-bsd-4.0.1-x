/* $XFree86: xc/programs/Xserver/hw/xfree86/os-support/bsd/bsd_kbd.c,v 1.9 2004/01/07 17:05:28 tsi Exp $ */

/*
 * Copyright (c) 2002 by The XFree86 Project, Inc.
 * Author: Ivan Pascal.
 *
 * Based on the code from bsd_io.c which is
 * Copyright 1992 by Rich Murphey <Rich@Rice.edu>
 * Copyright 1993 by David Dawes <dawes@xfree86.org>
 *
 * Hacked over by Mouse to add support for Sun keyboards on serial lines.
 */

#define NEED_EVENTS
#include "X.h"

#include "compiler.h"

#include "xf86.h"
#include "xf86Priv.h"
#include "xf86_OSlib.h"

#include "xf86Xinput.h"
#include "xf86OSKbd.h"
#include "atKeynames.h"
#include "bsd_kbd.h"

#include <termios.h>

extern Bool VTSwitchEnabled;
#ifdef USE_VT_SYSREQ
extern Bool VTSysreqToggle;
#endif

static KbdProtocolRec protocols[] = {
   {"standard", PROT_STD },
   {"sun", PROT_SUN },
#ifdef WSCONS_SUPPORT
   {"wskbd", PROT_WSCONS },
#endif
   { NULL, PROT_UNKNOWN_KBD }
};

typedef struct {
   struct termio kbdtty;
} BsdKbdPrivRec, *BsdKbdPrivPtr;

static
int KbdInit(InputInfoPtr pInfo, int what)
{
    KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;
    BsdKbdPrivPtr priv = (BsdKbdPrivPtr) pKbd->private;

    if (pKbd->isConsole) {
        switch (pKbd->consType) {
#if defined(PCCONS_SUPPORT) || defined(SYSCONS_SUPPORT) || defined (PCVT_SUPPORT)  || defined (WSCONS_SUPPORT)
	    case PCCONS:
	    case SYSCONS:
	    case PCVT:
#if defined WSCONS_SUPPORT
            case WSCONS:
#endif
 	         tcgetattr(pInfo->fd, &(priv->kbdtty));
#endif
	         break;
        }
    }

    return Success;
}

static void
SetKbdLeds(InputInfoPtr pInfo, int leds)
{
    KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;
    int real_leds = 0;

    if (leds & XLED1)  real_leds |= LED_CAP;
    if (leds & XLED2)  real_leds |= LED_NUM;
    if (leds & XLED3)  real_leds |= LED_SCR;
    if (leds & XLED4)  real_leds |= LED_SCR;

    switch (pKbd->consType) {

	case PCCONS:
		break;
#if defined (SYSCONS_SUPPORT) || defined (PCVT_SUPPORT)
	case SYSCONS:
	case PCVT:
	     ioctl(pInfo->fd, KDSETLED, real_leds);
	     break;
#endif
#if defined(WSCONS_SUPPORT)
        case WSCONS:
             ioctl(pInfo->fd, WSKBDIO_SETLEDS, &real_leds);
             break;
#endif
    }
}

static int
GetKbdLeds(InputInfoPtr pInfo)
{
    KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;
    int leds = 0, real_leds = 0;

    switch (pKbd->consType) {
	case PCCONS:
	     break;
#if defined (SYSCONS_SUPPORT) || defined (PCVT_SUPPORT)
	case SYSCONS:
	case PCVT:
	     ioctl(pInfo->fd, KDGETLED, &real_leds);
	     break;
#endif
#if defined(WSCONS_SUPPORT)
        case WSCONS:
             ioctl(pInfo->fd, WSKBDIO_GETLEDS, &real_leds);
             break;
#endif
    }

    if (real_leds & LED_CAP) leds |= XLED1;
    if (real_leds & LED_NUM) leds |= XLED2;
    if (real_leds & LED_SCR) leds |= XLED3;

    return(leds);
}

static void
SetKbdRepeat(InputInfoPtr pInfo, char rad)
{
    KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;
    switch (pKbd->consType) {

	case PCCONS:
		break;
#if defined (SYSCONS_SUPPORT) || defined (PCVT_SUPPORT)
	case SYSCONS:
	case PCVT:
		ioctl(pInfo->fd, KDSETRAD, rad);
		break;
#endif
    }
}

static int
KbdOn(InputInfoPtr pInfo, int what)
{
    KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;
#if defined(SYSCONS_SUPPORT) || defined(PCCONS_SUPPORT) || defined(PCVT_SUPPORT)
    BsdKbdPrivPtr priv = (BsdKbdPrivPtr) pKbd->private;
    struct termios nTty;
#endif
#ifdef WSCONS_SUPPORT
    int option;
#endif

    if (pKbd->isConsole) {
        switch (pKbd->consType) {

#if defined(SYSCONS_SUPPORT) || defined(PCCONS_SUPPORT) || defined(PCVT_SUPPORT)
	    case SYSCONS:
	    case PCCONS:
	    case PCVT:
#ifdef WSCONS_SUPPORT
            case WSCONS:
#endif
		 nTty = priv->kbdtty;
		 nTty.c_iflag = IGNPAR | IGNBRK;
		 nTty.c_oflag = 0;
		 nTty.c_cflag = CREAD | CS8;
		 nTty.c_lflag = 0;
		 nTty.c_cc[VTIME] = 0;
		 nTty.c_cc[VMIN] = 1;
		 cfsetispeed(&nTty, 9600);
		 cfsetospeed(&nTty, 9600);
		 tcsetattr(pInfo->fd, TCSANOW, &nTty);
                 break; 
#endif 
        }
#if defined (SYSCONS_SUPPORT) || defined (PCVT_SUPPORT) || defined (WSCONS_SUPPORT)
        switch (pKbd->consType) {
	    case SYSCONS:
	    case PCVT:
#ifdef K_CODE
                 if (pKbd->CustomKeycodes)
		     ioctl(pInfo->fd, KDSKBMODE, K_CODE);
	         else
	             ioctl(pInfo->fd, KDSKBMODE, K_RAW);
#else
		 ioctl(pInfo->fd, KDSKBMODE, K_RAW);
#endif
	         break;
#endif
#ifdef WSCONS_SUPPORT
            case WSCONS:
                 option = WSKBD_RAW;
                 if (ioctl(pInfo->fd, WSKBDIO_SETMODE, &option) == -1) {
			 FatalError("can't switch keyboard to raw mode. "
				    "Enable support for it in the kernel\n"
				    "or use for example:\n\n"
				    "Option \"Protocol\" \"wskbd\"\n"
				    "Option \"Device\" \"/dev/wskbd0\"\n"
				    "\nin your XF86Config(5) file\n");
		 }
		 break;
#endif
        }
    }
    return Success;
}

static int
KbdOff(InputInfoPtr pInfo, int what)
{
    KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;
    BsdKbdPrivPtr priv = (BsdKbdPrivPtr) pKbd->private;
#ifdef WSCONS_SUPPORT
    int option;
#endif

    if (pKbd->isConsole) {
        switch (pKbd->consType) {
#if defined (SYSCONS_SUPPORT) || defined (PCVT_SUPPORT)
	    case SYSCONS:
	    case PCVT:
	         ioctl(pInfo->fd, KDSKBMODE, K_XLATE);
	         /* FALL THROUGH */
#endif
#if defined(SYSCONS_SUPPORT) || defined(PCCONS_SUPPORT) || defined(PCVT_SUPPORT)
	    case PCCONS:
	         tcsetattr(pInfo->fd, TCSANOW, &(priv->kbdtty));
	         break;
#endif
#ifdef WSCONS_SUPPORT
            case WSCONS:
                 option = WSKBD_TRANSLATED;
                 ioctl(xf86Info.consoleFd, WSKBDIO_SETMODE, &option);
                 tcsetattr(xf86Info.consoleFd, TCSANOW, &(priv->kbdtty));
	         break;
#endif
        }
    }
    return Success;
}

static void
SoundBell(InputInfoPtr pInfo, int loudness, int pitch, int duration)
{
    KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;
#ifdef WSCONS_SUPPORT
    struct wskbd_bell_data wsb;
#endif

    if (loudness && pitch) {
    	switch (pKbd->consType) {
#ifdef PCCONS_SUPPORT
	    case PCCONS:
	         { int data[2];
		   data[0] = pitch;
		   data[1] = (duration * loudness) / 50;
		   ioctl(pInfo->fd, CONSOLE_X_BELL, data);
		   break;
		 }
#endif
#if defined (SYSCONS_SUPPORT) || defined (PCVT_SUPPORT)
	    case SYSCONS:
	    case PCVT:
		 ioctl(pInfo->fd, KDMKTONE,
		 ((1193190 / pitch) & 0xffff) |
		 (((unsigned long)duration*loudness/50)<<16));
		 break;
#endif
#if defined (WSCONS_SUPPORT)
            case WSCONS:
                 wsb.which = WSKBD_BELL_DOALL;
                 wsb.pitch = pitch;
                 wsb.period = duration;
                 wsb.volume = loudness;
                 ioctl(pInfo->fd, WSKBDIO_COMPLEXBELL, &wsb);
                 break;
#endif
	}
    }
}

#define ModifierSet(k) ((modifiers & (k)) == (k))

static
Bool SpecialKey(InputInfoPtr pInfo, int key, Bool down, int modifiers)
{
  KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;

  if(!pKbd->vtSwitchSupported)
      return FALSE;

  if ((ModifierSet(ControlMask | AltMask)) ||
      (ModifierSet(ControlMask | AltLangMask))) {
      if (VTSwitchEnabled && !xf86Info.vtSysreq) {
         switch (key) {
             case KEY_F1:
             case KEY_F2:
             case KEY_F3:
             case KEY_F4:
             case KEY_F5:
             case KEY_F6:
             case KEY_F7:
             case KEY_F8:
             case KEY_F9:
             case KEY_F10:
                  if (down) {
                    ioctl(xf86Info.consoleFd, VT_ACTIVATE, key - KEY_F1 + 1);
                    return TRUE;
                  }
             case KEY_F11:
             case KEY_F12:
                  if (down) {
                    ioctl(xf86Info.consoleFd, VT_ACTIVATE, key - KEY_F11 + 11);
                    return TRUE;
                  }
         }
      }
    }
#ifdef USE_VT_SYSREQ
    if (VTSwitchEnabled && xf86Info.vtSysreq) {
        switch (key) {
            case KEY_F1:
            case KEY_F2:
            case KEY_F3:
            case KEY_F4:
            case KEY_F5:
            case KEY_F6:
            case KEY_F7:
            case KEY_F8:
            case KEY_F9:
            case KEY_F10:
                 if (VTSysreqToggle && down) {
                     ioctl(xf86Info.consoleFd, VT_ACTIVATE, key - KEY_F1 + 1);
                     VTSysreqToggle = FALSE;
                     return TRUE;
                 }
                 break;
            case KEY_F11:
            case KEY_F12:
                 if (VTSysreqToggle && down) {
                     ioctl(xf86Info.consoleFd, VT_ACTIVATE, key - KEY_F11 + 11);
                     VTSysreqToggle = FALSE;
                     return TRUE;
                 }
                 break;
            /* Ignore these keys -- ie don't let them cancel an alt-sysreq */
            case KEY_Alt:
            case KEY_AltLang:
                 break;
            case KEY_SysReqest:
                 if ((ModifierSet(AltMask) || ModifierSet(AltLangMask)) && down)
                     VTSysreqToggle = TRUE;
                 break;
            default:
                 /*
                  * We only land here when Alt-SysReq is followed by a
                  * non-switching key.
                  */
                 if (VTSysreqToggle)
                     VTSysreqToggle = FALSE;
        }
    }
#endif /* USE_VT_SYSREQ */

    return FALSE;
}

static void
stdReadInput(InputInfoPtr pInfo)
{
    KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;
    unsigned char rBuf[64];
    int nBytes, i;
    if ((nBytes = read( pInfo->fd, (char *)rBuf, sizeof(rBuf))) > 0) {
       for (i = 0; i < nBytes; i++)
	   pKbd->PostEvent(pInfo, rBuf[i] & 0x7f,
                           rBuf[i] & 0x80 ? FALSE : TRUE);
       }
}

/* XXX this code assumes at most one Sun keyboard
   to do this right probably requires a devprivate or some such */

static int sun_fd;
static int sun_eat = 0;
static enum {
	 DECLICK_IDLE = 1,	/* idle */
	 DECLICK_WANTRESET,	/* want to send reset */
	 DECLICK_SENDRESET,	/* time to send reset */
	 DECLICK_WANTDECLICK,	/* want to send declick */
	 DECLICK_SENDDECLICK	/* time to send declick */
	 } sun_declick = DECLICK_IDLE;
static unsigned int sun_keysdown[4] = { 0, 0, 0, 0 };
static int sun_dbg_fd;

static void sun_dbg_out(const char *, ...)
	__attribute__((__format__(__printf__,1,2)));
static void sun_dbg_out(const char *fmt, ...)
{
 va_list ap;
 char *s;
 int n;

 if (sun_dbg_fd < 0) return;
 va_start(ap,fmt);
 n = vasprintf(&s,fmt,ap);
 va_end(ap);
 write(sun_dbg_fd,s,n);
 free(s);
}

static void sunReadInput(InputInfoPtr iip)
{
 KbdDevPtr kdp;
 unsigned char buf[64];
 int n;
 int i;
 int j;
 unsigned char k;

 kdp = (void *) iip->private;
 n = read(sun_fd,&buf[0],sizeof(buf));
 if (n <= 0) return;
 sun_dbg_out("sun read returned %d\n",n);
 for (i=0;i<n;i++)
  { sun_dbg_out("sun read i=%d n=%d eat=%d declick=%d\n",i,n,sun_eat,sun_declick);
    if (sun_eat > 0)
     { /* eat this byte */
       sun_eat --;
       continue;
     }
    k = buf[i];
    sun_dbg_out("k = %02x\n",k);
    switch (k)
     { case 0xff: /* reset - ID follows */
	  sun_eat = 1;
	  sun_declick = DECLICK_WANTDECLICK;
	  break;
       case 0xfe: /* layout follows */
	  sun_eat = 1;
	  break;
       case 0x7f: /* idle */
	  for (j=127;j>=0;j--)
	   { if (sun_keysdown[j>>5] & (1U << (j & 31)))
	      { sun_keysdown[j>>5] &= ~ (1U << (j & 31));
		kdp->PostEvent(iip,j,FALSE);
	      }
	   }
	  switch (sun_declick)
	   { case DECLICK_WANTRESET:
		sun_declick = DECLICK_SENDRESET;
		break;
	     case DECLICK_WANTDECLICK:
		sun_declick = DECLICK_SENDDECLICK;
		break;
	     default:
		break;
	   }
	  break;
       case 0x7e: /* keyboard detected an error - what can we do? */
	  break;
       case 0x00: case 0x80:
	  /* "can't happen" */
	  break;
       default:
	  k --;
	  if (k & 0x80)
	   { sun_keysdown[(k&0x7f)>>5] &= ~ (1U << (k & 31));
	     kdp->PostEvent(iip,k,FALSE);
	   }
	  else
	   { sun_keysdown[k>>5] |= 1U << (k & 31);
	     kdp->PostEvent(iip,k,TRUE);
	   }
	  break;
     }
  }
 switch (sun_declick)
  { case DECLICK_SENDRESET:
       sun_dbg_out("declick: resetting\n");
       write(sun_fd,"\1",1); /* \1 is "reset" */
       sun_declick = DECLICK_IDLE;
       break;
    case DECLICK_SENDDECLICK:
       sun_dbg_out("declicking\n");
       write(sun_fd,"\13",1); /* \13 is "keyclick off" */
       sun_declick = DECLICK_IDLE;
       break;
  }
}

void sunBell(
	InputInfoPtr iip __attribute__((__unused__)),
	int loudness,
	int pitch,
	int duration )
{
 static int lastdur = -1;
 static unsigned char obuf[122];
 static int dlen;

 if (!loudness || !pitch || !duration) return;
 if (duration > 1000) duration = 1000;
 if (duration != lastdur)
  { int i;
    lastdur = duration;
    dlen = ((duration * 120) + 2999) / 1000;
    if (dlen < 3) dlen = 3; else if (dlen > 122) dlen = 122;
    obuf[0] = '\2'; /* \2 is "bell on" */
    bzero(&obuf[1],dlen-2); /* \0 is ignored but takes time */
    obuf[dlen-1] = '\3'; /* \3 is "bell off" */
  }
 sun_dbg_out("bell, dlen=%d\n",dlen);
 write(sun_fd,&obuf,dlen);
}

/*
 * This replaces PostKbdEvent, from ../../input/keyboard/kbd.c, which
 *  contains a bunch of crap that's inappropriate for our use (and,
 *  arguably, inappropriate at all, but never mind).
 */
static void sunPostEvent(InputInfoPtr iip, unsigned int key, Bool down)
{
 KbdDevPtr pKbd;
 DeviceIntPtr device;
 KeyClassRec *keyc;
 KbdFeedbackClassRec *kbdfeed;
 KeySym *keysym;
 int keycode;
 static int lockkeys = 0;
#define CAPSFLAG    0x00000001
#define NUMFLAG     0x00000002
#define SCROLLFLAG  0x00000004
#define MODEFLAG    0x00000008
#define COMPOSEFLAG 0x00000010

 pKbd = (KbdDevPtr) iip->private;
 device = iip->dev;
 keyc = device->key;
 kbdfeed = device->kbdfeed;
 /* Disable any keyboard processing while in suspend */
 if (xf86inSuspend) return;
 keycode = key + MIN_KEYCODE;
 keysym = keyc->curKeySyms.map +
	  ( keyc->curKeySyms.mapWidth * 
	    (keycode - keyc->curKeySyms.minKeyCode) );
#ifdef XKB
 if (pKbd->noXkb)
#endif
  { /*
     * Filter redundant locking-key keystrokes.
     */
    if (down)
     { switch (keysym[0])
	{ case XK_Caps_Lock:
	     if (lockkeys & CAPSFLAG) return;
	     lockkeys |= CAPSFLAG;
	     break;
	  case XK_Num_Lock:
	     if (lockkeys & NUMFLAG) return;
	     lockkeys |= NUMFLAG;
	     break;
	  case XK_Scroll_Lock:
	     if (lockkeys & SCROLLFLAG) return;
	     lockkeys |= SCROLLFLAG;
	     break;
	}
       if (keysym[1] == XF86XK_ModeLock)
	{ if (lockkeys & MODEFLAG) return;
	  lockkeys |= MODEFLAG;
	}
     }
    else
     { switch (keysym[0])
	{ case XK_Caps_Lock:
	     lockkeys &= ~CAPSFLAG;
	     break;
	  case XK_Num_Lock:
	     lockkeys &= ~NUMFLAG;
	     break;
	  case XK_Scroll_Lock:
	     lockkeys &= ~SCROLLFLAG;
	     break;
	}
       if (keysym[1] == XF86XK_ModeLock) lockkeys &= ~MODEFLAG;
     }
    /*
     * LockKey special handling:
     * Ignore releases.  Toggle on & off on presses.
     * Don't deal with the Caps_Lock keysym directly, but check the
     *	lock modifier.
     *
     * XXX Is locking behaviour right if keysym[0] is an ordinary
     *	character but keysym[1] is XF86XK_ModeLock?  Or vice versa and
     *	Shift is down?
     */
    if ( (keyc->modifierMap[keycode] & LockMask) ||
	 (keysym[0] == XK_Num_Lock) ||
	 (keysym[0] == XK_Scroll_Lock) ||
	 (keysym[1] == XF86XK_ModeLock) )
     { if (! down) return;
       if (KeyPressed(keycode)) down = 0;
     }
  }
#if 0 /* XXX fix this to make autorepeat work? */
 /*
  * check for an autorepeat-event
  */
  if (down) {
      int num = keycode >> 3;
      int bit = 1 << (keycode & 7);
      if ((keyc->down[num] & bit) &&
          ((kbdfeed->ctrl.autoRepeat != AutoRepeatModeOn) ||
            keyc->modifierMap[keycode] ||
            !(kbdfeed->ctrl.autoRepeats[num] & bit)))
          return;
  }
#endif
 xf86PostKeyboardEvent(device,keycode,down);
}

static void sunSetupFd(int fd)
{
 struct termios tio;

 sun_fd = fd;
 if (tcgetattr(fd,&tio) < 0) return;
 cfmakeraw(&tio);
 tio.c_cc[VMIN] = 1;
 tio.c_cc[VTIME] = 0;
 tio.c_cflag |= CLOCAL;
 cfsetspeed(&tio,1200);
 tcsetattr(fd,TCSANOW,&tio);
 sun_declick = DECLICK_WANTRESET;
 sun_dbg_fd = -1; /* open("/tmp/sun-kbd-dbg",O_WRONLY|O_APPEND,0); */
 sun_dbg_out("startup\n");
}

#ifdef WSCONS_SUPPORT
static void
WSReadInput(InputInfoPtr pInfo)
{
    KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;
    struct wscons_event events[64];
    int n, i;
    if ((n = read( pInfo->fd, events, sizeof(events))) > 0) {
        n /=  sizeof(struct wscons_event);
        for (i = 0; i < n; i++)
           pKbd->PostEvent(pInfo, events[i].value,
	             events[i].type == WSCONS_EVENT_KEY_DOWN ? TRUE : FALSE);
	}
}
#endif

#ifdef WSCONS_SUPPORT
static void
printWsType(char *type, char *devname)
{
    xf86Msg(X_PROBED, "%s: Keyboard type: %s\n", type, devname); 
}
#endif

static Bool
OpenKeyboard(InputInfoPtr pInfo)
{
    KbdDevPtr pKbd = (KbdDevPtr) pInfo->private;
    int i;
    KbdProtocolId prot = PROT_UNKNOWN_KBD;
    char *s;
 char *protname;

    protname = xf86SetStrOption(pInfo->options, "Protocol", NULL);
    for (i = 0; protocols[i].name; i++) {
        if (xf86NameCmp(protname, protocols[i].name) == 0) {
           prot = protocols[i].id;
           break;
        }
    }

    switch (prot) {
    	case PROT_STD:
           pInfo->read_input = stdReadInput;
           break;
    	case PROT_SUN:
           pInfo->read_input = &sunReadInput;
	   pKbd->Bell = &sunBell;
	   pKbd->PostEvent = &sunPostEvent;
           break;
#ifdef WSCONS_SUPPORT
        case PROT_WSCONS:
           pInfo->read_input = WSReadInput;
           break;
#endif
        default:
           xf86Msg(X_ERROR,"\"%s\" is not a valid keyboard protocol name\n", protname);
           xfree(protname);
           return FALSE;
    }
    xf86Msg(X_CONFIG, "%s: Protocol: %s\n", pInfo->name, protname);
    xfree(protname);

    s = xf86SetStrOption(pInfo->options, "Device", NULL);
    if (s == NULL) {
       if ((prot == PROT_WSCONS) || (prot == PROT_SUN)) {
           xf86Msg(X_ERROR,"A \"device\" option is required with"
                                  " the \"%s\" keyboard protocol\n",protname);
	   xfree(protname);
           return FALSE;
       } else {
           pInfo->fd = xf86Info.consoleFd;
           pKbd->isConsole = TRUE;
           pKbd->consType = xf86Info.consType;
       }
    } else {
       pInfo->fd = open(s, ((prot==PROT_SUN)?O_RDWR:O_RDONLY) | O_NONBLOCK | O_EXCL, 0);
       if (pInfo->fd == -1) {
           xf86Msg(X_ERROR, "%s: cannot open \"%s\"\n", pInfo->name, s);
           xfree(s);
	   xfree(protname);
           return FALSE;
       }
       pKbd->isConsole = FALSE;
       /* XXX What is consType here? */
       pKbd->consType = SYSCONS;
       xfree(s);
       switch (prot) {
	   case PROT_SUN:
	      sunSetupFd(pInfo->fd);
	      break;
       }
    }

    xfree(protname);

#if defined (SYSCONS_SUPPORT) || defined (PCVT_SUPPORT)
    if (pKbd->isConsole &&
        ((pKbd->consType == SYSCONS) || (pKbd->consType == PCVT)))
        pKbd->vtSwitchSupported = TRUE;
#endif

#ifdef WSCONS_SUPPORT
    if( prot == PROT_WSCONS) {
       pKbd->consType = WSCONS;
       /* Find out keyboard type */
       if (ioctl(pInfo->fd, WSKBDIO_GTYPE, &(pKbd->wsKbdType)) == -1) {
           xf86Msg(X_ERROR, "%s: cannot get keyboard type", pInfo->name);
           close(pInfo->fd);
           return FALSE;
       }
       switch (pKbd->wsKbdType) {
           case WSKBD_TYPE_PC_XT:
               printWsType("XT", pInfo->name);
               break;
           case WSKBD_TYPE_PC_AT:
               printWsType("AT", pInfo->name);
               break;
           case WSKBD_TYPE_USB:
               printWsType("USB", pInfo->name);
               break;
#ifdef WSKBD_TYPE_ADB
           case WSKBD_TYPE_ADB:
               printWsType("ADB", pInfo->name);
               break;
#endif
#ifdef WSKBD_TYPE_SUN
           case WSKBD_TYPE_SUN:
               printWsType("Sun", pInfo->name);
               break;
#endif
           default:
	       printWsType("Unknown", pInfo->name);
	       break;
       }
    }
#endif
    return TRUE;
}

Bool
xf86OSKbdPreInit(InputInfoPtr pInfo)
{
    KbdDevPtr pKbd = pInfo->private;

    pKbd->KbdInit	= KbdInit;
    pKbd->KbdOn		= KbdOn;
    pKbd->KbdOff	= KbdOff;
    pKbd->Bell		= SoundBell;
    pKbd->SetLeds	= SetKbdLeds;
    pKbd->GetLeds	= GetKbdLeds;
    pKbd->SetKbdRepeat	= SetKbdRepeat;
    pKbd->KbdGetMapping	= KbdGetMapping;
    pKbd->SpecialKey	= SpecialKey;

    pKbd->RemapScanCode = NULL;
    pKbd->GetSpecialKey = NULL;

    pKbd->OpenKeyboard = OpenKeyboard;
    pKbd->vtSwitchSupported = FALSE;
    pKbd->CustomKeycodes = FALSE;
    
    pKbd->private = xcalloc(sizeof(BsdKbdPrivRec), 1);
    if (pKbd->private == NULL) {
       xf86Msg(X_ERROR,"can't allocate keyboard OS private data\n");
       return FALSE;
    }
    return TRUE;
}
