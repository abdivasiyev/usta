/* Links macOS opens with usta: the usta:// scheme the bundle's Info.plist
 * claims arrives as an Apple Event, which GTK does not pass on (it hands
 * only files to GApplication's open). usta installs its own handler for
 * it, before the app runs, so the link that launched usta is caught too. */

#ifdef __APPLE__
#include <CoreServices/CoreServices.h>
#include <stdlib.h>

/* Carbon's names for the event a URL opens with; Carbon itself is not
 * needed for them. */
enum { usta_url_class = 'GURL', usta_url_id = 'GURL' };

typedef void (*usta_url_callback)(const char *);

static usta_url_callback callback = NULL;

static OSErr handle_url(const AppleEvent *event, AppleEvent *reply, SRefCon refcon)
{
  (void) reply;
  (void) refcon;
  AEDesc desc;
  if (AEGetParamDesc(event, keyDirectObject, typeUTF8Text, &desc) != noErr)
    return errAEDescNotFound;
  Size size = AEGetDescDataSize(&desc);
  char *url = malloc(size + 1);
  if (url != NULL) {
    AEGetDescData(&desc, url, size);
    url[size] = '\0';
    if (callback != NULL)
      callback(url);
    free(url);
  }
  AEDisposeDesc(&desc);
  return noErr;
}

void usta_listen_urls(usta_url_callback each)
{
  callback = each;
  AEInstallEventHandler(usta_url_class, usta_url_id, NewAEEventHandlerUPP(handle_url), 0, false);
}

#else

/* Elsewhere a link comes as an argument, which usta reads itself. */
void usta_listen_urls(void (*each)(const char *))
{
  (void) each;
}

#endif
