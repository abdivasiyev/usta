/* Links macOS opens with usta: the usta:// scheme the bundle's Info.plist
 * claims arrives as an Apple Event, which GTK does not pass on (it hands
 * only files to GApplication's open). usta installs its own handler for
 * it, before the app runs, so the link that launched usta is caught too. */

#ifdef __APPLE__
#include <CoreServices/CoreServices.h>
#include <stdlib.h>
#include <CoreFoundation/CoreFoundation.h>

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

/* The browsers: the apps that open https links. Each string and the list
 * are malloc'd; the caller frees them. */
static char *copy_string(CFStringRef text)
{
  if (text == NULL)
    return NULL;
  CFIndex size = CFStringGetMaximumSizeForEncoding(CFStringGetLength(text), kCFStringEncodingUTF8) + 1;
  char *out = malloc(size);
  if (out != NULL && !CFStringGetCString(text, out, size, kCFStringEncodingUTF8)) {
    free(out);
    out = NULL;
  }
  return out;
}

int usta_browsers(char ***out)
{
  *out = NULL;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  CFArrayRef ids = LSCopyAllHandlersForURLScheme(CFSTR("https"));
#pragma clang diagnostic pop
  if (ids == NULL)
    return 0;
  CFIndex n = CFArrayGetCount(ids);
  char **list = calloc(n, sizeof(char *));
  int found = 0;
  for (CFIndex i = 0; list != NULL && i < n; i++) {
    char *id = copy_string(CFArrayGetValueAtIndex(ids, i));
    if (id != NULL)
      list[found++] = id;
  }
  CFRelease(ids);
  *out = list;
  return found;
}

/* The app's name as Finder shows it, "Zen Browser" of Zen Browser.app. */
char *usta_app_name(const char *bundle)
{
  CFStringRef id = CFStringCreateWithCString(NULL, bundle, kCFStringEncodingUTF8);
  CFArrayRef urls = LSCopyApplicationURLsForBundleIdentifier(id, NULL);
  CFRelease(id);
  if (urls == NULL)
    return NULL;
  char *name = NULL;
  if (CFArrayGetCount(urls) > 0) {
    CFURLRef url = CFArrayGetValueAtIndex(urls, 0);
    CFURLRef bare = CFURLCreateCopyDeletingPathExtension(NULL, url);
    CFStringRef last = CFURLCopyLastPathComponent(bare);
    name = copy_string(last);
    if (last != NULL)
      CFRelease(last);
    CFRelease(bare);
  }
  CFRelease(urls);
  return name;
}

/* The app that opens https links now. */
char *usta_default_browser(void)
{
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  CFStringRef id = LSCopyDefaultHandlerForURLScheme(CFSTR("https"));
#pragma clang diagnostic pop
  char *out = copy_string(id);
  if (id != NULL)
    CFRelease(id);
  return out;
}

/* Asks macOS to open http and https links with the app this runs as; the
 * system asks the user to confirm. */
void usta_make_default_browser(void)
{
  CFStringRef self = CFBundleGetIdentifier(CFBundleGetMainBundle());
  if (self == NULL)
    return;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
  LSSetDefaultHandlerForURLScheme(CFSTR("http"), self);
  LSSetDefaultHandlerForURLScheme(CFSTR("https"), self);
#pragma clang diagnostic pop
}

#else

/* Elsewhere a link comes as an argument, which usta reads itself. */
void usta_listen_urls(void (*each)(const char *))
{
  (void) each;
}

int usta_browsers(char ***out)
{
  *out = 0;
  return 0;
}

char *usta_app_name(const char *bundle)
{
  (void) bundle;
  return 0;
}

char *usta_default_browser(void)
{
  return 0;
}

void usta_make_default_browser(void)
{
}

#endif
