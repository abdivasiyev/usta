/* Desktop notifications on macOS through UNUserNotificationCenter, the
 * one macOS still shows; GLib's goes through NSUserNotificationCenter,
 * which macOS now drops. Called through the Objective-C runtime, so the
 * file stays C. They show only for an app bundle, as usta.app. */

#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>
#include <objc/message.h>
#include <objc/runtime.h>
#include <stdint.h>

typedef void (^usta_present)(uintptr_t);

/* Shown even while usta is the app in front: a banner, in the list,
 * with its sound. */
static void will_present(id self, SEL cmd, id center, id note, usta_present done)
{
  (void) self; (void) cmd; (void) center; (void) note;
  done((1 << 4) | (1 << 3) | (1 << 1));
}

static id string(const char *text)
{
  return ((id (*)(Class, SEL, const char *)) objc_msgSend)(objc_getClass("NSString"), sel_registerName("stringWithUTF8String:"), text);
}

/* Whether usta runs as an app bundle, the only way macOS shows its
 * notifications. */
int usta_in_bundle(void)
{
  CFStringRef id = CFBundleGetIdentifier(CFBundleGetMainBundle());
  return id != NULL;
}

static id center(void)
{
  static id shared = NULL;
  if (shared != NULL)
    return shared;
  Class kind = objc_getClass("UNUserNotificationCenter");
  if (kind == NULL || !usta_in_bundle())
    return NULL;
  shared = ((id (*)(Class, SEL)) objc_msgSend)(kind, sel_registerName("currentNotificationCenter"));
  /* A delegate that lets them show while usta is in front. */
  Class delegate = objc_allocateClassPair(objc_getClass("NSObject"), "UstaNotificationDelegate", 0);
  if (delegate != NULL) {
    class_addMethod(delegate, sel_registerName("userNotificationCenter:willPresentNotification:withCompletionHandler:"), (IMP) will_present, "v@:@@@?");
    objc_registerClassPair(delegate);
  } else {
    delegate = objc_getClass("UstaNotificationDelegate");
  }
  id listener = ((id (*)(id, SEL)) objc_msgSend)(((id (*)(Class, SEL)) objc_msgSend)(delegate, sel_registerName("alloc")), sel_registerName("init"));
  ((void (*)(id, SEL, id)) objc_msgSend)(shared, sel_registerName("setDelegate:"), listener);
  /* Asks once; macOS remembers the answer. Alert, sound and badge. */
  void (^answered)(BOOL, id) = ^(BOOL granted, id error) { (void) granted; (void) error; };
  ((void (*)(id, SEL, uintptr_t, void *)) objc_msgSend)(shared, sel_registerName("requestAuthorizationWithOptions:completionHandler:"), (1 << 2) | (1 << 1) | (1 << 0), (void *) answered);
  return shared;
}

/* Asks for permission early, so the first notification is not lost to
 * the question. */
void usta_notifications_start(void)
{
  (void) center();
}

/* One notification; a newer one with the same key replaces it. */
void usta_notify(const char *key, const char *title, const char *body)
{
  id c = center();
  if (c == NULL)
    return;
  id content = ((id (*)(id, SEL)) objc_msgSend)(((id (*)(Class, SEL)) objc_msgSend)(objc_getClass("UNMutableNotificationContent"), sel_registerName("alloc")), sel_registerName("init"));
  ((void (*)(id, SEL, id)) objc_msgSend)(content, sel_registerName("setTitle:"), string(title));
  ((void (*)(id, SEL, id)) objc_msgSend)(content, sel_registerName("setBody:"), string(body));
  id request = ((id (*)(Class, SEL, id, id, id)) objc_msgSend)(objc_getClass("UNNotificationRequest"), sel_registerName("requestWithIdentifier:content:trigger:"), string(key), content, NULL);
  ((void (*)(id, SEL, id, void *)) objc_msgSend)(c, sel_registerName("addNotificationRequest:withCompletionHandler:"), request, NULL);
}

#else

int usta_in_bundle(void) { return 0; }
void usta_notifications_start(void) {}
void usta_notify(const char *key, const char *title, const char *body)
{
  (void) key; (void) title; (void) body;
}

#endif
