/* Whether macOS shows its dark appearance. GTK does not read it on macOS,
 * so usta asks CoreFoundation for the AppleInterfaceStyle preference,
 * which is "Dark" while the system is dark (in Auto mode too). */

#ifdef __APPLE__
#include <CoreFoundation/CoreFoundation.h>

int usta_system_dark(void)
{
  /* Another process changes the preference; read it afresh. */
  CFPreferencesAppSynchronize(kCFPreferencesAnyApplication);
  CFPropertyListRef style = CFPreferencesCopyAppValue(CFSTR("AppleInterfaceStyle"), kCFPreferencesAnyApplication);
  int dark = style != NULL
    && CFGetTypeID(style) == CFStringGetTypeID()
    && CFStringCompare((CFStringRef) style, CFSTR("Dark"), kCFCompareCaseInsensitive) == kCFCompareEqualTo;
  if (style != NULL)
    CFRelease(style);
  return dark;
}

#else

int usta_system_dark(void)
{
  return 0;
}

#endif
