/* Dialogs on macOS. GTK makes a dialog its own NSWindow, which macOS puts
 * on a full-screen space of its own while the main window is full
 * screen. Made a child window of usta's window and allowed beside a
 * full-screen one, it opens over that window instead, as a sheet would. */

#ifdef __APPLE__
#include <objc/message.h>
#include <objc/runtime.h>
#include <stdint.h>

/* GTK's own: the NSWindow behind a surface. */
extern void *gdk_macos_surface_get_native_window(void *surface);

/* NSWindowCollectionBehaviorFullScreenPrimary and ...Auxiliary,
 * NSWindowTabbingModeDisallowed and NSWindowAbove. */
enum {
  usta_fullscreen_primary = 1 << 7,
  usta_fullscreen_auxiliary = 1 << 8,
  usta_tabbing_disallowed = 2,
  usta_window_above = 1
};

/* Before the dialog first shows, when macOS picks its space: GTK lets
 * every window go full screen itself, so beside a full-screen window a new
 * one would go full screen too, or into a tab. A dialog may only sit
 * beside one. */
void usta_prepare_dialog(void *dialog_surface)
{
  id dialog = (id) gdk_macos_surface_get_native_window(dialog_surface);
  if (dialog == NULL)
    return;
  uintptr_t behavior = ((uintptr_t (*)(id, SEL)) objc_msgSend)(dialog, sel_registerName("collectionBehavior"));
  behavior = (behavior & ~(uintptr_t) usta_fullscreen_primary) | usta_fullscreen_auxiliary;
  ((void (*)(id, SEL, uintptr_t)) objc_msgSend)(dialog, sel_registerName("setCollectionBehavior:"), behavior);
  ((void (*)(id, SEL, intptr_t)) objc_msgSend)(dialog, sel_registerName("setTabbingMode:"), usta_tabbing_disallowed);
}

void usta_attach_dialog(void *dialog_surface, void *parent_surface)
{
  id dialog = (id) gdk_macos_surface_get_native_window(dialog_surface);
  id parent = (id) gdk_macos_surface_get_native_window(parent_surface);
  if (dialog == NULL || parent == NULL)
    return;
  usta_prepare_dialog(dialog_surface);
  ((void (*)(id, SEL, id, intptr_t)) objc_msgSend)(parent, sel_registerName("addChildWindow:ordered:"), dialog, usta_window_above);
}

void usta_detach_dialog(void *dialog_surface, void *parent_surface)
{
  id dialog = (id) gdk_macos_surface_get_native_window(dialog_surface);
  id parent = (id) gdk_macos_surface_get_native_window(parent_surface);
  if (dialog != NULL && parent != NULL)
    ((void (*)(id, SEL, id)) objc_msgSend)(parent, sel_registerName("removeChildWindow:"), dialog);
}

#else

void usta_prepare_dialog(void *dialog_surface)
{
  (void) dialog_surface;
}

void usta_attach_dialog(void *dialog_surface, void *parent_surface)
{
  (void) dialog_surface; (void) parent_surface;
}

void usta_detach_dialog(void *dialog_surface, void *parent_surface)
{
  (void) dialog_surface; (void) parent_surface;
}

#endif
