/* Sends a key to a key controller as GTK would, and says whether a
 * handler took it. g_signal_emitv cannot be given the boolean the
 * signal returns from Haskell, so the tests use this. */
#include <gtk/gtk.h>

gboolean usta_test_press(GtkEventControllerKey *controller, guint keyval, GdkModifierType state)
{
  gboolean handled = FALSE;
  g_signal_emit_by_name(controller, "key-pressed", keyval, 0, state, &handled);
  return handled;
}
