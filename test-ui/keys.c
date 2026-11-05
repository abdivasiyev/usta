/* Sends a key to a key controller as GTK would, and says whether a
 * handler took it. g_signal_emitv cannot be given the boolean the
 * signal returns from Haskell, so the tests use this. GLib's one call
 * is declared here: GTK's headers would need pkg-config, which the
 * build's link check on Linux then trips over. */
typedef int gboolean;

extern void g_signal_emit_by_name(void *instance, const char *detailed_signal, ...);

gboolean usta_test_press(void *controller, unsigned int keyval, unsigned int state)
{
  gboolean handled = 0;
  g_signal_emit_by_name(controller, "key-pressed", keyval, 0u, state, &handled);
  return handled;
}
