/*
 * The system keyring, through each platform's own API: Keychain Services
 * on macOS, libsecret (the Secret Service) elsewhere. No process is started
 * and nothing is parsed, so a secret is never on a command line.
 *
 * Every function returns USTA_KEYRING_OK, USTA_KEYRING_NOT_FOUND, or
 * USTA_KEYRING_ERROR with *error set to a message (never the secret) that
 * the caller frees with usta_keyring_free. A secret from usta_keyring_load
 * is freed the same way, which zeroes it first.
 */

#include <stdlib.h>
#include <string.h>

#define USTA_KEYRING_OK 0
#define USTA_KEYRING_NOT_FOUND 1
#define USTA_KEYRING_ERROR 2

/* Zeroes, then frees; the volatile write is not optimised away. */
void usta_keyring_free(char *text)
{
  if (text == NULL)
    return;
  volatile char *p = text;
  while (*p != '\0')
    *p++ = '\0';
  free(text);
}

#ifdef __APPLE__

#include <CoreFoundation/CoreFoundation.h>
#include <Security/Security.h>

/* Items are generic passwords under this service, one per account. */
static const char *service = "usta";

static char *copy_message(OSStatus status)
{
  CFStringRef message = SecCopyErrorMessageString(status, NULL);
  char buffer[256] = "keychain error";
  if (message != NULL) {
    CFStringGetCString(message, buffer, sizeof buffer, kCFStringEncodingUTF8);
    CFRelease(message);
  }
  return strdup(buffer);
}

/* The query naming one account's item; the caller releases it. */
static CFMutableDictionaryRef query_for(const char *account)
{
  CFMutableDictionaryRef query = CFDictionaryCreateMutable(
      NULL, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
  CFStringRef service_string = CFStringCreateWithCString(NULL, service, kCFStringEncodingUTF8);
  CFStringRef account_string = CFStringCreateWithCString(NULL, account, kCFStringEncodingUTF8);
  CFDictionarySetValue(query, kSecClass, kSecClassGenericPassword);
  CFDictionarySetValue(query, kSecAttrService, service_string);
  CFDictionarySetValue(query, kSecAttrAccount, account_string);
  CFRelease(service_string);
  CFRelease(account_string);
  return query;
}

int usta_keyring_save(const char *account, const char *label, const char *secret, char **error)
{
  CFDataRef data = CFDataCreate(NULL, (const UInt8 *)secret, (CFIndex)strlen(secret));
  CFMutableDictionaryRef item = query_for(account);
  CFStringRef label_string = CFStringCreateWithCString(NULL, label, kCFStringEncodingUTF8);
  CFDictionarySetValue(item, kSecAttrLabel, label_string);
  CFDictionarySetValue(item, kSecValueData, data);
  OSStatus status = SecItemAdd(item, NULL);
  if (status == errSecDuplicateItem) {
    /* Updating keeps the item, and with it who may read it. */
    CFMutableDictionaryRef query = query_for(account);
    CFMutableDictionaryRef changes = CFDictionaryCreateMutable(
        NULL, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
    CFDictionarySetValue(changes, kSecValueData, data);
    status = SecItemUpdate(query, changes);
    CFRelease(changes);
    CFRelease(query);
  }
  CFRelease(label_string);
  CFRelease(item);
  CFRelease(data);
  if (status == errSecSuccess)
    return USTA_KEYRING_OK;
  *error = copy_message(status);
  return USTA_KEYRING_ERROR;
}

int usta_keyring_load(const char *account, char **secret, char **error)
{
  CFMutableDictionaryRef query = query_for(account);
  CFDictionarySetValue(query, kSecReturnData, kCFBooleanTrue);
  CFDictionarySetValue(query, kSecMatchLimit, kSecMatchLimitOne);
  CFTypeRef result = NULL;
  OSStatus status = SecItemCopyMatching(query, &result);
  CFRelease(query);
  if (status == errSecItemNotFound)
    return USTA_KEYRING_NOT_FOUND;
  if (status != errSecSuccess) {
    *error = copy_message(status);
    return USTA_KEYRING_ERROR;
  }
  CFDataRef data = (CFDataRef)result;
  CFIndex length = CFDataGetLength(data);
  *secret = malloc((size_t)length + 1);
  memcpy(*secret, CFDataGetBytePtr(data), (size_t)length);
  (*secret)[length] = '\0';
  CFRelease(result);
  return USTA_KEYRING_OK;
}

int usta_keyring_forget(const char *account, char **error)
{
  CFMutableDictionaryRef query = query_for(account);
  OSStatus status = SecItemDelete(query);
  CFRelease(query);
  if (status == errSecItemNotFound)
    return USTA_KEYRING_NOT_FOUND;
  if (status != errSecSuccess) {
    *error = copy_message(status);
    return USTA_KEYRING_ERROR;
  }
  return USTA_KEYRING_OK;
}

#else

#include <libsecret/secret.h>

/* One item per account, found by its "account" attribute. */
static const SecretSchema schema = {
  .name = "io.github.abdivasiyev.Usta.Token",
  .flags = SECRET_SCHEMA_NONE,
  .attributes = {
    {"account", SECRET_SCHEMA_ATTRIBUTE_STRING},
    {NULL, 0},
  },
};

static int failed(GError *e, char **error)
{
  *error = strdup(e->message);
  g_error_free(e);
  return USTA_KEYRING_ERROR;
}

int usta_keyring_save(const char *account, const char *label, const char *secret, char **error)
{
  GError *e = NULL;
  secret_password_store_sync(&schema, SECRET_COLLECTION_DEFAULT, label, secret, NULL, &e,
                             "account", account, NULL);
  return e == NULL ? USTA_KEYRING_OK : failed(e, error);
}

int usta_keyring_load(const char *account, char **secret, char **error)
{
  GError *e = NULL;
  gchar *found = secret_password_lookup_sync(&schema, NULL, &e, "account", account, NULL);
  if (e != NULL)
    return failed(e, error);
  if (found == NULL)
    return USTA_KEYRING_NOT_FOUND;
  *secret = strdup(found);
  secret_password_free(found);
  return USTA_KEYRING_OK;
}

int usta_keyring_forget(const char *account, char **error)
{
  GError *e = NULL;
  gboolean removed = secret_password_clear_sync(&schema, NULL, &e, "account", account, NULL);
  if (e != NULL)
    return failed(e, error);
  return removed ? USTA_KEYRING_OK : USTA_KEYRING_NOT_FOUND;
}

#endif
