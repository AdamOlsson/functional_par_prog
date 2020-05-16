// Headers

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#define CL_TARGET_OPENCL_VERSION 120
#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#ifdef __APPLE__
#define CL_SILENCE_DEPRECATION
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif


// Initialisation

int futhark_get_num_sizes(void);
const char *futhark_get_size_name(int);
const char *futhark_get_size_class(int);
struct futhark_context_config ;
struct futhark_context_config *futhark_context_config_new(void);
void futhark_context_config_free(struct futhark_context_config *cfg);
void futhark_context_config_add_build_option(struct futhark_context_config *cfg,
                                             const char *opt);
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int flag);
void futhark_context_config_set_profiling(struct futhark_context_config *cfg,
                                          int flag);
void futhark_context_config_set_logging(struct futhark_context_config *cfg,
                                        int flag);
void futhark_context_config_set_device(struct futhark_context_config *cfg, const
                                       char *s);
void futhark_context_config_set_platform(struct futhark_context_config *cfg,
                                         const char *s);
void
futhark_context_config_select_device_interactively(struct futhark_context_config *cfg);
void futhark_context_config_dump_program_to(struct futhark_context_config *cfg,
                                            const char *path);
void
futhark_context_config_load_program_from(struct futhark_context_config *cfg,
                                         const char *path);
void futhark_context_config_dump_binary_to(struct futhark_context_config *cfg,
                                           const char *path);
void futhark_context_config_load_binary_from(struct futhark_context_config *cfg,
                                             const char *path);
void
futhark_context_config_set_default_group_size(struct futhark_context_config *cfg,
                                              int size);
void
futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg,
                                              int num);
void
futhark_context_config_set_default_tile_size(struct futhark_context_config *cfg,
                                             int num);
void
futhark_context_config_set_default_threshold(struct futhark_context_config *cfg,
                                             int num);
int futhark_context_config_set_size(struct futhark_context_config *cfg, const
                                    char *size_name, size_t size_value);
struct futhark_context ;
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg);
struct futhark_context
*futhark_context_new_with_command_queue(struct futhark_context_config *cfg,
                                        cl_command_queue queue);
void futhark_context_free(struct futhark_context *ctx);
int futhark_context_sync(struct futhark_context *ctx);
char *futhark_context_get_error(struct futhark_context *ctx);
void futhark_context_pause_profiling(struct futhark_context *ctx);
void futhark_context_unpause_profiling(struct futhark_context *ctx);
int futhark_context_clear_caches(struct futhark_context *ctx);
cl_command_queue futhark_context_get_command_queue(struct futhark_context *ctx);

// Arrays

struct futhark_bool_1d ;
struct futhark_bool_1d *futhark_new_bool_1d(struct futhark_context *ctx,
                                            bool *data, int64_t dim0);
struct futhark_bool_1d *futhark_new_raw_bool_1d(struct futhark_context *ctx,
                                                cl_mem data, int offset,
                                                int64_t dim0);
int futhark_free_bool_1d(struct futhark_context *ctx,
                         struct futhark_bool_1d *arr);
int futhark_values_bool_1d(struct futhark_context *ctx,
                           struct futhark_bool_1d *arr, bool *data);
cl_mem futhark_values_raw_bool_1d(struct futhark_context *ctx,
                                  struct futhark_bool_1d *arr);
int64_t *futhark_shape_bool_1d(struct futhark_context *ctx,
                               struct futhark_bool_1d *arr);
struct futhark_i32_1d ;
struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx,
                                          int32_t *data, int64_t dim0);
struct futhark_i32_1d *futhark_new_raw_i32_1d(struct futhark_context *ctx,
                                              cl_mem data, int offset,
                                              int64_t dim0);
int futhark_free_i32_1d(struct futhark_context *ctx,
                        struct futhark_i32_1d *arr);
int futhark_values_i32_1d(struct futhark_context *ctx,
                          struct futhark_i32_1d *arr, int32_t *data);
cl_mem futhark_values_raw_i32_1d(struct futhark_context *ctx,
                                 struct futhark_i32_1d *arr);
int64_t *futhark_shape_i32_1d(struct futhark_context *ctx,
                              struct futhark_i32_1d *arr);

// Opaque values


// Entry points

int futhark_entry_main(struct futhark_context *ctx,
                       struct futhark_i32_1d **out0, const
                       struct futhark_i32_1d *in0, const
                       struct futhark_bool_1d *in1);

// Miscellaneous

void futhark_debugging_report(struct futhark_context *ctx);
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <stdint.h>
#undef NDEBUG
#include <assert.h>
// Start of panic.h.

#include <stdarg.h>

static const char *fut_progname;

static void futhark_panic(int eval, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
        fprintf(stderr, "%s: ", fut_progname);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
        exit(eval);
}

/* For generating arbitrary-sized error messages.  It is the callers
   responsibility to free the buffer at some point. */
static char* msgprintf(const char *s, ...) {
  va_list vl;
  va_start(vl, s);
  size_t needed = 1 + (size_t)vsnprintf(NULL, 0, s, vl);
  char *buffer = (char*) malloc(needed);
  va_start(vl, s); /* Must re-init. */
  vsnprintf(buffer, needed, s, vl);
  return buffer;
}

// End of panic.h.

// Start of timing.h.

// The function get_wall_time() returns the wall time in microseconds
// (with an unspecified offset).

#ifdef _WIN32

#include <windows.h>

static int64_t get_wall_time(void) {
  LARGE_INTEGER time,freq;
  assert(QueryPerformanceFrequency(&freq));
  assert(QueryPerformanceCounter(&time));
  return ((double)time.QuadPart / freq.QuadPart) * 1000000;
}

#else
/* Assuming POSIX */

#include <time.h>
#include <sys/time.h>

static int64_t get_wall_time(void) {
  struct timeval time;
  assert(gettimeofday(&time,NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

#endif

// End of timing.h.

#include <string.h>
#include <inttypes.h>
#include <errno.h>
#include <ctype.h>
#include <errno.h>
#include <getopt.h>
// Start of values.h.

//// Text I/O

typedef int (*writer)(FILE*, void*);
typedef int (*bin_reader)(void*);
typedef int (*str_reader)(const char *, void*);

struct array_reader {
  char* elems;
  int64_t n_elems_space;
  int64_t elem_size;
  int64_t n_elems_used;
  int64_t *shape;
  str_reader elem_reader;
};

static void skipspaces() {
  int c;
  do {
    c = getchar();
  } while (isspace(c));

  if (c != EOF) {
    ungetc(c, stdin);
  }
}

static int constituent(char c) {
  return isalnum(c) || c == '.' || c == '-' || c == '+' || c == '_';
}

// Produces an empty token only on EOF.
static void next_token(char *buf, int bufsize) {
 start:
  skipspaces();

  int i = 0;
  while (i < bufsize) {
    int c = getchar();
    buf[i] = (char)c;

    if (c == EOF) {
      buf[i] = 0;
      return;
    } else if (c == '-' && i == 1 && buf[0] == '-') {
      // Line comment, so skip to end of line and start over.
      for (; c != '\n' && c != EOF; c = getchar());
      goto start;
    } else if (!constituent((char)c)) {
      if (i == 0) {
        // We permit single-character tokens that are not
        // constituents; this lets things like ']' and ',' be
        // tokens.
        buf[i+1] = 0;
        return;
      } else {
        ungetc(c, stdin);
        buf[i] = 0;
        return;
      }
    }

    i++;
  }

  buf[bufsize-1] = 0;
}

static int next_token_is(char *buf, int bufsize, const char* expected) {
  next_token(buf, bufsize);
  return strcmp(buf, expected) == 0;
}

static void remove_underscores(char *buf) {
  char *w = buf;

  for (char *r = buf; *r; r++) {
    if (*r != '_') {
      *w++ = *r;
    }
  }

  *w++ = 0;
}

static int read_str_elem(char *buf, struct array_reader *reader) {
  int ret;
  if (reader->n_elems_used == reader->n_elems_space) {
    reader->n_elems_space *= 2;
    reader->elems = (char*) realloc(reader->elems,
                                    (size_t)(reader->n_elems_space * reader->elem_size));
  }

  ret = reader->elem_reader(buf, reader->elems + reader->n_elems_used * reader->elem_size);

  if (ret == 0) {
    reader->n_elems_used++;
  }

  return ret;
}

static int read_str_array_elems(char *buf, int bufsize,
                                struct array_reader *reader, int64_t dims) {
  int ret;
  int first = 1;
  char *knows_dimsize = (char*) calloc((size_t)dims, sizeof(char));
  int cur_dim = dims-1;
  int64_t *elems_read_in_dim = (int64_t*) calloc((size_t)dims, sizeof(int64_t));

  while (1) {
    next_token(buf, bufsize);

    if (strcmp(buf, "]") == 0) {
      if (knows_dimsize[cur_dim]) {
        if (reader->shape[cur_dim] != elems_read_in_dim[cur_dim]) {
          ret = 1;
          break;
        }
      } else {
        knows_dimsize[cur_dim] = 1;
        reader->shape[cur_dim] = elems_read_in_dim[cur_dim];
      }
      if (cur_dim == 0) {
        ret = 0;
        break;
      } else {
        cur_dim--;
        elems_read_in_dim[cur_dim]++;
      }
    } else if (strcmp(buf, ",") == 0) {
      next_token(buf, bufsize);
      if (strcmp(buf, "[") == 0) {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        first = 1;
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else if (cur_dim == dims - 1) {
        ret = read_str_elem(buf, reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
      } else {
        ret = 1;
        break;
      }
    } else if (strlen(buf) == 0) {
      // EOF
      ret = 1;
      break;
    } else if (first) {
      if (strcmp(buf, "[") == 0) {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else {
        ret = read_str_elem(buf, reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
        first = 0;
      }
    } else {
      ret = 1;
      break;
    }
  }

  free(knows_dimsize);
  free(elems_read_in_dim);
  return ret;
}

static int read_str_empty_array(char *buf, int bufsize,
                                const char *type_name, int64_t *shape, int64_t dims) {
  if (strlen(buf) == 0) {
    // EOF
    return 1;
  }

  if (strcmp(buf, "empty") != 0) {
    return 1;
  }

  if (!next_token_is(buf, bufsize, "(")) {
    return 1;
  }

  for (int i = 0; i < dims; i++) {
    if (!next_token_is(buf, bufsize, "[")) {
      return 1;
    }

    next_token(buf, bufsize);

    if (sscanf(buf, "%"SCNu64, &shape[i]) != 1) {
      return 1;
    }

    if (!next_token_is(buf, bufsize, "]")) {
      return 1;
    }
  }

  if (!next_token_is(buf, bufsize, type_name)) {
    return 1;
  }


  if (!next_token_is(buf, bufsize, ")")) {
    return 1;
  }

  // Check whether the array really is empty.
  for (int i = 0; i < dims; i++) {
    if (shape[i] == 0) {
      return 0;
    }
  }

  // Not an empty array!
  return 1;
}

static int read_str_array(int64_t elem_size, str_reader elem_reader,
                          const char *type_name,
                          void **data, int64_t *shape, int64_t dims) {
  int ret;
  struct array_reader reader;
  char buf[100];

  int dims_seen;
  for (dims_seen = 0; dims_seen < dims; dims_seen++) {
    if (!next_token_is(buf, sizeof(buf), "[")) {
      break;
    }
  }

  if (dims_seen == 0) {
    return read_str_empty_array(buf, sizeof(buf), type_name, shape, dims);
  }

  if (dims_seen != dims) {
    return 1;
  }

  reader.shape = shape;
  reader.n_elems_used = 0;
  reader.elem_size = elem_size;
  reader.n_elems_space = 16;
  reader.elems = (char*) realloc(*data, (size_t)(elem_size*reader.n_elems_space));
  reader.elem_reader = elem_reader;

  ret = read_str_array_elems(buf, sizeof(buf), &reader, dims);

  *data = reader.elems;

  return ret;
}

#define READ_STR(MACRO, PTR, SUFFIX)                                   \
  remove_underscores(buf);                                              \
  int j;                                                                \
  if (sscanf(buf, "%"MACRO"%n", (PTR*)dest, &j) == 1) {                 \
    return !(strcmp(buf+j, "") == 0 || strcmp(buf+j, SUFFIX) == 0);     \
  } else {                                                              \
    return 1;                                                           \
  }

static int read_str_i8(char *buf, void* dest) {
  /* Some platforms (WINDOWS) does not support scanf %hhd or its
     cousin, %SCNi8.  Read into int first to avoid corrupting
     memory.

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417  */
  remove_underscores(buf);
  int j, x;
  if (sscanf(buf, "%i%n", &x, &j) == 1) {
    *(int8_t*)dest = (int8_t)x;
    return !(strcmp(buf+j, "") == 0 || strcmp(buf+j, "i8") == 0);
  } else {
    return 1;
  }
}

static int read_str_u8(char *buf, void* dest) {
  /* Some platforms (WINDOWS) does not support scanf %hhd or its
     cousin, %SCNu8.  Read into int first to avoid corrupting
     memory.

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417  */
  remove_underscores(buf);
  int j, x;
  if (sscanf(buf, "%i%n", &x, &j) == 1) {
    *(uint8_t*)dest = (uint8_t)x;
    return !(strcmp(buf+j, "") == 0 || strcmp(buf+j, "u8") == 0);
  } else {
    return 1;
  }
}

static int read_str_i16(char *buf, void* dest) {
  READ_STR(SCNi16, int16_t, "i16");
}

static int read_str_u16(char *buf, void* dest) {
  READ_STR(SCNi16, int16_t, "u16");
}

static int read_str_i32(char *buf, void* dest) {
  READ_STR(SCNi32, int32_t, "i32");
}

static int read_str_u32(char *buf, void* dest) {
  READ_STR(SCNi32, int32_t, "u32");
}

static int read_str_i64(char *buf, void* dest) {
  READ_STR(SCNi64, int64_t, "i64");
}

static int read_str_u64(char *buf, void* dest) {
  // FIXME: This is not correct, as SCNu64 only permits decimal
  // literals.  However, SCNi64 does not handle very large numbers
  // correctly (it's really for signed numbers, so that's fair).
  READ_STR(SCNu64, uint64_t, "u64");
}

static int read_str_f32(char *buf, void* dest) {
  remove_underscores(buf);
  if (strcmp(buf, "f32.nan") == 0) {
    *(float*)dest = NAN;
    return 0;
  } else if (strcmp(buf, "f32.inf") == 0) {
    *(float*)dest = INFINITY;
    return 0;
  } else if (strcmp(buf, "-f32.inf") == 0) {
    *(float*)dest = -INFINITY;
    return 0;
  } else {
    READ_STR("f", float, "f32");
  }
}

static int read_str_f64(char *buf, void* dest) {
  remove_underscores(buf);
  if (strcmp(buf, "f64.nan") == 0) {
    *(double*)dest = NAN;
    return 0;
  } else if (strcmp(buf, "f64.inf") == 0) {
    *(double*)dest = INFINITY;
    return 0;
  } else if (strcmp(buf, "-f64.inf") == 0) {
    *(double*)dest = -INFINITY;
    return 0;
  } else {
    READ_STR("lf", double, "f64");
  }
}

static int read_str_bool(char *buf, void* dest) {
  if (strcmp(buf, "true") == 0) {
    *(char*)dest = 1;
    return 0;
  } else if (strcmp(buf, "false") == 0) {
    *(char*)dest = 0;
    return 0;
  } else {
    return 1;
  }
}

static int write_str_i8(FILE *out, int8_t *src) {
  return fprintf(out, "%hhdi8", *src);
}

static int write_str_u8(FILE *out, uint8_t *src) {
  return fprintf(out, "%hhuu8", *src);
}

static int write_str_i16(FILE *out, int16_t *src) {
  return fprintf(out, "%hdi16", *src);
}

static int write_str_u16(FILE *out, uint16_t *src) {
  return fprintf(out, "%huu16", *src);
}

static int write_str_i32(FILE *out, int32_t *src) {
  return fprintf(out, "%di32", *src);
}

static int write_str_u32(FILE *out, uint32_t *src) {
  return fprintf(out, "%uu32", *src);
}

static int write_str_i64(FILE *out, int64_t *src) {
  return fprintf(out, "%"PRIi64"i64", *src);
}

static int write_str_u64(FILE *out, uint64_t *src) {
  return fprintf(out, "%"PRIu64"u64", *src);
}

static int write_str_f32(FILE *out, float *src) {
  float x = *src;
  if (isnan(x)) {
    return fprintf(out, "f32.nan");
  } else if (isinf(x) && x >= 0) {
    return fprintf(out, "f32.inf");
  } else if (isinf(x)) {
    return fprintf(out, "-f32.inf");
  } else {
    return fprintf(out, "%.6ff32", x);
  }
}

static int write_str_f64(FILE *out, double *src) {
  double x = *src;
  if (isnan(x)) {
    return fprintf(out, "f64.nan");
  } else if (isinf(x) && x >= 0) {
    return fprintf(out, "f64.inf");
  } else if (isinf(x)) {
    return fprintf(out, "-f64.inf");
  } else {
    return fprintf(out, "%.6ff64", *src);
  }
}

static int write_str_bool(FILE *out, void *src) {
  return fprintf(out, *(char*)src ? "true" : "false");
}

//// Binary I/O

#define BINARY_FORMAT_VERSION 2
#define IS_BIG_ENDIAN (!*(unsigned char *)&(uint16_t){1})

static void flip_bytes(int elem_size, unsigned char *elem) {
  for (int j=0; j<elem_size/2; j++) {
    unsigned char head = elem[j];
    int tail_index = elem_size-1-j;
    elem[j] = elem[tail_index];
    elem[tail_index] = head;
  }
}

// On Windows we need to explicitly set the file mode to not mangle
// newline characters.  On *nix there is no difference.
#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
static void set_binary_mode(FILE *f) {
  setmode(fileno(f), O_BINARY);
}
#else
static void set_binary_mode(FILE *f) {
  (void)f;
}
#endif

static int read_byte(void* dest) {
  int num_elems_read = fread(dest, 1, 1, stdin);
  return num_elems_read == 1 ? 0 : 1;
}

//// Types

struct primtype_info_t {
  const char binname[4]; // Used for parsing binary data.
  const char* type_name; // Same name as in Futhark.
  const int64_t size; // in bytes
  const writer write_str; // Write in text format.
  const str_reader read_str; // Read in text format.
};

static const struct primtype_info_t i8_info =
  {.binname = "  i8", .type_name = "i8",   .size = 1,
   .write_str = (writer)write_str_i8, .read_str = (str_reader)read_str_i8};
static const struct primtype_info_t i16_info =
  {.binname = " i16", .type_name = "i16",  .size = 2,
   .write_str = (writer)write_str_i16, .read_str = (str_reader)read_str_i16};
static const struct primtype_info_t i32_info =
  {.binname = " i32", .type_name = "i32",  .size = 4,
   .write_str = (writer)write_str_i32, .read_str = (str_reader)read_str_i32};
static const struct primtype_info_t i64_info =
  {.binname = " i64", .type_name = "i64",  .size = 8,
   .write_str = (writer)write_str_i64, .read_str = (str_reader)read_str_i64};
static const struct primtype_info_t u8_info =
  {.binname = "  u8", .type_name = "u8",   .size = 1,
   .write_str = (writer)write_str_u8, .read_str = (str_reader)read_str_u8};
static const struct primtype_info_t u16_info =
  {.binname = " u16", .type_name = "u16",  .size = 2,
   .write_str = (writer)write_str_u16, .read_str = (str_reader)read_str_u16};
static const struct primtype_info_t u32_info =
  {.binname = " u32", .type_name = "u32",  .size = 4,
   .write_str = (writer)write_str_u32, .read_str = (str_reader)read_str_u32};
static const struct primtype_info_t u64_info =
  {.binname = " u64", .type_name = "u64",  .size = 8,
   .write_str = (writer)write_str_u64, .read_str = (str_reader)read_str_u64};
static const struct primtype_info_t f32_info =
  {.binname = " f32", .type_name = "f32",  .size = 4,
   .write_str = (writer)write_str_f32, .read_str = (str_reader)read_str_f32};
static const struct primtype_info_t f64_info =
  {.binname = " f64", .type_name = "f64",  .size = 8,
   .write_str = (writer)write_str_f64, .read_str = (str_reader)read_str_f64};
static const struct primtype_info_t bool_info =
  {.binname = "bool", .type_name = "bool", .size = 1,
   .write_str = (writer)write_str_bool, .read_str = (str_reader)read_str_bool};

static const struct primtype_info_t* primtypes[] = {
  &i8_info, &i16_info, &i32_info, &i64_info,
  &u8_info, &u16_info, &u32_info, &u64_info,
  &f32_info, &f64_info,
  &bool_info,
  NULL // NULL-terminated
};

// General value interface.  All endian business taken care of at
// lower layers.

static int read_is_binary() {
  skipspaces();
  int c = getchar();
  if (c == 'b') {
    int8_t bin_version;
    int ret = read_byte(&bin_version);

    if (ret != 0) { futhark_panic(1, "binary-input: could not read version.\n"); }

    if (bin_version != BINARY_FORMAT_VERSION) {
      futhark_panic(1, "binary-input: File uses version %i, but I only understand version %i.\n",
            bin_version, BINARY_FORMAT_VERSION);
    }

    return 1;
  }
  ungetc(c, stdin);
  return 0;
}

static const struct primtype_info_t* read_bin_read_type_enum() {
  char read_binname[4];

  int num_matched = scanf("%4c", read_binname);
  if (num_matched != 1) { futhark_panic(1, "binary-input: Couldn't read element type.\n"); }

  const struct primtype_info_t **type = primtypes;

  for (; *type != NULL; type++) {
    // I compare the 4 characters manually instead of using strncmp because
    // this allows any value to be used, also NULL bytes
    if (memcmp(read_binname, (*type)->binname, 4) == 0) {
      return *type;
    }
  }
  futhark_panic(1, "binary-input: Did not recognize the type '%s'.\n", read_binname);
  return NULL;
}

static void read_bin_ensure_scalar(const struct primtype_info_t *expected_type) {
  int8_t bin_dims;
  int ret = read_byte(&bin_dims);
  if (ret != 0) { futhark_panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != 0) {
    futhark_panic(1, "binary-input: Expected scalar (0 dimensions), but got array with %i dimensions.\n",
          bin_dims);
  }

  const struct primtype_info_t *bin_type = read_bin_read_type_enum();
  if (bin_type != expected_type) {
    futhark_panic(1, "binary-input: Expected scalar of type %s but got scalar of type %s.\n",
          expected_type->type_name,
          bin_type->type_name);
  }
}

//// High-level interface

static int read_bin_array(const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  int ret;

  int8_t bin_dims;
  ret = read_byte(&bin_dims);
  if (ret != 0) { futhark_panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != dims) {
    futhark_panic(1, "binary-input: Expected %i dimensions, but got array with %i dimensions.\n",
          dims, bin_dims);
  }

  const struct primtype_info_t *bin_primtype = read_bin_read_type_enum();
  if (expected_type != bin_primtype) {
    futhark_panic(1, "binary-input: Expected %iD-array with element type '%s' but got %iD-array with element type '%s'.\n",
          dims, expected_type->type_name, dims, bin_primtype->type_name);
  }

  int64_t elem_count = 1;
  for (int i=0; i<dims; i++) {
    int64_t bin_shape;
    ret = fread(&bin_shape, sizeof(bin_shape), 1, stdin);
    if (ret != 1) {
      futhark_panic(1, "binary-input: Couldn't read size for dimension %i of array.\n", i);
    }
    if (IS_BIG_ENDIAN) {
      flip_bytes(sizeof(bin_shape), (unsigned char*) &bin_shape);
    }
    elem_count *= bin_shape;
    shape[i] = bin_shape;
  }

  int64_t elem_size = expected_type->size;
  void* tmp = realloc(*data, (size_t)(elem_count * elem_size));
  if (tmp == NULL) {
    futhark_panic(1, "binary-input: Failed to allocate array of size %i.\n",
          elem_count * elem_size);
  }
  *data = tmp;

  int64_t num_elems_read = (int64_t)fread(*data, (size_t)elem_size, (size_t)elem_count, stdin);
  if (num_elems_read != elem_count) {
    futhark_panic(1, "binary-input: tried to read %i elements of an array, but only got %i elements.\n",
          elem_count, num_elems_read);
  }

  // If we're on big endian platform we must change all multibyte elements
  // from using little endian to big endian
  if (IS_BIG_ENDIAN && elem_size != 1) {
    flip_bytes(elem_size, (unsigned char*) *data);
  }

  return 0;
}

static int read_array(const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  if (!read_is_binary()) {
    return read_str_array(expected_type->size, (str_reader)expected_type->read_str, expected_type->type_name, data, shape, dims);
  } else {
    return read_bin_array(expected_type, data, shape, dims);
  }
}

static int end_of_input() {
  skipspaces();
  char token[2];
  next_token(token, sizeof(token));
  if (strcmp(token, "") == 0) {
    return 0;
  } else {
    return 1;
  }
}

static int write_str_array(FILE *out, const struct primtype_info_t *elem_type, unsigned char *data, int64_t *shape, int8_t rank) {
  if (rank==0) {
    elem_type->write_str(out, (void*)data);
  } else {
    int64_t len = (int64_t)shape[0];
    int64_t slice_size = 1;

    int64_t elem_size = elem_type->size;
    for (int8_t i = 1; i < rank; i++) {
      slice_size *= shape[i];
    }

    if (len*slice_size == 0) {
      printf("empty(");
      for (int64_t i = 0; i < rank; i++) {
        printf("[%"PRIi64"]", shape[i]);
      }
      printf("%s", elem_type->type_name);
      printf(")");
    } else if (rank==1) {
      putchar('[');
      for (int64_t i = 0; i < len; i++) {
        elem_type->write_str(out, (void*) (data + i * elem_size));
        if (i != len-1) {
          printf(", ");
        }
      }
      putchar(']');
    } else {
      putchar('[');
      for (int64_t i = 0; i < len; i++) {
        write_str_array(out, elem_type, data + i * slice_size * elem_size, shape+1, rank-1);
        if (i != len-1) {
          printf(", ");
        }
      }
      putchar(']');
    }
  }
  return 0;
}

static int write_bin_array(FILE *out, const struct primtype_info_t *elem_type, unsigned char *data, int64_t *shape, int8_t rank) {
  int64_t num_elems = 1;
  for (int64_t i = 0; i < rank; i++) {
    num_elems *= shape[i];
  }

  fputc('b', out);
  fputc((char)BINARY_FORMAT_VERSION, out);
  fwrite(&rank, sizeof(int8_t), 1, out);
  fputs(elem_type->binname, out);
  if (shape != NULL) {
    fwrite(shape, sizeof(int64_t), (size_t)rank, out);
  }

  if (IS_BIG_ENDIAN) {
    for (int64_t i = 0; i < num_elems; i++) {
      unsigned char *elem = data+i*elem_type->size;
      for (int64_t j = 0; j < elem_type->size; j++) {
        fwrite(&elem[elem_type->size-j], 1, 1, out);
      }
    }
  } else {
    fwrite(data, (size_t)elem_type->size, (size_t)num_elems, out);
  }

  return 0;
}

static int write_array(FILE *out, int write_binary,
                       const struct primtype_info_t *elem_type, void *data, int64_t *shape, int8_t rank) {
  if (write_binary) {
    return write_bin_array(out, elem_type, data, shape, rank);
  } else {
    return write_str_array(out, elem_type, data, shape, rank);
  }
}

static int read_scalar(const struct primtype_info_t *expected_type, void *dest) {
  if (!read_is_binary()) {
    char buf[100];
    next_token(buf, sizeof(buf));
    return expected_type->read_str(buf, dest);
  } else {
    read_bin_ensure_scalar(expected_type);
    int64_t elem_size = expected_type->size;
    int num_elems_read = fread(dest, (size_t)elem_size, 1, stdin);
    if (IS_BIG_ENDIAN) {
      flip_bytes(elem_size, (unsigned char*) dest);
    }
    return num_elems_read == 1 ? 0 : 1;
  }
}

static int write_scalar(FILE *out, int write_binary, const struct primtype_info_t *type, void *src) {
  if (write_binary) {
    return write_bin_array(out, type, src, NULL, 0);
  } else {
    return type->write_str(out, src);
  }
}

// End of values.h.

#define __private
static int binary_output = 0;
static FILE *runtime_file;
static int perform_warmup = 0;
static int num_runs = 1;
static const char *entry_point = "main";
// Start of tuning.h.

static char* load_tuning_file(const char *fname,
                              void *cfg,
                              int (*set_size)(void*, const char*, size_t)) {
  const int max_line_len = 1024;
  char* line = (char*) malloc(max_line_len);

  FILE *f = fopen(fname, "r");

  if (f == NULL) {
    snprintf(line, max_line_len, "Cannot open file: %s", strerror(errno));
    return line;
  }

  int lineno = 0;
  while (fgets(line, max_line_len, f) != NULL) {
    lineno++;
    char *eql = strstr(line, "=");
    if (eql) {
      *eql = 0;
      int value = atoi(eql+1);
      if (set_size(cfg, line, value) != 0) {
        strncpy(eql+1, line, max_line_len-strlen(line)-1);
        snprintf(line, max_line_len, "Unknown name '%s' on line %d.", eql+1, lineno);
        return line;
      }
    } else {
      snprintf(line, max_line_len, "Invalid line %d (must be of form 'name=int').",
               lineno);
      return line;
    }
  }

  free(line);

  return NULL;
}

// End of tuning.h.

int parse_options(struct futhark_context_config *cfg, int argc,
                  char *const argv[])
{
    int ch;
    static struct option long_options[] = {{"write-runtime-to",
                                            required_argument, NULL, 1},
                                           {"runs", required_argument, NULL, 2},
                                           {"debugging", no_argument, NULL, 3},
                                           {"log", no_argument, NULL, 4},
                                           {"entry-point", required_argument,
                                            NULL, 5}, {"binary-output",
                                                       no_argument, NULL, 6},
                                           {"default-group-size",
                                            required_argument, NULL, 7},
                                           {"default-num-groups",
                                            required_argument, NULL, 8},
                                           {"default-tile-size",
                                            required_argument, NULL, 9},
                                           {"default-threshold",
                                            required_argument, NULL, 10},
                                           {"print-sizes", no_argument, NULL,
                                            11}, {"size", required_argument,
                                                  NULL, 12}, {"tuning",
                                                              required_argument,
                                                              NULL, 13},
                                           {"platform", required_argument, NULL,
                                            14}, {"device", required_argument,
                                                  NULL, 15}, {"dump-opencl",
                                                              required_argument,
                                                              NULL, 16},
                                           {"load-opencl", required_argument,
                                            NULL, 17}, {"dump-opencl-binary",
                                                        required_argument, NULL,
                                                        18},
                                           {"load-opencl-binary",
                                            required_argument, NULL, 19},
                                           {"build-option", required_argument,
                                            NULL, 20}, {"profile", no_argument,
                                                        NULL, 21}, {0, 0, 0,
                                                                    0}};
    
    while ((ch = getopt_long(argc, argv, ":t:r:DLe:bp:d:P", long_options,
                             NULL)) != -1) {
        if (ch == 1 || ch == 't') {
            runtime_file = fopen(optarg, "w");
            if (runtime_file == NULL)
                futhark_panic(1, "Cannot open %s: %s\n", optarg,
                              strerror(errno));
        }
        if (ch == 2 || ch == 'r') {
            num_runs = atoi(optarg);
            perform_warmup = 1;
            if (num_runs <= 0)
                futhark_panic(1, "Need a positive number of runs, not %s\n",
                              optarg);
        }
        if (ch == 3 || ch == 'D')
            futhark_context_config_set_debugging(cfg, 1);
        if (ch == 4 || ch == 'L')
            futhark_context_config_set_logging(cfg, 1);
        if (ch == 5 || ch == 'e') {
            if (entry_point != NULL)
                entry_point = optarg;
        }
        if (ch == 6 || ch == 'b')
            binary_output = 1;
        if (ch == 7)
            futhark_context_config_set_default_group_size(cfg, atoi(optarg));
        if (ch == 8)
            futhark_context_config_set_default_num_groups(cfg, atoi(optarg));
        if (ch == 9)
            futhark_context_config_set_default_tile_size(cfg, atoi(optarg));
        if (ch == 10)
            futhark_context_config_set_default_threshold(cfg, atoi(optarg));
        if (ch == 11) {
            int n = futhark_get_num_sizes();
            
            for (int i = 0; i < n; i++)
                printf("%s (%s)\n", futhark_get_size_name(i),
                       futhark_get_size_class(i));
            exit(0);
        }
        if (ch == 12) {
            char *name = optarg;
            char *equals = strstr(optarg, "=");
            char *value_str = equals != NULL ? equals + 1 : optarg;
            int value = atoi(value_str);
            
            if (equals != NULL) {
                *equals = 0;
                if (futhark_context_config_set_size(cfg, name, value) != 0)
                    futhark_panic(1, "Unknown size: %s\n", name);
            } else
                futhark_panic(1, "Invalid argument for size option: %s\n",
                              optarg);
        }
        if (ch == 13) {
            char *ret = load_tuning_file(optarg, cfg, (int (*)(void *, const
                                                               char *,
                                                               size_t)) futhark_context_config_set_size);
            
            if (ret != NULL)
                futhark_panic(1, "When loading tuning from '%s': %s\n", optarg,
                              ret);
        }
        if (ch == 14 || ch == 'p')
            futhark_context_config_set_platform(cfg, optarg);
        if (ch == 15 || ch == 'd')
            futhark_context_config_set_device(cfg, optarg);
        if (ch == 16) {
            futhark_context_config_dump_program_to(cfg, optarg);
            entry_point = NULL;
        }
        if (ch == 17)
            futhark_context_config_load_program_from(cfg, optarg);
        if (ch == 18) {
            futhark_context_config_dump_binary_to(cfg, optarg);
            entry_point = NULL;
        }
        if (ch == 19)
            futhark_context_config_load_binary_from(cfg, optarg);
        if (ch == 20)
            futhark_context_config_add_build_option(cfg, optarg);
        if (ch == 21 || ch == 'P')
            futhark_context_config_set_profiling(cfg, 1);
        if (ch == ':')
            futhark_panic(-1, "Missing argument for option %s\n", argv[optind -
                                                                       1]);
        if (ch == '?') {
            fprintf(stderr, "Usage: %s: %s\n", fut_progname,
                    "[-t/--write-runtime-to FILE] [-r/--runs INT] [-D/--debugging] [-L/--log] [-e/--entry-point NAME] [-b/--binary-output] [--default-group-size INT] [--default-num-groups INT] [--default-tile-size INT] [--default-threshold INT] [--print-sizes] [--size NAME=INT] [--tuning FILE] [-p/--platform NAME] [-d/--device NAME] [--dump-opencl FILE] [--load-opencl FILE] [--dump-opencl-binary FILE] [--load-opencl-binary FILE] [--build-option OPT] [-P/--profile]");
            futhark_panic(1, "Unknown option: %s\n", argv[optind - 1]);
        }
    }
    return optind;
}
static void futrts_cli_entry_main(struct futhark_context *ctx)
{
    int64_t t_start, t_end;
    int time_runs = 0, profile_run = 0;
    
    // We do not want to profile all the initialisation.
    futhark_context_pause_profiling(ctx);
    // Declare and read input.
    set_binary_mode(stdin);
    
    struct futhark_i32_1d *read_value_6010;
    int64_t read_shape_6011[1];
    int32_t *read_arr_6012 = NULL;
    
    errno = 0;
    if (read_array(&i32_info, (void **) &read_arr_6012, read_shape_6011, 1) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s%s (errno: %s).\n", 0,
                      "[]", i32_info.type_name, strerror(errno));
    
    struct futhark_bool_1d *read_value_6013;
    int64_t read_shape_6014[1];
    bool *read_arr_6015 = NULL;
    
    errno = 0;
    if (read_array(&bool_info, (void **) &read_arr_6015, read_shape_6014, 1) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s%s (errno: %s).\n", 1,
                      "[]", bool_info.type_name, strerror(errno));
    if (end_of_input() != 0)
        futhark_panic(1, "Expected EOF on stdin after reading input for %s.\n",
                      "\"main\"");
    
    struct futhark_i32_1d *result_6016;
    
    if (perform_warmup) {
        int r;
        
        assert((read_value_6010 = futhark_new_i32_1d(ctx, read_arr_6012,
                                                     read_shape_6011[0])) != 0);
        assert((read_value_6013 = futhark_new_bool_1d(ctx, read_arr_6015,
                                                      read_shape_6014[0])) !=
            0);
        if (futhark_context_sync(ctx) != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        ;
        // Only profile last run.
        if (profile_run)
            futhark_context_unpause_profiling(ctx);
        t_start = get_wall_time();
        r = futhark_entry_main(ctx, &result_6016, read_value_6010,
                               read_value_6013);
        if (r != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        if (futhark_context_sync(ctx) != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        ;
        if (profile_run)
            futhark_context_pause_profiling(ctx);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
        assert(futhark_free_i32_1d(ctx, read_value_6010) == 0);
        assert(futhark_free_bool_1d(ctx, read_value_6013) == 0);
        assert(futhark_free_i32_1d(ctx, result_6016) == 0);
    }
    time_runs = 1;
    // Proper run.
    for (int run = 0; run < num_runs; run++) {
        // Only profile last run.
        profile_run = run == num_runs - 1;
        
        int r;
        
        assert((read_value_6010 = futhark_new_i32_1d(ctx, read_arr_6012,
                                                     read_shape_6011[0])) != 0);
        assert((read_value_6013 = futhark_new_bool_1d(ctx, read_arr_6015,
                                                      read_shape_6014[0])) !=
            0);
        if (futhark_context_sync(ctx) != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        ;
        // Only profile last run.
        if (profile_run)
            futhark_context_unpause_profiling(ctx);
        t_start = get_wall_time();
        r = futhark_entry_main(ctx, &result_6016, read_value_6010,
                               read_value_6013);
        if (r != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        if (futhark_context_sync(ctx) != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        ;
        if (profile_run)
            futhark_context_pause_profiling(ctx);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
        assert(futhark_free_i32_1d(ctx, read_value_6010) == 0);
        assert(futhark_free_bool_1d(ctx, read_value_6013) == 0);
        if (run < num_runs - 1) {
            assert(futhark_free_i32_1d(ctx, result_6016) == 0);
        }
    }
    free(read_arr_6012);
    free(read_arr_6015);
    if (binary_output)
        set_binary_mode(stdout);
    {
        int32_t *arr = calloc(sizeof(int32_t), futhark_shape_i32_1d(ctx,
                                                                    result_6016)[0]);
        
        assert(arr != NULL);
        assert(futhark_values_i32_1d(ctx, result_6016, arr) == 0);
        write_array(stdout, binary_output, &i32_info, arr,
                    futhark_shape_i32_1d(ctx, result_6016), 1);
        free(arr);
    }
    printf("\n");
    assert(futhark_free_i32_1d(ctx, result_6016) == 0);
}
typedef void entry_point_fun(struct futhark_context *);
struct entry_point_entry {
    const char *name;
    entry_point_fun *fun;
} ;
int main(int argc, char **argv)
{
    fut_progname = argv[0];
    
    struct entry_point_entry entry_points[] = {{.name ="main", .fun =
                                                futrts_cli_entry_main}};
    struct futhark_context_config *cfg = futhark_context_config_new();
    
    assert(cfg != NULL);
    
    int parsed_options = parse_options(cfg, argc, argv);
    
    argc -= parsed_options;
    argv += parsed_options;
    if (argc != 0)
        futhark_panic(1, "Excess non-option: %s\n", argv[0]);
    
    struct futhark_context *ctx = futhark_context_new(cfg);
    
    assert(ctx != NULL);
    
    char *error = futhark_context_get_error(ctx);
    
    if (error != NULL)
        futhark_panic(1, "%s", error);
    if (entry_point != NULL) {
        int num_entry_points = sizeof(entry_points) / sizeof(entry_points[0]);
        entry_point_fun *entry_point_fun = NULL;
        
        for (int i = 0; i < num_entry_points; i++) {
            if (strcmp(entry_points[i].name, entry_point) == 0) {
                entry_point_fun = entry_points[i].fun;
                break;
            }
        }
        if (entry_point_fun == NULL) {
            fprintf(stderr,
                    "No entry point '%s'.  Select another with --entry-point.  Options are:\n",
                    entry_point);
            for (int i = 0; i < num_entry_points; i++)
                fprintf(stderr, "%s\n", entry_points[i].name);
            return 1;
        }
        entry_point_fun(ctx);
        if (runtime_file != NULL)
            fclose(runtime_file);
        futhark_debugging_report(ctx);
    }
    futhark_context_free(ctx);
    futhark_context_config_free(cfg);
    return 0;
}
#ifdef _MSC_VER
#define inline __inline
#endif
#include <string.h>
#include <inttypes.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
#define CL_TARGET_OPENCL_VERSION 120
#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#ifdef __APPLE__
#define CL_SILENCE_DEPRECATION
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif

// Start of lock.h.

/* A very simple cross-platform implementation of locks.  Uses
   pthreads on Unix and some Windows thing there.  Futhark's
   host-level code is not multithreaded, but user code may be, so we
   need some mechanism for ensuring atomic access to API functions.
   This is that mechanism.  It is not exposed to user code at all, so
   we do not have to worry about name collisions. */

#ifdef _WIN32

typedef HANDLE lock_t;

static lock_t create_lock(lock_t *lock) {
  *lock = CreateMutex(NULL,  /* Default security attributes. */
                      FALSE, /* Initially unlocked. */
                      NULL); /* Unnamed. */
}

static void lock_lock(lock_t *lock) {
  assert(WaitForSingleObject(*lock, INFINITE) == WAIT_OBJECT_0);
}

static void lock_unlock(lock_t *lock) {
  assert(ReleaseMutex(*lock));
}

static void free_lock(lock_t *lock) {
  CloseHandle(*lock);
}

#else
/* Assuming POSIX */

#include <pthread.h>

typedef pthread_mutex_t lock_t;

static void create_lock(lock_t *lock) {
  int r = pthread_mutex_init(lock, NULL);
  assert(r == 0);
}

static void lock_lock(lock_t *lock) {
  int r = pthread_mutex_lock(lock);
  assert(r == 0);
}

static void lock_unlock(lock_t *lock) {
  int r = pthread_mutex_unlock(lock);
  assert(r == 0);
}

static void free_lock(lock_t *lock) {
  /* Nothing to do for pthreads. */
  (void)lock;
}

#endif

// End of lock.h.

static inline uint8_t add8(uint8_t x, uint8_t y)
{
    return x + y;
}
static inline uint16_t add16(uint16_t x, uint16_t y)
{
    return x + y;
}
static inline uint32_t add32(uint32_t x, uint32_t y)
{
    return x + y;
}
static inline uint64_t add64(uint64_t x, uint64_t y)
{
    return x + y;
}
static inline uint8_t sub8(uint8_t x, uint8_t y)
{
    return x - y;
}
static inline uint16_t sub16(uint16_t x, uint16_t y)
{
    return x - y;
}
static inline uint32_t sub32(uint32_t x, uint32_t y)
{
    return x - y;
}
static inline uint64_t sub64(uint64_t x, uint64_t y)
{
    return x - y;
}
static inline uint8_t mul8(uint8_t x, uint8_t y)
{
    return x * y;
}
static inline uint16_t mul16(uint16_t x, uint16_t y)
{
    return x * y;
}
static inline uint32_t mul32(uint32_t x, uint32_t y)
{
    return x * y;
}
static inline uint64_t mul64(uint64_t x, uint64_t y)
{
    return x * y;
}
static inline uint8_t udiv8(uint8_t x, uint8_t y)
{
    return x / y;
}
static inline uint16_t udiv16(uint16_t x, uint16_t y)
{
    return x / y;
}
static inline uint32_t udiv32(uint32_t x, uint32_t y)
{
    return x / y;
}
static inline uint64_t udiv64(uint64_t x, uint64_t y)
{
    return x / y;
}
static inline uint8_t umod8(uint8_t x, uint8_t y)
{
    return x % y;
}
static inline uint16_t umod16(uint16_t x, uint16_t y)
{
    return x % y;
}
static inline uint32_t umod32(uint32_t x, uint32_t y)
{
    return x % y;
}
static inline uint64_t umod64(uint64_t x, uint64_t y)
{
    return x % y;
}
static inline int8_t sdiv8(int8_t x, int8_t y)
{
    int8_t q = x / y;
    int8_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int16_t sdiv16(int16_t x, int16_t y)
{
    int16_t q = x / y;
    int16_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int32_t sdiv32(int32_t x, int32_t y)
{
    int32_t q = x / y;
    int32_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int64_t sdiv64(int64_t x, int64_t y)
{
    int64_t q = x / y;
    int64_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int8_t smod8(int8_t x, int8_t y)
{
    int8_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int16_t smod16(int16_t x, int16_t y)
{
    int16_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int32_t smod32(int32_t x, int32_t y)
{
    int32_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int64_t smod64(int64_t x, int64_t y)
{
    int64_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int8_t squot8(int8_t x, int8_t y)
{
    return x / y;
}
static inline int16_t squot16(int16_t x, int16_t y)
{
    return x / y;
}
static inline int32_t squot32(int32_t x, int32_t y)
{
    return x / y;
}
static inline int64_t squot64(int64_t x, int64_t y)
{
    return x / y;
}
static inline int8_t srem8(int8_t x, int8_t y)
{
    return x % y;
}
static inline int16_t srem16(int16_t x, int16_t y)
{
    return x % y;
}
static inline int32_t srem32(int32_t x, int32_t y)
{
    return x % y;
}
static inline int64_t srem64(int64_t x, int64_t y)
{
    return x % y;
}
static inline int8_t smin8(int8_t x, int8_t y)
{
    return x < y ? x : y;
}
static inline int16_t smin16(int16_t x, int16_t y)
{
    return x < y ? x : y;
}
static inline int32_t smin32(int32_t x, int32_t y)
{
    return x < y ? x : y;
}
static inline int64_t smin64(int64_t x, int64_t y)
{
    return x < y ? x : y;
}
static inline uint8_t umin8(uint8_t x, uint8_t y)
{
    return x < y ? x : y;
}
static inline uint16_t umin16(uint16_t x, uint16_t y)
{
    return x < y ? x : y;
}
static inline uint32_t umin32(uint32_t x, uint32_t y)
{
    return x < y ? x : y;
}
static inline uint64_t umin64(uint64_t x, uint64_t y)
{
    return x < y ? x : y;
}
static inline int8_t smax8(int8_t x, int8_t y)
{
    return x < y ? y : x;
}
static inline int16_t smax16(int16_t x, int16_t y)
{
    return x < y ? y : x;
}
static inline int32_t smax32(int32_t x, int32_t y)
{
    return x < y ? y : x;
}
static inline int64_t smax64(int64_t x, int64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t umax8(uint8_t x, uint8_t y)
{
    return x < y ? y : x;
}
static inline uint16_t umax16(uint16_t x, uint16_t y)
{
    return x < y ? y : x;
}
static inline uint32_t umax32(uint32_t x, uint32_t y)
{
    return x < y ? y : x;
}
static inline uint64_t umax64(uint64_t x, uint64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t shl8(uint8_t x, uint8_t y)
{
    return x << y;
}
static inline uint16_t shl16(uint16_t x, uint16_t y)
{
    return x << y;
}
static inline uint32_t shl32(uint32_t x, uint32_t y)
{
    return x << y;
}
static inline uint64_t shl64(uint64_t x, uint64_t y)
{
    return x << y;
}
static inline uint8_t lshr8(uint8_t x, uint8_t y)
{
    return x >> y;
}
static inline uint16_t lshr16(uint16_t x, uint16_t y)
{
    return x >> y;
}
static inline uint32_t lshr32(uint32_t x, uint32_t y)
{
    return x >> y;
}
static inline uint64_t lshr64(uint64_t x, uint64_t y)
{
    return x >> y;
}
static inline int8_t ashr8(int8_t x, int8_t y)
{
    return x >> y;
}
static inline int16_t ashr16(int16_t x, int16_t y)
{
    return x >> y;
}
static inline int32_t ashr32(int32_t x, int32_t y)
{
    return x >> y;
}
static inline int64_t ashr64(int64_t x, int64_t y)
{
    return x >> y;
}
static inline uint8_t and8(uint8_t x, uint8_t y)
{
    return x & y;
}
static inline uint16_t and16(uint16_t x, uint16_t y)
{
    return x & y;
}
static inline uint32_t and32(uint32_t x, uint32_t y)
{
    return x & y;
}
static inline uint64_t and64(uint64_t x, uint64_t y)
{
    return x & y;
}
static inline uint8_t or8(uint8_t x, uint8_t y)
{
    return x | y;
}
static inline uint16_t or16(uint16_t x, uint16_t y)
{
    return x | y;
}
static inline uint32_t or32(uint32_t x, uint32_t y)
{
    return x | y;
}
static inline uint64_t or64(uint64_t x, uint64_t y)
{
    return x | y;
}
static inline uint8_t xor8(uint8_t x, uint8_t y)
{
    return x ^ y;
}
static inline uint16_t xor16(uint16_t x, uint16_t y)
{
    return x ^ y;
}
static inline uint32_t xor32(uint32_t x, uint32_t y)
{
    return x ^ y;
}
static inline uint64_t xor64(uint64_t x, uint64_t y)
{
    return x ^ y;
}
static inline bool ult8(uint8_t x, uint8_t y)
{
    return x < y;
}
static inline bool ult16(uint16_t x, uint16_t y)
{
    return x < y;
}
static inline bool ult32(uint32_t x, uint32_t y)
{
    return x < y;
}
static inline bool ult64(uint64_t x, uint64_t y)
{
    return x < y;
}
static inline bool ule8(uint8_t x, uint8_t y)
{
    return x <= y;
}
static inline bool ule16(uint16_t x, uint16_t y)
{
    return x <= y;
}
static inline bool ule32(uint32_t x, uint32_t y)
{
    return x <= y;
}
static inline bool ule64(uint64_t x, uint64_t y)
{
    return x <= y;
}
static inline bool slt8(int8_t x, int8_t y)
{
    return x < y;
}
static inline bool slt16(int16_t x, int16_t y)
{
    return x < y;
}
static inline bool slt32(int32_t x, int32_t y)
{
    return x < y;
}
static inline bool slt64(int64_t x, int64_t y)
{
    return x < y;
}
static inline bool sle8(int8_t x, int8_t y)
{
    return x <= y;
}
static inline bool sle16(int16_t x, int16_t y)
{
    return x <= y;
}
static inline bool sle32(int32_t x, int32_t y)
{
    return x <= y;
}
static inline bool sle64(int64_t x, int64_t y)
{
    return x <= y;
}
static inline int8_t pow8(int8_t x, int8_t y)
{
    int8_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int16_t pow16(int16_t x, int16_t y)
{
    int16_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int32_t pow32(int32_t x, int32_t y)
{
    int32_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int64_t pow64(int64_t x, int64_t y)
{
    int64_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline bool itob_i8_bool(int8_t x)
{
    return x;
}
static inline bool itob_i16_bool(int16_t x)
{
    return x;
}
static inline bool itob_i32_bool(int32_t x)
{
    return x;
}
static inline bool itob_i64_bool(int64_t x)
{
    return x;
}
static inline int8_t btoi_bool_i8(bool x)
{
    return x;
}
static inline int16_t btoi_bool_i16(bool x)
{
    return x;
}
static inline int32_t btoi_bool_i32(bool x)
{
    return x;
}
static inline int64_t btoi_bool_i64(bool x)
{
    return x;
}
#define sext_i8_i8(x) ((int8_t) (int8_t) x)
#define sext_i8_i16(x) ((int16_t) (int8_t) x)
#define sext_i8_i32(x) ((int32_t) (int8_t) x)
#define sext_i8_i64(x) ((int64_t) (int8_t) x)
#define sext_i16_i8(x) ((int8_t) (int16_t) x)
#define sext_i16_i16(x) ((int16_t) (int16_t) x)
#define sext_i16_i32(x) ((int32_t) (int16_t) x)
#define sext_i16_i64(x) ((int64_t) (int16_t) x)
#define sext_i32_i8(x) ((int8_t) (int32_t) x)
#define sext_i32_i16(x) ((int16_t) (int32_t) x)
#define sext_i32_i32(x) ((int32_t) (int32_t) x)
#define sext_i32_i64(x) ((int64_t) (int32_t) x)
#define sext_i64_i8(x) ((int8_t) (int64_t) x)
#define sext_i64_i16(x) ((int16_t) (int64_t) x)
#define sext_i64_i32(x) ((int32_t) (int64_t) x)
#define sext_i64_i64(x) ((int64_t) (int64_t) x)
#define zext_i8_i8(x) ((uint8_t) (uint8_t) x)
#define zext_i8_i16(x) ((uint16_t) (uint8_t) x)
#define zext_i8_i32(x) ((uint32_t) (uint8_t) x)
#define zext_i8_i64(x) ((uint64_t) (uint8_t) x)
#define zext_i16_i8(x) ((uint8_t) (uint16_t) x)
#define zext_i16_i16(x) ((uint16_t) (uint16_t) x)
#define zext_i16_i32(x) ((uint32_t) (uint16_t) x)
#define zext_i16_i64(x) ((uint64_t) (uint16_t) x)
#define zext_i32_i8(x) ((uint8_t) (uint32_t) x)
#define zext_i32_i16(x) ((uint16_t) (uint32_t) x)
#define zext_i32_i32(x) ((uint32_t) (uint32_t) x)
#define zext_i32_i64(x) ((uint64_t) (uint32_t) x)
#define zext_i64_i8(x) ((uint8_t) (uint64_t) x)
#define zext_i64_i16(x) ((uint16_t) (uint64_t) x)
#define zext_i64_i32(x) ((uint32_t) (uint64_t) x)
#define zext_i64_i64(x) ((uint64_t) (uint64_t) x)
#if defined(__OPENCL_VERSION__)
static int32_t futrts_popc8(int8_t x)
{
    return popcount(x);
}
static int32_t futrts_popc16(int16_t x)
{
    return popcount(x);
}
static int32_t futrts_popc32(int32_t x)
{
    return popcount(x);
}
static int32_t futrts_popc64(int64_t x)
{
    return popcount(x);
}
#elif defined(__CUDA_ARCH__)
static int32_t futrts_popc8(int8_t x)
{
    return __popc(zext_i8_i32(x));
}
static int32_t futrts_popc16(int16_t x)
{
    return __popc(zext_i16_i32(x));
}
static int32_t futrts_popc32(int32_t x)
{
    return __popc(x);
}
static int32_t futrts_popc64(int64_t x)
{
    return __popcll(x);
}
#else
static int32_t futrts_popc8(int8_t x)
{
    int c = 0;
    
    for (; x; ++c)
        x &= x - 1;
    return c;
}
static int32_t futrts_popc16(int16_t x)
{
    int c = 0;
    
    for (; x; ++c)
        x &= x - 1;
    return c;
}
static int32_t futrts_popc32(int32_t x)
{
    int c = 0;
    
    for (; x; ++c)
        x &= x - 1;
    return c;
}
static int32_t futrts_popc64(int64_t x)
{
    int c = 0;
    
    for (; x; ++c)
        x &= x - 1;
    return c;
}
#endif
#if defined(__OPENCL_VERSION__)
static uint8_t futrts_mul_hi8(uint8_t a, uint8_t b)
{
    return mul_hi(a, b);
}
static uint16_t futrts_mul_hi16(uint16_t a, uint16_t b)
{
    return mul_hi(a, b);
}
static uint32_t futrts_mul_hi32(uint32_t a, uint32_t b)
{
    return mul_hi(a, b);
}
static uint64_t futrts_mul_hi64(uint64_t a, uint64_t b)
{
    return mul_hi(a, b);
}
#elif defined(__CUDA_ARCH__)
static uint8_t futrts_mul_hi8(uint8_t a, uint8_t b)
{
    uint16_t aa = a;
    uint16_t bb = b;
    
    return aa * bb >> 8;
}
static uint16_t futrts_mul_hi16(uint16_t a, uint16_t b)
{
    uint32_t aa = a;
    uint32_t bb = b;
    
    return aa * bb >> 16;
}
static uint32_t futrts_mul_hi32(uint32_t a, uint32_t b)
{
    return mulhi(a, b);
}
static uint64_t futrts_mul_hi64(uint64_t a, uint64_t b)
{
    return mul64hi(a, b);
}
#else
static uint8_t futrts_mul_hi8(uint8_t a, uint8_t b)
{
    uint16_t aa = a;
    uint16_t bb = b;
    
    return aa * bb >> 8;
}
static uint16_t futrts_mul_hi16(uint16_t a, uint16_t b)
{
    uint32_t aa = a;
    uint32_t bb = b;
    
    return aa * bb >> 16;
}
static uint32_t futrts_mul_hi32(uint32_t a, uint32_t b)
{
    uint64_t aa = a;
    uint64_t bb = b;
    
    return aa * bb >> 32;
}
static uint64_t futrts_mul_hi64(uint64_t a, uint64_t b)
{
    __uint128_t aa = a;
    __uint128_t bb = b;
    
    return aa * bb >> 64;
}
#endif
#if defined(__OPENCL_VERSION__)
static uint8_t futrts_mad_hi8(uint8_t a, uint8_t b, uint8_t c)
{
    return mad_hi(a, b, c);
}
static uint16_t futrts_mad_hi16(uint16_t a, uint16_t b, uint16_t c)
{
    return mad_hi(a, b, c);
}
static uint32_t futrts_mad_hi32(uint32_t a, uint32_t b, uint32_t c)
{
    return mad_hi(a, b, c);
}
static uint64_t futrts_mad_hi64(uint64_t a, uint64_t b, uint64_t c)
{
    return mad_hi(a, b, c);
}
#else
static uint8_t futrts_mad_hi8(uint8_t a, uint8_t b, uint8_t c)
{
    return futrts_mul_hi8(a, b) + c;
}
static uint16_t futrts_mad_hi16(uint16_t a, uint16_t b, uint16_t c)
{
    return futrts_mul_hi16(a, b) + c;
}
static uint32_t futrts_mad_hi32(uint32_t a, uint32_t b, uint32_t c)
{
    return futrts_mul_hi32(a, b) + c;
}
static uint64_t futrts_mad_hi64(uint64_t a, uint64_t b, uint64_t c)
{
    return futrts_mul_hi64(a, b) + c;
}
#endif
#if defined(__OPENCL_VERSION__)
static int32_t futrts_clzz8(int8_t x)
{
    return clz(x);
}
static int32_t futrts_clzz16(int16_t x)
{
    return clz(x);
}
static int32_t futrts_clzz32(int32_t x)
{
    return clz(x);
}
static int32_t futrts_clzz64(int64_t x)
{
    return clz(x);
}
#elif defined(__CUDA_ARCH__)
static int32_t futrts_clzz8(int8_t x)
{
    return __clz(zext_i8_i32(x)) - 24;
}
static int32_t futrts_clzz16(int16_t x)
{
    return __clz(zext_i16_i32(x)) - 16;
}
static int32_t futrts_clzz32(int32_t x)
{
    return __clz(x);
}
static int32_t futrts_clzz64(int64_t x)
{
    return __clzll(x);
}
#else
static int32_t futrts_clzz8(int8_t x)
{
    int n = 0;
    int bits = sizeof(x) * 8;
    
    for (int i = 0; i < bits; i++) {
        if (x < 0)
            break;
        n++;
        x <<= 1;
    }
    return n;
}
static int32_t futrts_clzz16(int16_t x)
{
    int n = 0;
    int bits = sizeof(x) * 8;
    
    for (int i = 0; i < bits; i++) {
        if (x < 0)
            break;
        n++;
        x <<= 1;
    }
    return n;
}
static int32_t futrts_clzz32(int32_t x)
{
    int n = 0;
    int bits = sizeof(x) * 8;
    
    for (int i = 0; i < bits; i++) {
        if (x < 0)
            break;
        n++;
        x <<= 1;
    }
    return n;
}
static int32_t futrts_clzz64(int64_t x)
{
    int n = 0;
    int bits = sizeof(x) * 8;
    
    for (int i = 0; i < bits; i++) {
        if (x < 0)
            break;
        n++;
        x <<= 1;
    }
    return n;
}
#endif
static inline float fdiv32(float x, float y)
{
    return x / y;
}
static inline float fadd32(float x, float y)
{
    return x + y;
}
static inline float fsub32(float x, float y)
{
    return x - y;
}
static inline float fmul32(float x, float y)
{
    return x * y;
}
static inline float fmin32(float x, float y)
{
    return fmin(x, y);
}
static inline float fmax32(float x, float y)
{
    return fmax(x, y);
}
static inline float fpow32(float x, float y)
{
    return pow(x, y);
}
static inline bool cmplt32(float x, float y)
{
    return x < y;
}
static inline bool cmple32(float x, float y)
{
    return x <= y;
}
static inline float sitofp_i8_f32(int8_t x)
{
    return (float) x;
}
static inline float sitofp_i16_f32(int16_t x)
{
    return (float) x;
}
static inline float sitofp_i32_f32(int32_t x)
{
    return (float) x;
}
static inline float sitofp_i64_f32(int64_t x)
{
    return (float) x;
}
static inline float uitofp_i8_f32(uint8_t x)
{
    return (float) x;
}
static inline float uitofp_i16_f32(uint16_t x)
{
    return (float) x;
}
static inline float uitofp_i32_f32(uint32_t x)
{
    return (float) x;
}
static inline float uitofp_i64_f32(uint64_t x)
{
    return (float) x;
}
static inline int8_t fptosi_f32_i8(float x)
{
    return (int8_t) x;
}
static inline int16_t fptosi_f32_i16(float x)
{
    return (int16_t) x;
}
static inline int32_t fptosi_f32_i32(float x)
{
    return (int32_t) x;
}
static inline int64_t fptosi_f32_i64(float x)
{
    return (int64_t) x;
}
static inline uint8_t fptoui_f32_i8(float x)
{
    return (uint8_t) x;
}
static inline uint16_t fptoui_f32_i16(float x)
{
    return (uint16_t) x;
}
static inline uint32_t fptoui_f32_i32(float x)
{
    return (uint32_t) x;
}
static inline uint64_t fptoui_f32_i64(float x)
{
    return (uint64_t) x;
}
static inline double fdiv64(double x, double y)
{
    return x / y;
}
static inline double fadd64(double x, double y)
{
    return x + y;
}
static inline double fsub64(double x, double y)
{
    return x - y;
}
static inline double fmul64(double x, double y)
{
    return x * y;
}
static inline double fmin64(double x, double y)
{
    return fmin(x, y);
}
static inline double fmax64(double x, double y)
{
    return fmax(x, y);
}
static inline double fpow64(double x, double y)
{
    return pow(x, y);
}
static inline bool cmplt64(double x, double y)
{
    return x < y;
}
static inline bool cmple64(double x, double y)
{
    return x <= y;
}
static inline double sitofp_i8_f64(int8_t x)
{
    return (double) x;
}
static inline double sitofp_i16_f64(int16_t x)
{
    return (double) x;
}
static inline double sitofp_i32_f64(int32_t x)
{
    return (double) x;
}
static inline double sitofp_i64_f64(int64_t x)
{
    return (double) x;
}
static inline double uitofp_i8_f64(uint8_t x)
{
    return (double) x;
}
static inline double uitofp_i16_f64(uint16_t x)
{
    return (double) x;
}
static inline double uitofp_i32_f64(uint32_t x)
{
    return (double) x;
}
static inline double uitofp_i64_f64(uint64_t x)
{
    return (double) x;
}
static inline int8_t fptosi_f64_i8(double x)
{
    return (int8_t) x;
}
static inline int16_t fptosi_f64_i16(double x)
{
    return (int16_t) x;
}
static inline int32_t fptosi_f64_i32(double x)
{
    return (int32_t) x;
}
static inline int64_t fptosi_f64_i64(double x)
{
    return (int64_t) x;
}
static inline uint8_t fptoui_f64_i8(double x)
{
    return (uint8_t) x;
}
static inline uint16_t fptoui_f64_i16(double x)
{
    return (uint16_t) x;
}
static inline uint32_t fptoui_f64_i32(double x)
{
    return (uint32_t) x;
}
static inline uint64_t fptoui_f64_i64(double x)
{
    return (uint64_t) x;
}
static inline float fpconv_f32_f32(float x)
{
    return (float) x;
}
static inline double fpconv_f32_f64(float x)
{
    return (double) x;
}
static inline float fpconv_f64_f32(double x)
{
    return (float) x;
}
static inline double fpconv_f64_f64(double x)
{
    return (double) x;
}
static inline float futrts_log32(float x)
{
    return log(x);
}
static inline float futrts_log2_32(float x)
{
    return log2(x);
}
static inline float futrts_log10_32(float x)
{
    return log10(x);
}
static inline float futrts_sqrt32(float x)
{
    return sqrt(x);
}
static inline float futrts_exp32(float x)
{
    return exp(x);
}
static inline float futrts_cos32(float x)
{
    return cos(x);
}
static inline float futrts_sin32(float x)
{
    return sin(x);
}
static inline float futrts_tan32(float x)
{
    return tan(x);
}
static inline float futrts_acos32(float x)
{
    return acos(x);
}
static inline float futrts_asin32(float x)
{
    return asin(x);
}
static inline float futrts_atan32(float x)
{
    return atan(x);
}
static inline float futrts_cosh32(float x)
{
    return cosh(x);
}
static inline float futrts_sinh32(float x)
{
    return sinh(x);
}
static inline float futrts_tanh32(float x)
{
    return tanh(x);
}
static inline float futrts_acosh32(float x)
{
    return acosh(x);
}
static inline float futrts_asinh32(float x)
{
    return asinh(x);
}
static inline float futrts_atanh32(float x)
{
    return atanh(x);
}
static inline float futrts_atan2_32(float x, float y)
{
    return atan2(x, y);
}
static inline float futrts_gamma32(float x)
{
    return tgamma(x);
}
static inline float futrts_lgamma32(float x)
{
    return lgamma(x);
}
static inline bool futrts_isnan32(float x)
{
    return isnan(x);
}
static inline bool futrts_isinf32(float x)
{
    return isinf(x);
}
static inline int32_t futrts_to_bits32(float x)
{
    union {
        float f;
        int32_t t;
    } p;
    
    p.f = x;
    return p.t;
}
static inline float futrts_from_bits32(int32_t x)
{
    union {
        int32_t f;
        float t;
    } p;
    
    p.f = x;
    return p.t;
}
#ifdef __OPENCL_VERSION__
static inline float fmod32(float x, float y)
{
    return fmod(x, y);
}
static inline float futrts_round32(float x)
{
    return rint(x);
}
static inline float futrts_floor32(float x)
{
    return floor(x);
}
static inline float futrts_ceil32(float x)
{
    return ceil(x);
}
static inline float futrts_lerp32(float v0, float v1, float t)
{
    return mix(v0, v1, t);
}
static inline float futrts_mad32(float a, float b, float c)
{
    return mad(a, b, c);
}
static inline float futrts_fma32(float a, float b, float c)
{
    return fma(a, b, c);
}
#else
static inline float fmod32(float x, float y)
{
    return fmodf(x, y);
}
static inline float futrts_round32(float x)
{
    return rintf(x);
}
static inline float futrts_floor32(float x)
{
    return floorf(x);
}
static inline float futrts_ceil32(float x)
{
    return ceilf(x);
}
static inline float futrts_lerp32(float v0, float v1, float t)
{
    return v0 + (v1 - v0) * t;
}
static inline float futrts_mad32(float a, float b, float c)
{
    return a * b + c;
}
static inline float futrts_fma32(float a, float b, float c)
{
    return fmaf(a, b, c);
}
#endif
static inline double futrts_log64(double x)
{
    return log(x);
}
static inline double futrts_log2_64(double x)
{
    return log2(x);
}
static inline double futrts_log10_64(double x)
{
    return log10(x);
}
static inline double futrts_sqrt64(double x)
{
    return sqrt(x);
}
static inline double futrts_exp64(double x)
{
    return exp(x);
}
static inline double futrts_cos64(double x)
{
    return cos(x);
}
static inline double futrts_sin64(double x)
{
    return sin(x);
}
static inline double futrts_tan64(double x)
{
    return tan(x);
}
static inline double futrts_acos64(double x)
{
    return acos(x);
}
static inline double futrts_asin64(double x)
{
    return asin(x);
}
static inline double futrts_atan64(double x)
{
    return atan(x);
}
static inline double futrts_cosh64(double x)
{
    return cosh(x);
}
static inline double futrts_sinh64(double x)
{
    return sinh(x);
}
static inline double futrts_tanh64(double x)
{
    return tanh(x);
}
static inline double futrts_acosh64(double x)
{
    return acosh(x);
}
static inline double futrts_asinh64(double x)
{
    return asinh(x);
}
static inline double futrts_atanh64(double x)
{
    return atanh(x);
}
static inline double futrts_atan2_64(double x, double y)
{
    return atan2(x, y);
}
static inline double futrts_gamma64(double x)
{
    return tgamma(x);
}
static inline double futrts_lgamma64(double x)
{
    return lgamma(x);
}
static inline double futrts_fma64(double a, double b, double c)
{
    return fma(a, b, c);
}
static inline double futrts_round64(double x)
{
    return rint(x);
}
static inline double futrts_ceil64(double x)
{
    return ceil(x);
}
static inline double futrts_floor64(double x)
{
    return floor(x);
}
static inline bool futrts_isnan64(double x)
{
    return isnan(x);
}
static inline bool futrts_isinf64(double x)
{
    return isinf(x);
}
static inline int64_t futrts_to_bits64(double x)
{
    union {
        double f;
        int64_t t;
    } p;
    
    p.f = x;
    return p.t;
}
static inline double futrts_from_bits64(int64_t x)
{
    union {
        int64_t f;
        double t;
    } p;
    
    p.f = x;
    return p.t;
}
static inline double fmod64(double x, double y)
{
    return fmod(x, y);
}
#ifdef __OPENCL_VERSION__
static inline double futrts_lerp64(double v0, double v1, double t)
{
    return mix(v0, v1, t);
}
static inline double futrts_mad64(double a, double b, double c)
{
    return mad(a, b, c);
}
#else
static inline double futrts_lerp64(double v0, double v1, double t)
{
    return v0 + (v1 - v0) * t;
}
static inline double futrts_mad64(double a, double b, double c)
{
    return a * b + c;
}
#endif
int init_constants(struct futhark_context *);
int free_constants(struct futhark_context *);
struct memblock_device {
    int *references;
    cl_mem mem;
    int64_t size;
    const char *desc;
} ;
struct memblock {
    int *references;
    char *mem;
    int64_t size;
    const char *desc;
} ;
typedef cl_mem fl_mem_t;
// Start of free_list.h.

/* An entry in the free list.  May be invalid, to avoid having to
   deallocate entries as soon as they are removed.  There is also a
   tag, to help with memory reuse. */
struct free_list_entry {
  size_t size;
  fl_mem_t mem;
  const char *tag;
  unsigned char valid;
};

struct free_list {
  struct free_list_entry *entries;        // Pointer to entries.
  int capacity;                           // Number of entries.
  int used;                               // Number of valid entries.
};

static void free_list_init(struct free_list *l) {
  l->capacity = 30; // Picked arbitrarily.
  l->used = 0;
  l->entries = (struct free_list_entry*) malloc(sizeof(struct free_list_entry) * l->capacity);
  for (int i = 0; i < l->capacity; i++) {
    l->entries[i].valid = 0;
  }
}

/* Remove invalid entries from the free list. */
static void free_list_pack(struct free_list *l) {
  int p = 0;
  for (int i = 0; i < l->capacity; i++) {
    if (l->entries[i].valid) {
      l->entries[p] = l->entries[i];
      p++;
    }
  }

  // Now p is the number of used elements.  We don't want it to go
  // less than the default capacity (although in practice it's OK as
  // long as it doesn't become 1).
  if (p < 30) {
    p = 30;
  }
  l->entries = realloc(l->entries, p * sizeof(struct free_list_entry));
  l->capacity = p;
}

static void free_list_destroy(struct free_list *l) {
  assert(l->used == 0);
  free(l->entries);
}

static int free_list_find_invalid(struct free_list *l) {
  int i;
  for (i = 0; i < l->capacity; i++) {
    if (!l->entries[i].valid) {
      break;
    }
  }
  return i;
}

static void free_list_insert(struct free_list *l, size_t size, fl_mem_t mem, const char *tag) {
  int i = free_list_find_invalid(l);

  if (i == l->capacity) {
    // List is full; so we have to grow it.
    int new_capacity = l->capacity * 2 * sizeof(struct free_list_entry);
    l->entries = realloc(l->entries, new_capacity);
    for (int j = 0; j < l->capacity; j++) {
      l->entries[j+l->capacity].valid = 0;
    }
    l->capacity *= 2;
  }

  // Now 'i' points to the first invalid entry.
  l->entries[i].valid = 1;
  l->entries[i].size = size;
  l->entries[i].mem = mem;
  l->entries[i].tag = tag;

  l->used++;
}

/* Find and remove a memory block of at least the desired size and
   tag.  Returns 0 on success.  */
static int free_list_find(struct free_list *l, const char *tag, size_t *size_out, fl_mem_t *mem_out) {
  int i;
  for (i = 0; i < l->capacity; i++) {
    if (l->entries[i].valid && l->entries[i].tag == tag) {
      l->entries[i].valid = 0;
      *size_out = l->entries[i].size;
      *mem_out = l->entries[i].mem;
      l->used--;
      return 0;
    }
  }

  return 1;
}

/* Remove the first block in the free list.  Returns 0 if a block was
   removed, and nonzero if the free list was already empty. */
static int free_list_first(struct free_list *l, fl_mem_t *mem_out) {
  for (int i = 0; i < l->capacity; i++) {
    if (l->entries[i].valid) {
      l->entries[i].valid = 0;
      *mem_out = l->entries[i].mem;
      l->used--;
      return 0;
    }
  }

  return 1;
}

// End of free_list.h.

// Start of opencl.h.

#define OPENCL_SUCCEED_FATAL(e) opencl_succeed_fatal(e, #e, __FILE__, __LINE__)
#define OPENCL_SUCCEED_NONFATAL(e) opencl_succeed_nonfatal(e, #e, __FILE__, __LINE__)
// Take care not to override an existing error.
#define OPENCL_SUCCEED_OR_RETURN(e) {             \
    char *serror = OPENCL_SUCCEED_NONFATAL(e);    \
    if (serror) {                                 \
      if (!ctx->error) {                          \
        ctx->error = serror;                      \
        return bad;                               \
      } else {                                    \
        free(serror);                             \
      }                                           \
    }                                             \
  }

// OPENCL_SUCCEED_OR_RETURN returns the value of the variable 'bad' in
// scope.  By default, it will be this one.  Create a local variable
// of some other type if needed.  This is a bit of a hack, but it
// saves effort in the code generator.
static const int bad = 1;

struct opencl_config {
  int debugging;
  int profiling;
  int logging;
  int preferred_device_num;
  const char *preferred_platform;
  const char *preferred_device;
  int ignore_blacklist;

  const char* dump_program_to;
  const char* load_program_from;
  const char* dump_binary_to;
  const char* load_binary_from;

  size_t default_group_size;
  size_t default_num_groups;
  size_t default_tile_size;
  size_t default_threshold;

  int default_group_size_changed;
  int default_tile_size_changed;

  int num_sizes;
  const char **size_names;
  const char **size_vars;
  size_t *size_values;
  const char **size_classes;
};

static void opencl_config_init(struct opencl_config *cfg,
                               int num_sizes,
                               const char *size_names[],
                               const char *size_vars[],
                               size_t *size_values,
                               const char *size_classes[]) {
  cfg->debugging = 0;
  cfg->logging = 0;
  cfg->profiling = 0;
  cfg->preferred_device_num = 0;
  cfg->preferred_platform = "";
  cfg->preferred_device = "";
  cfg->ignore_blacklist = 0;
  cfg->dump_program_to = NULL;
  cfg->load_program_from = NULL;
  cfg->dump_binary_to = NULL;
  cfg->load_binary_from = NULL;

  // The following are dummy sizes that mean the concrete defaults
  // will be set during initialisation via hardware-inspection-based
  // heuristics.
  cfg->default_group_size = 0;
  cfg->default_num_groups = 0;
  cfg->default_tile_size = 0;
  cfg->default_threshold = 0;

  cfg->default_group_size_changed = 0;
  cfg->default_tile_size_changed = 0;

  cfg->num_sizes = num_sizes;
  cfg->size_names = size_names;
  cfg->size_vars = size_vars;
  cfg->size_values = size_values;
  cfg->size_classes = size_classes;
}

// A record of something that happened.
struct profiling_record {
  cl_event *event;
  int *runs;
  int64_t *runtime;
};

struct opencl_context {
  cl_device_id device;
  cl_context ctx;
  cl_command_queue queue;

  struct opencl_config cfg;

  struct free_list free_list;

  size_t max_group_size;
  size_t max_num_groups;
  size_t max_tile_size;
  size_t max_threshold;
  size_t max_local_memory;

  size_t lockstep_width;

  struct profiling_record *profiling_records;
  int profiling_records_capacity;
  int profiling_records_used;
};

struct opencl_device_option {
  cl_platform_id platform;
  cl_device_id device;
  cl_device_type device_type;
  char *platform_name;
  char *device_name;
};

/* This function must be defined by the user.  It is invoked by
   setup_opencl() after the platform and device has been found, but
   before the program is loaded.  Its intended use is to tune
   constants based on the selected platform and device. */
static void post_opencl_setup(struct opencl_context*, struct opencl_device_option*);

static char *strclone(const char *str) {
  size_t size = strlen(str) + 1;
  char *copy = (char*) malloc(size);
  if (copy == NULL) {
    return NULL;
  }

  memcpy(copy, str, size);
  return copy;
}

// Read a file into a NUL-terminated string; returns NULL on error.
static char* slurp_file(const char *filename, size_t *size) {
  char *s;
  FILE *f = fopen(filename, "rb"); // To avoid Windows messing with linebreaks.
  if (f == NULL) return NULL;
  fseek(f, 0, SEEK_END);
  size_t src_size = ftell(f);
  fseek(f, 0, SEEK_SET);
  s = (char*) malloc(src_size + 1);
  if (fread(s, 1, src_size, f) != src_size) {
    free(s);
    s = NULL;
  } else {
    s[src_size] = '\0';
  }
  fclose(f);

  if (size) {
    *size = src_size;
  }

  return s;
}

static const char* opencl_error_string(cl_int err)
{
    switch (err) {
        case CL_SUCCESS:                            return "Success!";
        case CL_DEVICE_NOT_FOUND:                   return "Device not found.";
        case CL_DEVICE_NOT_AVAILABLE:               return "Device not available";
        case CL_COMPILER_NOT_AVAILABLE:             return "Compiler not available";
        case CL_MEM_OBJECT_ALLOCATION_FAILURE:      return "Memory object allocation failure";
        case CL_OUT_OF_RESOURCES:                   return "Out of resources";
        case CL_OUT_OF_HOST_MEMORY:                 return "Out of host memory";
        case CL_PROFILING_INFO_NOT_AVAILABLE:       return "Profiling information not available";
        case CL_MEM_COPY_OVERLAP:                   return "Memory copy overlap";
        case CL_IMAGE_FORMAT_MISMATCH:              return "Image format mismatch";
        case CL_IMAGE_FORMAT_NOT_SUPPORTED:         return "Image format not supported";
        case CL_BUILD_PROGRAM_FAILURE:              return "Program build failure";
        case CL_MAP_FAILURE:                        return "Map failure";
        case CL_INVALID_VALUE:                      return "Invalid value";
        case CL_INVALID_DEVICE_TYPE:                return "Invalid device type";
        case CL_INVALID_PLATFORM:                   return "Invalid platform";
        case CL_INVALID_DEVICE:                     return "Invalid device";
        case CL_INVALID_CONTEXT:                    return "Invalid context";
        case CL_INVALID_QUEUE_PROPERTIES:           return "Invalid queue properties";
        case CL_INVALID_COMMAND_QUEUE:              return "Invalid command queue";
        case CL_INVALID_HOST_PTR:                   return "Invalid host pointer";
        case CL_INVALID_MEM_OBJECT:                 return "Invalid memory object";
        case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:    return "Invalid image format descriptor";
        case CL_INVALID_IMAGE_SIZE:                 return "Invalid image size";
        case CL_INVALID_SAMPLER:                    return "Invalid sampler";
        case CL_INVALID_BINARY:                     return "Invalid binary";
        case CL_INVALID_BUILD_OPTIONS:              return "Invalid build options";
        case CL_INVALID_PROGRAM:                    return "Invalid program";
        case CL_INVALID_PROGRAM_EXECUTABLE:         return "Invalid program executable";
        case CL_INVALID_KERNEL_NAME:                return "Invalid kernel name";
        case CL_INVALID_KERNEL_DEFINITION:          return "Invalid kernel definition";
        case CL_INVALID_KERNEL:                     return "Invalid kernel";
        case CL_INVALID_ARG_INDEX:                  return "Invalid argument index";
        case CL_INVALID_ARG_VALUE:                  return "Invalid argument value";
        case CL_INVALID_ARG_SIZE:                   return "Invalid argument size";
        case CL_INVALID_KERNEL_ARGS:                return "Invalid kernel arguments";
        case CL_INVALID_WORK_DIMENSION:             return "Invalid work dimension";
        case CL_INVALID_WORK_GROUP_SIZE:            return "Invalid work group size";
        case CL_INVALID_WORK_ITEM_SIZE:             return "Invalid work item size";
        case CL_INVALID_GLOBAL_OFFSET:              return "Invalid global offset";
        case CL_INVALID_EVENT_WAIT_LIST:            return "Invalid event wait list";
        case CL_INVALID_EVENT:                      return "Invalid event";
        case CL_INVALID_OPERATION:                  return "Invalid operation";
        case CL_INVALID_GL_OBJECT:                  return "Invalid OpenGL object";
        case CL_INVALID_BUFFER_SIZE:                return "Invalid buffer size";
        case CL_INVALID_MIP_LEVEL:                  return "Invalid mip-map level";
        default:                                    return "Unknown";
    }
}

static void opencl_succeed_fatal(unsigned int ret,
                                 const char *call,
                                 const char *file,
                                 int line) {
  if (ret != CL_SUCCESS) {
    futhark_panic(-1, "%s:%d: OpenCL call\n  %s\nfailed with error code %d (%s)\n",
          file, line, call, ret, opencl_error_string(ret));
  }
}

static char* opencl_succeed_nonfatal(unsigned int ret,
                                     const char *call,
                                     const char *file,
                                     int line) {
  if (ret != CL_SUCCESS) {
    return msgprintf("%s:%d: OpenCL call\n  %s\nfailed with error code %d (%s)\n",
                     file, line, call, ret, opencl_error_string(ret));
  } else {
    return NULL;
  }
}

static void set_preferred_platform(struct opencl_config *cfg, const char *s) {
  cfg->preferred_platform = s;
  cfg->ignore_blacklist = 1;
}

static void set_preferred_device(struct opencl_config *cfg, const char *s) {
  int x = 0;
  if (*s == '#') {
    s++;
    while (isdigit(*s)) {
      x = x * 10 + (*s++)-'0';
    }
    // Skip trailing spaces.
    while (isspace(*s)) {
      s++;
    }
  }
  cfg->preferred_device = s;
  cfg->preferred_device_num = x;
  cfg->ignore_blacklist = 1;
}

static char* opencl_platform_info(cl_platform_id platform,
                                  cl_platform_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED_FATAL(clGetPlatformInfo(platform, param, 0, NULL, &req_bytes));

  info = (char*) malloc(req_bytes);

  OPENCL_SUCCEED_FATAL(clGetPlatformInfo(platform, param, req_bytes, info, NULL));

  return info;
}

static char* opencl_device_info(cl_device_id device,
                                cl_device_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device, param, 0, NULL, &req_bytes));

  info = (char*) malloc(req_bytes);

  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device, param, req_bytes, info, NULL));

  return info;
}

static void opencl_all_device_options(struct opencl_device_option **devices_out,
                                      size_t *num_devices_out) {
  size_t num_devices = 0, num_devices_added = 0;

  cl_platform_id *all_platforms;
  cl_uint *platform_num_devices;

  cl_uint num_platforms;

  // Find the number of platforms.
  OPENCL_SUCCEED_FATAL(clGetPlatformIDs(0, NULL, &num_platforms));

  // Make room for them.
  all_platforms = calloc(num_platforms, sizeof(cl_platform_id));
  platform_num_devices = calloc(num_platforms, sizeof(cl_uint));

  // Fetch all the platforms.
  OPENCL_SUCCEED_FATAL(clGetPlatformIDs(num_platforms, all_platforms, NULL));

  // Count the number of devices for each platform, as well as the
  // total number of devices.
  for (cl_uint i = 0; i < num_platforms; i++) {
    if (clGetDeviceIDs(all_platforms[i], CL_DEVICE_TYPE_ALL,
                       0, NULL, &platform_num_devices[i]) == CL_SUCCESS) {
      num_devices += platform_num_devices[i];
    } else {
      platform_num_devices[i] = 0;
    }
  }

  // Make room for all the device options.
  struct opencl_device_option *devices =
    calloc(num_devices, sizeof(struct opencl_device_option));

  // Loop through the platforms, getting information about their devices.
  for (cl_uint i = 0; i < num_platforms; i++) {
    cl_platform_id platform = all_platforms[i];
    cl_uint num_platform_devices = platform_num_devices[i];

    if (num_platform_devices == 0) {
      continue;
    }

    char *platform_name = opencl_platform_info(platform, CL_PLATFORM_NAME);
    cl_device_id *platform_devices =
      calloc(num_platform_devices, sizeof(cl_device_id));

    // Fetch all the devices.
    OPENCL_SUCCEED_FATAL(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL,
                                  num_platform_devices, platform_devices, NULL));

    // Loop through the devices, adding them to the devices array.
    for (cl_uint i = 0; i < num_platform_devices; i++) {
      char *device_name = opencl_device_info(platform_devices[i], CL_DEVICE_NAME);
      devices[num_devices_added].platform = platform;
      devices[num_devices_added].device = platform_devices[i];
      OPENCL_SUCCEED_FATAL(clGetDeviceInfo(platform_devices[i], CL_DEVICE_TYPE,
                                     sizeof(cl_device_type),
                                     &devices[num_devices_added].device_type,
                                     NULL));
      // We don't want the structs to share memory, so copy the platform name.
      // Each device name is already unique.
      devices[num_devices_added].platform_name = strclone(platform_name);
      devices[num_devices_added].device_name = device_name;
      num_devices_added++;
    }
    free(platform_devices);
    free(platform_name);
  }
  free(all_platforms);
  free(platform_num_devices);

  *devices_out = devices;
  *num_devices_out = num_devices;
}

// Returns 0 on success.
static int select_device_interactively(struct opencl_config *cfg) {
  struct opencl_device_option *devices;
  size_t num_devices;
  int ret = 1;

  opencl_all_device_options(&devices, &num_devices);

  printf("Choose OpenCL device:\n");
  const char *cur_platform = "";
  for (size_t i = 0; i < num_devices; i++) {
    struct opencl_device_option device = devices[i];
    if (strcmp(cur_platform, device.platform_name) != 0) {
      printf("Platform: %s\n", device.platform_name);
      cur_platform = device.platform_name;
    }
    printf("[%d] %s\n", (int)i, device.device_name);
  }

  int selection;
  printf("Choice: ");
  if (scanf("%d", &selection) == 1) {
    ret = 0;
    cfg->preferred_platform = "";
    cfg->preferred_device = "";
    cfg->preferred_device_num = selection;
    cfg->ignore_blacklist = 1;
  }

  // Free all the platform and device names.
  for (size_t j = 0; j < num_devices; j++) {
    free(devices[j].platform_name);
    free(devices[j].device_name);
  }
  free(devices);

  return ret;
}

static int is_blacklisted(const char *platform_name, const char *device_name,
                          const struct opencl_config *cfg) {
  if (strcmp(cfg->preferred_platform, "") != 0 ||
      strcmp(cfg->preferred_device, "") != 0) {
    return 0;
  } else if (strstr(platform_name, "Apple") != NULL &&
             strstr(device_name, "Intel(R) Core(TM)") != NULL) {
    return 1;
  } else {
    return 0;
  }
}

static struct opencl_device_option get_preferred_device(const struct opencl_config *cfg) {
  struct opencl_device_option *devices;
  size_t num_devices;

  opencl_all_device_options(&devices, &num_devices);

  int num_device_matches = 0;

  for (size_t i = 0; i < num_devices; i++) {
    struct opencl_device_option device = devices[i];
    if (strstr(device.platform_name, cfg->preferred_platform) != NULL &&
        strstr(device.device_name, cfg->preferred_device) != NULL &&
        (cfg->ignore_blacklist ||
         !is_blacklisted(device.platform_name, device.device_name, cfg)) &&
        num_device_matches++ == cfg->preferred_device_num) {
      // Free all the platform and device names, except the ones we have chosen.
      for (size_t j = 0; j < num_devices; j++) {
        if (j != i) {
          free(devices[j].platform_name);
          free(devices[j].device_name);
        }
      }
      free(devices);
      return device;
    }
  }

  futhark_panic(1, "Could not find acceptable OpenCL device.\n");
  exit(1); // Never reached
}

static void describe_device_option(struct opencl_device_option device) {
  fprintf(stderr, "Using platform: %s\n", device.platform_name);
  fprintf(stderr, "Using device: %s\n", device.device_name);
}

static cl_build_status build_opencl_program(cl_program program, cl_device_id device, const char* options) {
  cl_int clBuildProgram_error = clBuildProgram(program, 1, &device, options, NULL, NULL);

  // Avoid termination due to CL_BUILD_PROGRAM_FAILURE
  if (clBuildProgram_error != CL_SUCCESS &&
      clBuildProgram_error != CL_BUILD_PROGRAM_FAILURE) {
    OPENCL_SUCCEED_FATAL(clBuildProgram_error);
  }

  cl_build_status build_status;
  OPENCL_SUCCEED_FATAL(clGetProgramBuildInfo(program,
                                             device,
                                             CL_PROGRAM_BUILD_STATUS,
                                             sizeof(cl_build_status),
                                             &build_status,
                                             NULL));

  if (build_status != CL_SUCCESS) {
    char *build_log;
    size_t ret_val_size;
    OPENCL_SUCCEED_FATAL(clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &ret_val_size));

    build_log = (char*) malloc(ret_val_size+1);
    OPENCL_SUCCEED_FATAL(clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, ret_val_size, build_log, NULL));

    // The spec technically does not say whether the build log is zero-terminated, so let's be careful.
    build_log[ret_val_size] = '\0';

    fprintf(stderr, "Build log:\n%s\n", build_log);

    free(build_log);
  }

  return build_status;
}

/* Fields in a bitmask indicating which types we must be sure are
   available. */
enum opencl_required_type { OPENCL_F64 = 1 };

// We take as input several strings representing the program, because
// C does not guarantee that the compiler supports particularly large
// literals.  Notably, Visual C has a limit of 2048 characters.  The
// array must be NULL-terminated.
static cl_program setup_opencl_with_command_queue(struct opencl_context *ctx,
                                                  cl_command_queue queue,
                                                  const char *srcs[],
                                                  int required_types,
                                                  const char *extra_build_opts[]) {
  int error;

  free_list_init(&ctx->free_list);
  ctx->queue = queue;

  OPENCL_SUCCEED_FATAL(clGetCommandQueueInfo(ctx->queue, CL_QUEUE_CONTEXT, sizeof(cl_context), &ctx->ctx, NULL));

  // Fill out the device info.  This is redundant work if we are
  // called from setup_opencl() (which is the common case), but I
  // doubt it matters much.
  struct opencl_device_option device_option;
  OPENCL_SUCCEED_FATAL(clGetCommandQueueInfo(ctx->queue, CL_QUEUE_DEVICE,
                                       sizeof(cl_device_id),
                                       &device_option.device,
                                       NULL));
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_PLATFORM,
                                 sizeof(cl_platform_id),
                                 &device_option.platform,
                                 NULL));
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_TYPE,
                                 sizeof(cl_device_type),
                                 &device_option.device_type,
                                 NULL));
  device_option.platform_name = opencl_platform_info(device_option.platform, CL_PLATFORM_NAME);
  device_option.device_name = opencl_device_info(device_option.device, CL_DEVICE_NAME);

  ctx->device = device_option.device;

  if (required_types & OPENCL_F64) {
    cl_uint supported;
    OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
                                   sizeof(cl_uint), &supported, NULL));
    if (!supported) {
      futhark_panic(1, "Program uses double-precision floats, but this is not supported on the chosen device: %s\n",
            device_option.device_name);
    }
  }

  size_t max_group_size;
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_MAX_WORK_GROUP_SIZE,
                                 sizeof(size_t), &max_group_size, NULL));

  size_t max_tile_size = sqrt(max_group_size);

  cl_ulong max_local_memory;
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_LOCAL_MEM_SIZE,
                                       sizeof(size_t), &max_local_memory, NULL));

  // Make sure this function is defined.
  post_opencl_setup(ctx, &device_option);

  if (max_group_size < ctx->cfg.default_group_size) {
    if (ctx->cfg.default_group_size_changed) {
      fprintf(stderr, "Note: Device limits default group size to %zu (down from %zu).\n",
              max_group_size, ctx->cfg.default_group_size);
    }
    ctx->cfg.default_group_size = max_group_size;
  }

  if (max_tile_size < ctx->cfg.default_tile_size) {
    if (ctx->cfg.default_tile_size_changed) {
      fprintf(stderr, "Note: Device limits default tile size to %zu (down from %zu).\n",
              max_tile_size, ctx->cfg.default_tile_size);
    }
    ctx->cfg.default_tile_size = max_tile_size;
  }

  ctx->max_group_size = max_group_size;
  ctx->max_tile_size = max_tile_size; // No limit.
  ctx->max_threshold = ctx->max_num_groups = 0; // No limit.
  ctx->max_local_memory = max_local_memory;

  // Now we go through all the sizes, clamp them to the valid range,
  // or set them to the default.
  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    const char *size_class = ctx->cfg.size_classes[i];
    size_t *size_value = &ctx->cfg.size_values[i];
    const char* size_name = ctx->cfg.size_names[i];
    size_t max_value, default_value;
    if (strstr(size_class, "group_size") == size_class) {
      max_value = max_group_size;
      default_value = ctx->cfg.default_group_size;
    } else if (strstr(size_class, "num_groups") == size_class) {
      max_value = max_group_size; // Futhark assumes this constraint.
      default_value = ctx->cfg.default_num_groups;
    } else if (strstr(size_class, "tile_size") == size_class) {
      max_value = sqrt(max_group_size);
      default_value = ctx->cfg.default_tile_size;
    } else if (strstr(size_class, "threshold") == size_class) {
      max_value = 0; // No limit.
      default_value = ctx->cfg.default_threshold;
    } else {
      // Bespoke sizes have no limit or default.
      max_value = 0;
    }
    if (*size_value == 0) {
      *size_value = default_value;
    } else if (max_value > 0 && *size_value > max_value) {
      fprintf(stderr, "Note: Device limits %s to %d (down from %d)\n",
              size_name, (int)max_value, (int)*size_value);
      *size_value = max_value;
    }
  }

  if (ctx->lockstep_width == 0) {
    ctx->lockstep_width = 1;
  }

  if (ctx->cfg.logging) {
    fprintf(stderr, "Lockstep width: %d\n", (int)ctx->lockstep_width);
    fprintf(stderr, "Default group size: %d\n", (int)ctx->cfg.default_group_size);
    fprintf(stderr, "Default number of groups: %d\n", (int)ctx->cfg.default_num_groups);
  }

  char *fut_opencl_src = NULL;
  cl_program prog;
  error = CL_SUCCESS;

  if (ctx->cfg.load_binary_from == NULL) {
    size_t src_size = 0;

    // Maybe we have to read OpenCL source from somewhere else (used for debugging).
    if (ctx->cfg.load_program_from != NULL) {
      fut_opencl_src = slurp_file(ctx->cfg.load_program_from, NULL);
      assert(fut_opencl_src != NULL);
    } else {
      // Construct the OpenCL source concatenating all the fragments.
      for (const char **src = srcs; src && *src; src++) {
        src_size += strlen(*src);
      }

      fut_opencl_src = (char*) malloc(src_size + 1);

      size_t n, i;
      for (i = 0, n = 0; srcs && srcs[i]; i++) {
        strncpy(fut_opencl_src+n, srcs[i], src_size-n);
        n += strlen(srcs[i]);
      }
      fut_opencl_src[src_size] = 0;
    }

    if (ctx->cfg.dump_program_to != NULL) {
      if (ctx->cfg.debugging) {
        fprintf(stderr, "Dumping OpenCL source to %s...\n", ctx->cfg.dump_program_to);
      }
      FILE *f = fopen(ctx->cfg.dump_program_to, "w");
      assert(f != NULL);
      fputs(fut_opencl_src, f);
      fclose(f);
    }

    if (ctx->cfg.debugging) {
      fprintf(stderr, "Creating OpenCL program...\n");
    }

    const char* src_ptr[] = {fut_opencl_src};
    prog = clCreateProgramWithSource(ctx->ctx, 1, src_ptr, &src_size, &error);
    OPENCL_SUCCEED_FATAL(error);
  } else {
    if (ctx->cfg.debugging) {
      fprintf(stderr, "Loading OpenCL binary from %s...\n", ctx->cfg.load_binary_from);
    }
    size_t binary_size;
    unsigned char *fut_opencl_bin =
      (unsigned char*) slurp_file(ctx->cfg.load_binary_from, &binary_size);
    assert(fut_opencl_bin != NULL);
    const unsigned char *binaries[1] = { fut_opencl_bin };
    cl_int status = 0;

    prog = clCreateProgramWithBinary(ctx->ctx, 1, &device_option.device,
                                     &binary_size, binaries,
                                     &status, &error);

    OPENCL_SUCCEED_FATAL(status);
    OPENCL_SUCCEED_FATAL(error);
  }

  int compile_opts_size = 1024;

  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    compile_opts_size += strlen(ctx->cfg.size_names[i]) + 20;
  }

  for (int i = 0; extra_build_opts[i] != NULL; i++) {
    compile_opts_size += strlen(extra_build_opts[i] + 1);
  }

  char *compile_opts = (char*) malloc(compile_opts_size);

  int w = snprintf(compile_opts, compile_opts_size,
                   "-DLOCKSTEP_WIDTH=%d ",
                   (int)ctx->lockstep_width);

  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    w += snprintf(compile_opts+w, compile_opts_size-w,
                  "-D%s=%d ",
                  ctx->cfg.size_vars[i],
                  (int)ctx->cfg.size_values[i]);
  }

  for (int i = 0; extra_build_opts[i] != NULL; i++) {
    w += snprintf(compile_opts+w, compile_opts_size-w,
                  "%s ", extra_build_opts[i]);
  }

  if (ctx->cfg.debugging) {
    fprintf(stderr, "Building OpenCL program...\n");
  }
  OPENCL_SUCCEED_FATAL(build_opencl_program(prog, device_option.device, compile_opts));

  free(compile_opts);
  free(fut_opencl_src);

  if (ctx->cfg.dump_binary_to != NULL) {
    if (ctx->cfg.debugging) {
      fprintf(stderr, "Dumping OpenCL binary to %s...\n", ctx->cfg.dump_binary_to);
    }

    size_t binary_size;
    OPENCL_SUCCEED_FATAL(clGetProgramInfo(prog, CL_PROGRAM_BINARY_SIZES,
                                          sizeof(size_t), &binary_size, NULL));
    unsigned char *binary = (unsigned char*) malloc(binary_size);
    unsigned char *binaries[1] = { binary };
    OPENCL_SUCCEED_FATAL(clGetProgramInfo(prog, CL_PROGRAM_BINARIES,
                                          sizeof(unsigned char*), binaries, NULL));

    FILE *f = fopen(ctx->cfg.dump_binary_to, "w");
    assert(f != NULL);
    fwrite(binary, sizeof(char), binary_size, f);
    fclose(f);
  }

  return prog;
}

static cl_program setup_opencl(struct opencl_context *ctx,
                               const char *srcs[],
                               int required_types,
                               const char *extra_build_opts[]) {

  ctx->lockstep_width = 0; // Real value set later.

  struct opencl_device_option device_option = get_preferred_device(&ctx->cfg);

  if (ctx->cfg.logging) {
    describe_device_option(device_option);
  }

  // Note that NVIDIA's OpenCL requires the platform property
  cl_context_properties properties[] = {
    CL_CONTEXT_PLATFORM,
    (cl_context_properties)device_option.platform,
    0
  };

  cl_int clCreateContext_error;
  ctx->ctx = clCreateContext(properties, 1, &device_option.device, NULL, NULL, &clCreateContext_error);
  OPENCL_SUCCEED_FATAL(clCreateContext_error);

  cl_int clCreateCommandQueue_error;
  cl_command_queue queue =
    clCreateCommandQueue(ctx->ctx,
                         device_option.device,
                         ctx->cfg.profiling ? CL_QUEUE_PROFILING_ENABLE : 0,
                         &clCreateCommandQueue_error);
  OPENCL_SUCCEED_FATAL(clCreateCommandQueue_error);

  return setup_opencl_with_command_queue(ctx, queue, srcs, required_types, extra_build_opts);
}

// Count up the runtime all the profiling_records that occured during execution.
// Also clears the buffer of profiling_records.
static cl_int opencl_tally_profiling_records(struct opencl_context *ctx) {
  cl_int err;
  for (int i = 0; i < ctx->profiling_records_used; i++) {
    struct profiling_record record = ctx->profiling_records[i];

    cl_ulong start_t, end_t;

    if ((err = clGetEventProfilingInfo(*record.event,
                                       CL_PROFILING_COMMAND_START,
                                       sizeof(start_t),
                                       &start_t,
                                       NULL)) != CL_SUCCESS) {
      return err;
    }

    if ((err = clGetEventProfilingInfo(*record.event,
                                       CL_PROFILING_COMMAND_END,
                                       sizeof(end_t),
                                       &end_t,
                                       NULL)) != CL_SUCCESS) {
      return err;
    }

    // OpenCL provides nanosecond resolution, but we want
    // microseconds.
    *record.runs += 1;
    *record.runtime += (end_t - start_t)/1000;

    if ((err = clReleaseEvent(*record.event)) != CL_SUCCESS) {
      return err;
    }
    free(record.event);
  }

  ctx->profiling_records_used = 0;

  return CL_SUCCESS;
}

// If profiling, produce an event associated with a profiling record.
static cl_event* opencl_get_event(struct opencl_context *ctx, int *runs, int64_t *runtime) {
    if (ctx->profiling_records_used == ctx->profiling_records_capacity) {
      ctx->profiling_records_capacity *= 2;
      ctx->profiling_records =
        realloc(ctx->profiling_records,
                ctx->profiling_records_capacity *
                sizeof(struct profiling_record));
    }
    cl_event *event = malloc(sizeof(cl_event));
    ctx->profiling_records[ctx->profiling_records_used].event = event;
    ctx->profiling_records[ctx->profiling_records_used].runs = runs;
    ctx->profiling_records[ctx->profiling_records_used].runtime = runtime;
    ctx->profiling_records_used++;
    return event;
}

// Allocate memory from driver. The problem is that OpenCL may perform
// lazy allocation, so we cannot know whether an allocation succeeded
// until the first time we try to use it.  Hence we immediately
// perform a write to see if the allocation succeeded.  This is slow,
// but the assumption is that this operation will be rare (most things
// will go through the free list).
static int opencl_alloc_actual(struct opencl_context *ctx, size_t size, cl_mem *mem_out) {
  int error;
  *mem_out = clCreateBuffer(ctx->ctx, CL_MEM_READ_WRITE, size, NULL, &error);

  if (error != CL_SUCCESS) {
    return error;
  }

  int x = 2;
  error = clEnqueueWriteBuffer(ctx->queue, *mem_out, 1, 0, sizeof(x), &x, 0, NULL, NULL);

  // No need to wait for completion here. clWaitForEvents() cannot
  // return mem object allocation failures. This implies that the
  // buffer is faulted onto the device on enqueue. (Observation by
  // Andreas Kloeckner.)

  return error;
}

static int opencl_alloc(struct opencl_context *ctx, size_t min_size, const char *tag, cl_mem *mem_out) {
  if (min_size < sizeof(int)) {
    min_size = sizeof(int);
  }

  size_t size;

  if (free_list_find(&ctx->free_list, tag, &size, mem_out) == 0) {
    // Successfully found a free block.  Is it big enough?
    //
    // FIXME: we might also want to check whether the block is *too
    // big*, to avoid internal fragmentation.  However, this can
    // sharply impact performance on programs where arrays change size
    // frequently.  Fortunately, such allocations are usually fairly
    // short-lived, as they are necessarily within a loop, so the risk
    // of internal fragmentation resulting in an OOM situation is
    // limited.  However, it would be preferable if we could go back
    // and *shrink* oversize allocations when we encounter an OOM
    // condition.  That is technically feasible, since we do not
    // expose OpenCL pointer values directly to the application, but
    // instead rely on a level of indirection.
    if (size >= min_size) {
      return CL_SUCCESS;
    } else {
      // Not just right - free it.
      int error = clReleaseMemObject(*mem_out);
      if (error != CL_SUCCESS) {
        return error;
      }
    }
  }

  // We have to allocate a new block from the driver.  If the
  // allocation does not succeed, then we might be in an out-of-memory
  // situation.  We now start freeing things from the free list until
  // we think we have freed enough that the allocation will succeed.
  // Since we don't know how far the allocation is from fitting, we
  // have to check after every deallocation.  This might be pretty
  // expensive.  Let's hope that this case is hit rarely.

  int error = opencl_alloc_actual(ctx, min_size, mem_out);

  while (error == CL_MEM_OBJECT_ALLOCATION_FAILURE) {
    if (ctx->cfg.debugging) {
      fprintf(stderr, "Out of OpenCL memory: releasing entry from the free list...\n");
    }
    cl_mem mem;
    if (free_list_first(&ctx->free_list, &mem) == 0) {
      error = clReleaseMemObject(mem);
      if (error != CL_SUCCESS) {
        return error;
      }
    } else {
      break;
    }
    error = opencl_alloc_actual(ctx, min_size, mem_out);
  }

  return error;
}

static int opencl_free(struct opencl_context *ctx, cl_mem mem, const char *tag) {
  size_t size;
  cl_mem existing_mem;

  // If there is already a block with this tag, then remove it.
  if (free_list_find(&ctx->free_list, tag, &size, &existing_mem) == 0) {
    int error = clReleaseMemObject(existing_mem);
    if (error != CL_SUCCESS) {
      return error;
    }
  }

  int error = clGetMemObjectInfo(mem, CL_MEM_SIZE, sizeof(size_t), &size, NULL);

  if (error == CL_SUCCESS) {
    free_list_insert(&ctx->free_list, size, mem, tag);
  }

  return error;
}

static int opencl_free_all(struct opencl_context *ctx) {
  cl_mem mem;
  free_list_pack(&ctx->free_list);
  while (free_list_first(&ctx->free_list, &mem) == 0) {
    int error = clReleaseMemObject(mem);
    if (error != CL_SUCCESS) {
      return error;
    }
  }

  return CL_SUCCESS;
}

// End of opencl.h.

static const char *opencl_program[] =
                  {"#ifdef cl_clang_storage_class_specifiers\n#pragma OPENCL EXTENSION cl_clang_storage_class_specifiers : enable\n#endif\n#pragma OPENCL EXTENSION cl_khr_byte_addressable_store : enable\n__kernel void dummy_kernel(__global unsigned char *dummy, int n)\n{\n    const int thread_gid = get_global_id(0);\n    \n    if (thread_gid >= n)\n        return;\n}\ntypedef char int8_t;\ntypedef short int16_t;\ntypedef int int32_t;\ntypedef long int64_t;\ntypedef uchar uint8_t;\ntypedef ushort uint16_t;\ntypedef uint uint32_t;\ntypedef ulong uint64_t;\n#ifdef cl_nv_pragma_unroll\nstatic inline void mem_fence_global()\n{\n    asm(\"membar.gl;\");\n}\n#else\nstatic inline void mem_fence_global()\n{\n    mem_fence(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);\n}\n#endif\nstatic inline void mem_fence_local()\n{\n    mem_fence(CLK_LOCAL_MEM_FENCE);\n}\nstatic inline uint8_t add8(uint8_t x, uint8_t y)\n{\n    return x + y;\n}\nstatic inline uint16_t add16(uint16_t x, uint16_t y)\n{\n    return x + y;\n}\nstatic inline uint32_t add32(uint32_t x, uint32_t y)\n{\n    return x + y;\n}\nstatic inline uint64_t add64(uint64_t x, uint64_t y)\n{\n    return x + y;\n}\nstatic inline uint8_t sub8(uint8_t x, uint8_t y)\n{\n    return x - y;\n}\nstatic inline uint16_t sub16(uint16_t x, uint16_t y)\n{\n    return x - y;\n}\nstatic inline uint32_t sub32(uint32_t x, uint32_t y)\n{\n    return x - y;\n}\nstatic inline uint64_t sub64(uint64_t x, uint64_t y)\n{\n    return x - y;\n}\nstatic inline uint8_t mul8(uint8_t x, uint8_t y)\n{\n    return x * y;\n}\nstatic inline uint16_t mul16(uint16_t x, uint16_t y)\n{\n    return x * y;\n}\nstatic inline uint32_t mul32(uint32_t x, uint32_t y)\n{\n    return x * y;\n}\nstatic inline uint64_t mul64(uint64_t x, uint64_t y)\n{\n    return x * y;\n}\nstatic inline uint8_t udiv8(uint8_t x, uint8_t y)\n{\n    return x / y;\n}\nstatic inline uint16_t udiv16(uint16_t x, uint16_t y)\n{\n    return x / y;\n}\nstatic inline uint32_t udiv32(uint32_t x, uint32_t y)\n{\n    return x / y;\n}\nstatic inline uint64_t udiv64(uint64_t x, uint64_t y)\n{\n    return x / y;\n}\nstatic ",
                   "inline uint8_t umod8(uint8_t x, uint8_t y)\n{\n    return x % y;\n}\nstatic inline uint16_t umod16(uint16_t x, uint16_t y)\n{\n    return x % y;\n}\nstatic inline uint32_t umod32(uint32_t x, uint32_t y)\n{\n    return x % y;\n}\nstatic inline uint64_t umod64(uint64_t x, uint64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t sdiv8(int8_t x, int8_t y)\n{\n    int8_t q = x / y;\n    int8_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int16_t sdiv16(int16_t x, int16_t y)\n{\n    int16_t q = x / y;\n    int16_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int32_t sdiv32(int32_t x, int32_t y)\n{\n    int32_t q = x / y;\n    int32_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int64_t sdiv64(int64_t x, int64_t y)\n{\n    int64_t q = x / y;\n    int64_t r = x % y;\n    \n    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);\n}\nstatic inline int8_t smod8(int8_t x, int8_t y)\n{\n    int8_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int16_t smod16(int16_t x, int16_t y)\n{\n    int16_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int32_t smod32(int32_t x, int32_t y)\n{\n    int32_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int64_t smod64(int64_t x, int64_t y)\n{\n    int64_t r = x % y;\n    \n    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);\n}\nstatic inline int8_t squot8(int8_t x, int8_t y)\n{\n    return x / y;\n}\nstatic inline int16_t squot16(int16_t x, int16_t y)\n{\n    return x / y;\n}\nstatic inline int32_t squot32(int32_t x, int32_t y)\n{\n    return x / y;\n}\nstatic inline int64_t squot64(int64_t x, int64_t y)\n{\n    return x / y;\n}\nstatic inline int8_t srem8(int8_t x, int8_t y)\n{\n    return x % y;\n}\nstatic inline int16_t srem16(int16_t x, int16_t y)\n{\n    return x % y;\n}\nstatic inline int32_t sr",
                   "em32(int32_t x, int32_t y)\n{\n    return x % y;\n}\nstatic inline int64_t srem64(int64_t x, int64_t y)\n{\n    return x % y;\n}\nstatic inline int8_t smin8(int8_t x, int8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int16_t smin16(int16_t x, int16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int32_t smin32(int32_t x, int32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int64_t smin64(int64_t x, int64_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint8_t umin8(uint8_t x, uint8_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint16_t umin16(uint16_t x, uint16_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint32_t umin32(uint32_t x, uint32_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline uint64_t umin64(uint64_t x, uint64_t y)\n{\n    return x < y ? x : y;\n}\nstatic inline int8_t smax8(int8_t x, int8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int16_t smax16(int16_t x, int16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int32_t smax32(int32_t x, int32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline int64_t smax64(int64_t x, int64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t umax8(uint8_t x, uint8_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint16_t umax16(uint16_t x, uint16_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint32_t umax32(uint32_t x, uint32_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint64_t umax64(uint64_t x, uint64_t y)\n{\n    return x < y ? y : x;\n}\nstatic inline uint8_t shl8(uint8_t x, uint8_t y)\n{\n    return x << y;\n}\nstatic inline uint16_t shl16(uint16_t x, uint16_t y)\n{\n    return x << y;\n}\nstatic inline uint32_t shl32(uint32_t x, uint32_t y)\n{\n    return x << y;\n}\nstatic inline uint64_t shl64(uint64_t x, uint64_t y)\n{\n    return x << y;\n}\nstatic inline uint8_t lshr8(uint8_t x, uint8_t y)\n{\n    return x >> y;\n}\nstatic inline uint16_t lshr16(uint16_t x, uint16_t y)\n{\n    return x >> y;\n}\nstatic inline uint32_t lshr32(uint32_t x, uint32_t y)\n{\n    return x >> y;\n}\nstatic inline uint64_t lshr64(uint64_t x, uint6",
                   "4_t y)\n{\n    return x >> y;\n}\nstatic inline int8_t ashr8(int8_t x, int8_t y)\n{\n    return x >> y;\n}\nstatic inline int16_t ashr16(int16_t x, int16_t y)\n{\n    return x >> y;\n}\nstatic inline int32_t ashr32(int32_t x, int32_t y)\n{\n    return x >> y;\n}\nstatic inline int64_t ashr64(int64_t x, int64_t y)\n{\n    return x >> y;\n}\nstatic inline uint8_t and8(uint8_t x, uint8_t y)\n{\n    return x & y;\n}\nstatic inline uint16_t and16(uint16_t x, uint16_t y)\n{\n    return x & y;\n}\nstatic inline uint32_t and32(uint32_t x, uint32_t y)\n{\n    return x & y;\n}\nstatic inline uint64_t and64(uint64_t x, uint64_t y)\n{\n    return x & y;\n}\nstatic inline uint8_t or8(uint8_t x, uint8_t y)\n{\n    return x | y;\n}\nstatic inline uint16_t or16(uint16_t x, uint16_t y)\n{\n    return x | y;\n}\nstatic inline uint32_t or32(uint32_t x, uint32_t y)\n{\n    return x | y;\n}\nstatic inline uint64_t or64(uint64_t x, uint64_t y)\n{\n    return x | y;\n}\nstatic inline uint8_t xor8(uint8_t x, uint8_t y)\n{\n    return x ^ y;\n}\nstatic inline uint16_t xor16(uint16_t x, uint16_t y)\n{\n    return x ^ y;\n}\nstatic inline uint32_t xor32(uint32_t x, uint32_t y)\n{\n    return x ^ y;\n}\nstatic inline uint64_t xor64(uint64_t x, uint64_t y)\n{\n    return x ^ y;\n}\nstatic inline bool ult8(uint8_t x, uint8_t y)\n{\n    return x < y;\n}\nstatic inline bool ult16(uint16_t x, uint16_t y)\n{\n    return x < y;\n}\nstatic inline bool ult32(uint32_t x, uint32_t y)\n{\n    return x < y;\n}\nstatic inline bool ult64(uint64_t x, uint64_t y)\n{\n    return x < y;\n}\nstatic inline bool ule8(uint8_t x, uint8_t y)\n{\n    return x <= y;\n}\nstatic inline bool ule16(uint16_t x, uint16_t y)\n{\n    return x <= y;\n}\nstatic inline bool ule32(uint32_t x, uint32_t y)\n{\n    return x <= y;\n}\nstatic inline bool ule64(uint64_t x, uint64_t y)\n{\n    return x <= y;\n}\nstatic inline bool slt8(int8_t x, int8_t y)\n{\n    return x < y;\n}\nstatic inline bool slt16(int16_t x, int16_t y)\n{\n    return x < y;\n}\nstatic inline bool slt32(int32_t x, int32_t y)\n{\n    return x < y;\n}\nstatic inline bool slt64",
                   "(int64_t x, int64_t y)\n{\n    return x < y;\n}\nstatic inline bool sle8(int8_t x, int8_t y)\n{\n    return x <= y;\n}\nstatic inline bool sle16(int16_t x, int16_t y)\n{\n    return x <= y;\n}\nstatic inline bool sle32(int32_t x, int32_t y)\n{\n    return x <= y;\n}\nstatic inline bool sle64(int64_t x, int64_t y)\n{\n    return x <= y;\n}\nstatic inline int8_t pow8(int8_t x, int8_t y)\n{\n    int8_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int16_t pow16(int16_t x, int16_t y)\n{\n    int16_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int32_t pow32(int32_t x, int32_t y)\n{\n    int32_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline int64_t pow64(int64_t x, int64_t y)\n{\n    int64_t res = 1, rem = y;\n    \n    while (rem != 0) {\n        if (rem & 1)\n            res *= x;\n        rem >>= 1;\n        x *= x;\n    }\n    return res;\n}\nstatic inline bool itob_i8_bool(int8_t x)\n{\n    return x;\n}\nstatic inline bool itob_i16_bool(int16_t x)\n{\n    return x;\n}\nstatic inline bool itob_i32_bool(int32_t x)\n{\n    return x;\n}\nstatic inline bool itob_i64_bool(int64_t x)\n{\n    return x;\n}\nstatic inline int8_t btoi_bool_i8(bool x)\n{\n    return x;\n}\nstatic inline int16_t btoi_bool_i16(bool x)\n{\n    return x;\n}\nstatic inline int32_t btoi_bool_i32(bool x)\n{\n    return x;\n}\nstatic inline int64_t btoi_bool_i64(bool x)\n{\n    return x;\n}\n#define sext_i8_i8(x) ((int8_t) (int8_t) x)\n#define sext_i8_i16(x) ((int16_t) (int8_t) x)\n#define sext_i8_i32(x) ((int32_t) (int8_t) x)\n#define sext_i8_i64(x) ((int64_t) (int8_t) x)\n#define sext_i16_i8(x) ((int8_t) (int16_t) x)\n#define sext_i16_i16(x) ((int16_t) (int16_t) x)\n#define sext_i16_i32(x) ((int32_t) (int16_t) x)\n#define sext_i16_i6",
                   "4(x) ((int64_t) (int16_t) x)\n#define sext_i32_i8(x) ((int8_t) (int32_t) x)\n#define sext_i32_i16(x) ((int16_t) (int32_t) x)\n#define sext_i32_i32(x) ((int32_t) (int32_t) x)\n#define sext_i32_i64(x) ((int64_t) (int32_t) x)\n#define sext_i64_i8(x) ((int8_t) (int64_t) x)\n#define sext_i64_i16(x) ((int16_t) (int64_t) x)\n#define sext_i64_i32(x) ((int32_t) (int64_t) x)\n#define sext_i64_i64(x) ((int64_t) (int64_t) x)\n#define zext_i8_i8(x) ((uint8_t) (uint8_t) x)\n#define zext_i8_i16(x) ((uint16_t) (uint8_t) x)\n#define zext_i8_i32(x) ((uint32_t) (uint8_t) x)\n#define zext_i8_i64(x) ((uint64_t) (uint8_t) x)\n#define zext_i16_i8(x) ((uint8_t) (uint16_t) x)\n#define zext_i16_i16(x) ((uint16_t) (uint16_t) x)\n#define zext_i16_i32(x) ((uint32_t) (uint16_t) x)\n#define zext_i16_i64(x) ((uint64_t) (uint16_t) x)\n#define zext_i32_i8(x) ((uint8_t) (uint32_t) x)\n#define zext_i32_i16(x) ((uint16_t) (uint32_t) x)\n#define zext_i32_i32(x) ((uint32_t) (uint32_t) x)\n#define zext_i32_i64(x) ((uint64_t) (uint32_t) x)\n#define zext_i64_i8(x) ((uint8_t) (uint64_t) x)\n#define zext_i64_i16(x) ((uint16_t) (uint64_t) x)\n#define zext_i64_i32(x) ((uint32_t) (uint64_t) x)\n#define zext_i64_i64(x) ((uint64_t) (uint64_t) x)\n#if defined(__OPENCL_VERSION__)\nstatic int32_t futrts_popc8(int8_t x)\n{\n    return popcount(x);\n}\nstatic int32_t futrts_popc16(int16_t x)\n{\n    return popcount(x);\n}\nstatic int32_t futrts_popc32(int32_t x)\n{\n    return popcount(x);\n}\nstatic int32_t futrts_popc64(int64_t x)\n{\n    return popcount(x);\n}\n#elif defined(__CUDA_ARCH__)\nstatic int32_t futrts_popc8(int8_t x)\n{\n    return __popc(zext_i8_i32(x));\n}\nstatic int32_t futrts_popc16(int16_t x)\n{\n    return __popc(zext_i16_i32(x));\n}\nstatic int32_t futrts_popc32(int32_t x)\n{\n    return __popc(x);\n}\nstatic int32_t futrts_popc64(int64_t x)\n{\n    return __popcll(x);\n}\n#else\nstatic int32_t futrts_popc8(int8_t x)\n{\n    int c = 0;\n    \n    for (; x; ++c)\n        x &= x - 1;\n    return c;\n}\nstatic int32_t futrts_popc16(int16_t x)\n{\n    int c = 0;\n    \n  ",
                   "  for (; x; ++c)\n        x &= x - 1;\n    return c;\n}\nstatic int32_t futrts_popc32(int32_t x)\n{\n    int c = 0;\n    \n    for (; x; ++c)\n        x &= x - 1;\n    return c;\n}\nstatic int32_t futrts_popc64(int64_t x)\n{\n    int c = 0;\n    \n    for (; x; ++c)\n        x &= x - 1;\n    return c;\n}\n#endif\n#if defined(__OPENCL_VERSION__)\nstatic uint8_t futrts_mul_hi8(uint8_t a, uint8_t b)\n{\n    return mul_hi(a, b);\n}\nstatic uint16_t futrts_mul_hi16(uint16_t a, uint16_t b)\n{\n    return mul_hi(a, b);\n}\nstatic uint32_t futrts_mul_hi32(uint32_t a, uint32_t b)\n{\n    return mul_hi(a, b);\n}\nstatic uint64_t futrts_mul_hi64(uint64_t a, uint64_t b)\n{\n    return mul_hi(a, b);\n}\n#elif defined(__CUDA_ARCH__)\nstatic uint8_t futrts_mul_hi8(uint8_t a, uint8_t b)\n{\n    uint16_t aa = a;\n    uint16_t bb = b;\n    \n    return aa * bb >> 8;\n}\nstatic uint16_t futrts_mul_hi16(uint16_t a, uint16_t b)\n{\n    uint32_t aa = a;\n    uint32_t bb = b;\n    \n    return aa * bb >> 16;\n}\nstatic uint32_t futrts_mul_hi32(uint32_t a, uint32_t b)\n{\n    return mulhi(a, b);\n}\nstatic uint64_t futrts_mul_hi64(uint64_t a, uint64_t b)\n{\n    return mul64hi(a, b);\n}\n#else\nstatic uint8_t futrts_mul_hi8(uint8_t a, uint8_t b)\n{\n    uint16_t aa = a;\n    uint16_t bb = b;\n    \n    return aa * bb >> 8;\n}\nstatic uint16_t futrts_mul_hi16(uint16_t a, uint16_t b)\n{\n    uint32_t aa = a;\n    uint32_t bb = b;\n    \n    return aa * bb >> 16;\n}\nstatic uint32_t futrts_mul_hi32(uint32_t a, uint32_t b)\n{\n    uint64_t aa = a;\n    uint64_t bb = b;\n    \n    return aa * bb >> 32;\n}\nstatic uint64_t futrts_mul_hi64(uint64_t a, uint64_t b)\n{\n    __uint128_t aa = a;\n    __uint128_t bb = b;\n    \n    return aa * bb >> 64;\n}\n#endif\n#if defined(__OPENCL_VERSION__)\nstatic uint8_t futrts_mad_hi8(uint8_t a, uint8_t b, uint8_t c)\n{\n    return mad_hi(a, b, c);\n}\nstatic uint16_t futrts_mad_hi16(uint16_t a, uint16_t b, uint16_t c)\n{\n    return mad_hi(a, b, c);\n}\nstatic uint32_t futrts_mad_hi32(uint32_t a, uint32_t b, uint32_t c)\n{\n    return mad_hi(a, b, c);\n}\nstati",
                   "c uint64_t futrts_mad_hi64(uint64_t a, uint64_t b, uint64_t c)\n{\n    return mad_hi(a, b, c);\n}\n#else\nstatic uint8_t futrts_mad_hi8(uint8_t a, uint8_t b, uint8_t c)\n{\n    return futrts_mul_hi8(a, b) + c;\n}\nstatic uint16_t futrts_mad_hi16(uint16_t a, uint16_t b, uint16_t c)\n{\n    return futrts_mul_hi16(a, b) + c;\n}\nstatic uint32_t futrts_mad_hi32(uint32_t a, uint32_t b, uint32_t c)\n{\n    return futrts_mul_hi32(a, b) + c;\n}\nstatic uint64_t futrts_mad_hi64(uint64_t a, uint64_t b, uint64_t c)\n{\n    return futrts_mul_hi64(a, b) + c;\n}\n#endif\n#if defined(__OPENCL_VERSION__)\nstatic int32_t futrts_clzz8(int8_t x)\n{\n    return clz(x);\n}\nstatic int32_t futrts_clzz16(int16_t x)\n{\n    return clz(x);\n}\nstatic int32_t futrts_clzz32(int32_t x)\n{\n    return clz(x);\n}\nstatic int32_t futrts_clzz64(int64_t x)\n{\n    return clz(x);\n}\n#elif defined(__CUDA_ARCH__)\nstatic int32_t futrts_clzz8(int8_t x)\n{\n    return __clz(zext_i8_i32(x)) - 24;\n}\nstatic int32_t futrts_clzz16(int16_t x)\n{\n    return __clz(zext_i16_i32(x)) - 16;\n}\nstatic int32_t futrts_clzz32(int32_t x)\n{\n    return __clz(x);\n}\nstatic int32_t futrts_clzz64(int64_t x)\n{\n    return __clzll(x);\n}\n#else\nstatic int32_t futrts_clzz8(int8_t x)\n{\n    int n = 0;\n    int bits = sizeof(x) * 8;\n    \n    for (int i = 0; i < bits; i++) {\n        if (x < 0)\n            break;\n        n++;\n        x <<= 1;\n    }\n    return n;\n}\nstatic int32_t futrts_clzz16(int16_t x)\n{\n    int n = 0;\n    int bits = sizeof(x) * 8;\n    \n    for (int i = 0; i < bits; i++) {\n        if (x < 0)\n            break;\n        n++;\n        x <<= 1;\n    }\n    return n;\n}\nstatic int32_t futrts_clzz32(int32_t x)\n{\n    int n = 0;\n    int bits = sizeof(x) * 8;\n    \n    for (int i = 0; i < bits; i++) {\n        if (x < 0)\n            break;\n        n++;\n        x <<= 1;\n    }\n    return n;\n}\nstatic int32_t futrts_clzz64(int64_t x)\n{\n    int n = 0;\n    int bits = sizeof(x) * 8;\n    \n    for (int i = 0; i < bits; i++) {\n        if (x < 0)\n            break;\n        n++;\n        x",
                   " <<= 1;\n    }\n    return n;\n}\n#endif\nstatic inline float fdiv32(float x, float y)\n{\n    return x / y;\n}\nstatic inline float fadd32(float x, float y)\n{\n    return x + y;\n}\nstatic inline float fsub32(float x, float y)\n{\n    return x - y;\n}\nstatic inline float fmul32(float x, float y)\n{\n    return x * y;\n}\nstatic inline float fmin32(float x, float y)\n{\n    return fmin(x, y);\n}\nstatic inline float fmax32(float x, float y)\n{\n    return fmax(x, y);\n}\nstatic inline float fpow32(float x, float y)\n{\n    return pow(x, y);\n}\nstatic inline bool cmplt32(float x, float y)\n{\n    return x < y;\n}\nstatic inline bool cmple32(float x, float y)\n{\n    return x <= y;\n}\nstatic inline float sitofp_i8_f32(int8_t x)\n{\n    return (float) x;\n}\nstatic inline float sitofp_i16_f32(int16_t x)\n{\n    return (float) x;\n}\nstatic inline float sitofp_i32_f32(int32_t x)\n{\n    return (float) x;\n}\nstatic inline float sitofp_i64_f32(int64_t x)\n{\n    return (float) x;\n}\nstatic inline float uitofp_i8_f32(uint8_t x)\n{\n    return (float) x;\n}\nstatic inline float uitofp_i16_f32(uint16_t x)\n{\n    return (float) x;\n}\nstatic inline float uitofp_i32_f32(uint32_t x)\n{\n    return (float) x;\n}\nstatic inline float uitofp_i64_f32(uint64_t x)\n{\n    return (float) x;\n}\nstatic inline int8_t fptosi_f32_i8(float x)\n{\n    return (int8_t) x;\n}\nstatic inline int16_t fptosi_f32_i16(float x)\n{\n    return (int16_t) x;\n}\nstatic inline int32_t fptosi_f32_i32(float x)\n{\n    return (int32_t) x;\n}\nstatic inline int64_t fptosi_f32_i64(float x)\n{\n    return (int64_t) x;\n}\nstatic inline uint8_t fptoui_f32_i8(float x)\n{\n    return (uint8_t) x;\n}\nstatic inline uint16_t fptoui_f32_i16(float x)\n{\n    return (uint16_t) x;\n}\nstatic inline uint32_t fptoui_f32_i32(float x)\n{\n    return (uint32_t) x;\n}\nstatic inline uint64_t fptoui_f32_i64(float x)\n{\n    return (uint64_t) x;\n}\nstatic inline float futrts_log32(float x)\n{\n    return log(x);\n}\nstatic inline float futrts_log2_32(float x)\n{\n    return log2(x);\n}\nstatic inline float futrts_log10_32(float ",
                   "x)\n{\n    return log10(x);\n}\nstatic inline float futrts_sqrt32(float x)\n{\n    return sqrt(x);\n}\nstatic inline float futrts_exp32(float x)\n{\n    return exp(x);\n}\nstatic inline float futrts_cos32(float x)\n{\n    return cos(x);\n}\nstatic inline float futrts_sin32(float x)\n{\n    return sin(x);\n}\nstatic inline float futrts_tan32(float x)\n{\n    return tan(x);\n}\nstatic inline float futrts_acos32(float x)\n{\n    return acos(x);\n}\nstatic inline float futrts_asin32(float x)\n{\n    return asin(x);\n}\nstatic inline float futrts_atan32(float x)\n{\n    return atan(x);\n}\nstatic inline float futrts_cosh32(float x)\n{\n    return cosh(x);\n}\nstatic inline float futrts_sinh32(float x)\n{\n    return sinh(x);\n}\nstatic inline float futrts_tanh32(float x)\n{\n    return tanh(x);\n}\nstatic inline float futrts_acosh32(float x)\n{\n    return acosh(x);\n}\nstatic inline float futrts_asinh32(float x)\n{\n    return asinh(x);\n}\nstatic inline float futrts_atanh32(float x)\n{\n    return atanh(x);\n}\nstatic inline float futrts_atan2_32(float x, float y)\n{\n    return atan2(x, y);\n}\nstatic inline float futrts_gamma32(float x)\n{\n    return tgamma(x);\n}\nstatic inline float futrts_lgamma32(float x)\n{\n    return lgamma(x);\n}\nstatic inline bool futrts_isnan32(float x)\n{\n    return isnan(x);\n}\nstatic inline bool futrts_isinf32(float x)\n{\n    return isinf(x);\n}\nstatic inline int32_t futrts_to_bits32(float x)\n{\n    union {\n        float f;\n        int32_t t;\n    } p;\n    \n    p.f = x;\n    return p.t;\n}\nstatic inline float futrts_from_bits32(int32_t x)\n{\n    union {\n        int32_t f;\n        float t;\n    } p;\n    \n    p.f = x;\n    return p.t;\n}\n#ifdef __OPENCL_VERSION__\nstatic inline float fmod32(float x, float y)\n{\n    return fmod(x, y);\n}\nstatic inline float futrts_round32(float x)\n{\n    return rint(x);\n}\nstatic inline float futrts_floor32(float x)\n{\n    return floor(x);\n}\nstatic inline float futrts_ceil32(float x)\n{\n    return ceil(x);\n}\nstatic inline float futrts_lerp32(float v0, float v1, float t)\n{\n    return mix(v0, v1,",
                   " t);\n}\nstatic inline float futrts_mad32(float a, float b, float c)\n{\n    return mad(a, b, c);\n}\nstatic inline float futrts_fma32(float a, float b, float c)\n{\n    return fma(a, b, c);\n}\n#else\nstatic inline float fmod32(float x, float y)\n{\n    return fmodf(x, y);\n}\nstatic inline float futrts_round32(float x)\n{\n    return rintf(x);\n}\nstatic inline float futrts_floor32(float x)\n{\n    return floorf(x);\n}\nstatic inline float futrts_ceil32(float x)\n{\n    return ceilf(x);\n}\nstatic inline float futrts_lerp32(float v0, float v1, float t)\n{\n    return v0 + (v1 - v0) * t;\n}\nstatic inline float futrts_mad32(float a, float b, float c)\n{\n    return a * b + c;\n}\nstatic inline float futrts_fma32(float a, float b, float c)\n{\n    return fmaf(a, b, c);\n}\n#endif\n// Start of atomics.h\n\ninline int32_t atomic_add_i32_global(volatile __global int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicAdd((int32_t*)p, x);\n#else\n  return atomic_add(p, x);\n#endif\n}\n\ninline int32_t atomic_add_i32_local(volatile __local int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicAdd((int32_t*)p, x);\n#else\n  return atomic_add(p, x);\n#endif\n}\n\ninline float atomic_fadd_f32_global(volatile __global float *p, float x) {\n#ifdef FUTHARK_CUDA\n  return atomicAdd((float*)p, x);\n#else\n  union { int32_t i; float f; } old;\n  union { int32_t i; float f; } assumed;\n  old.f = *p;\n  do {\n    assumed.f = old.f;\n    old.f = old.f + x;\n    old.i = atomic_cmpxchg((volatile __global int32_t*)p, assumed.i, old.i);\n  } while (assumed.i != old.i);\n  return old.f;\n#endif\n}\n\ninline float atomic_fadd_f32_local(volatile __local float *p, float x) {\n#ifdef FUTHARK_CUDA\n  return atomicAdd((float*)p, x);\n#else\n  union { int32_t i; float f; } old;\n  union { int32_t i; float f; } assumed;\n  old.f = *p;\n  do {\n    assumed.f = old.f;\n    old.f = old.f + x;\n    old.i = atomic_cmpxchg((volatile __local int32_t*)p, assumed.i, old.i);\n  } while (assumed.i != old.i);\n  return old.f;\n#endif\n}\n\ninline int32_t atomic_smax_i32_global(vola",
                   "tile __global int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicMax((int32_t*)p, x);\n#else\n  return atomic_max(p, x);\n#endif\n}\n\ninline int32_t atomic_smax_i32_local(volatile __local int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicMax((int32_t*)p, x);\n#else\n  return atomic_max(p, x);\n#endif\n}\n\ninline int32_t atomic_smin_i32_global(volatile __global int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicMin((int32_t*)p, x);\n#else\n  return atomic_min(p, x);\n#endif\n}\n\ninline int32_t atomic_smin_i32_local(volatile __local int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicMin((int32_t*)p, x);\n#else\n  return atomic_min(p, x);\n#endif\n}\n\ninline uint32_t atomic_umax_i32_global(volatile __global uint32_t *p, uint32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicMax((uint32_t*)p, x);\n#else\n  return atomic_max(p, x);\n#endif\n}\n\ninline uint32_t atomic_umax_i32_local(volatile __local uint32_t *p, uint32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicMax((uint32_t*)p, x);\n#else\n  return atomic_max(p, x);\n#endif\n}\n\ninline uint32_t atomic_umin_i32_global(volatile __global uint32_t *p, uint32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicMin((uint32_t*)p, x);\n#else\n  return atomic_min(p, x);\n#endif\n}\n\ninline uint32_t atomic_umin_i32_local(volatile __local uint32_t *p, uint32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicMin((uint32_t*)p, x);\n#else\n  return atomic_min(p, x);\n#endif\n}\n\ninline int32_t atomic_and_i32_global(volatile __global int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicAnd((int32_t*)p, x);\n#else\n  return atomic_and(p, x);\n#endif\n}\n\ninline int32_t atomic_and_i32_local(volatile __local int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicAnd((int32_t*)p, x);\n#else\n  return atomic_and(p, x);\n#endif\n}\n\ninline int32_t atomic_or_i32_global(volatile __global int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicOr((int32_t*)p, x);\n#else\n  return atomic_or(p, x);\n#endif\n}\n\ninline int32_t atomic_or_i32_local(volatile __local int32_t *p",
                   ", int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicOr((int32_t*)p, x);\n#else\n  return atomic_or(p, x);\n#endif\n}\n\ninline int32_t atomic_xor_i32_global(volatile __global int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicXor((int32_t*)p, x);\n#else\n  return atomic_xor(p, x);\n#endif\n}\n\ninline int32_t atomic_xor_i32_local(volatile __local int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicXor((int32_t*)p, x);\n#else\n  return atomic_xor(p, x);\n#endif\n}\n\ninline int32_t atomic_xchg_i32_global(volatile __global int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicExch((int32_t*)p, x);\n#else\n  return atomic_xor(p, x);\n#endif\n}\n\ninline int32_t atomic_xchg_i32_local(volatile __local int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicExch((int32_t*)p, x);\n#else\n  return atomic_xor(p, x);\n#endif\n}\n\ninline int32_t atomic_cmpxchg_i32_global(volatile __global int32_t *p,\n                                         int32_t cmp, int32_t val) {\n#ifdef FUTHARK_CUDA\n  return atomicCAS((int32_t*)p, cmp, val);\n#else\n  return atomic_cmpxchg(p, cmp, val);\n#endif\n}\n\ninline int32_t atomic_cmpxchg_i32_local(volatile __local int32_t *p,\n                                         int32_t cmp, int32_t val) {\n#ifdef FUTHARK_CUDA\n  return atomicCAS((int32_t*)p, cmp, val);\n#else\n  return atomic_cmpxchg(p, cmp, val);\n#endif\n}\n\n// End of atomics.h\n\n__kernel void scan_stage1_5696(__global int *global_failure, __local volatile\n                               int64_t *scan_arr_mem_5792_backing_aligned_0,\n                               __local volatile\n                               int64_t *scan_arr_mem_5790_backing_aligned_1,\n                               int32_t n_5540, __global\n                               unsigned char *k_mem_5742, __global\n                               unsigned char *d_mem_5743, __global\n                               unsigned char *mem_5747, __global\n                               unsigned char *mem_5749,\n                               int32_t num_threa",
                   "ds_5775)\n{\n    #define segscan_group_sizze_5691 (mainzisegscan_group_sizze_5690)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict scan_arr_mem_5792_backing_1 =\n                          (__local volatile\n                           char *) scan_arr_mem_5792_backing_aligned_0;\n    __local volatile char *restrict scan_arr_mem_5790_backing_0 =\n                          (__local volatile\n                           char *) scan_arr_mem_5790_backing_aligned_1;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_5785;\n    int32_t local_tid_5786;\n    int32_t group_sizze_5789;\n    int32_t wave_sizze_5788;\n    int32_t group_tid_5787;\n    \n    global_tid_5785 = get_global_id(0);\n    local_tid_5786 = get_local_id(0);\n    group_sizze_5789 = get_local_size(0);\n    wave_sizze_5788 = LOCKSTEP_WIDTH;\n    group_tid_5787 = get_group_id(0);\n    \n    int32_t phys_tid_5696 = global_tid_5785;\n    __local char *scan_arr_mem_5790;\n    \n    scan_arr_mem_5790 = (__local char *) scan_arr_mem_5790_backing_0;\n    \n    __local char *scan_arr_mem_5792;\n    \n    scan_arr_mem_5792 = (__local char *) scan_arr_mem_5792_backing_1;\n    \n    int32_t x_5580;\n    bool x_5581;\n    int32_t x_5582;\n    bool x_5583;\n    \n    x_5580 = 0;\n    x_5581 = 0;\n    for (int32_t j_5794 = 0; j_5794 < squot32(sub32(add32(n_5540,\n                                                          num_threads_5775), 1),\n                                              num_threads_5775); j_5794++) {\n        int32_t chunk_offset_5795 = add32(mul32(segscan_group_sizze_5691,\n                                                j_5794), mul32(group_tid_5787,\n                                                               mul32(segscan_group_sizze_5691,\n                                                                     squot32(sub32(add32(n_5540,\n                                                                                         num_th",
                   "reads_5775),\n                                                                                   1),\n                                                                             num_threads_5775))));\n        int32_t flat_idx_5796 = add32(chunk_offset_5795, local_tid_5786);\n        int32_t gtid_5695 = flat_idx_5796;\n        \n        // threads in bounds read input; others get neutral element\n        {\n            if (slt32(gtid_5695, n_5540)) {\n                int32_t x_5589 = ((__global int32_t *) k_mem_5742)[gtid_5695];\n                bool x_5590 = ((__global bool *) d_mem_5743)[gtid_5695];\n                \n                // write to-scan values to parameters\n                {\n                    x_5582 = x_5589;\n                    x_5583 = x_5590;\n                }\n                // write mapped values results to global memory\n                { }\n            } else {\n                x_5582 = 0;\n                x_5583 = 0;\n            }\n        }\n        // combine with carry and write to local memory\n        {\n            int32_t res_5584;\n            bool res_5585;\n            \n            if (x_5583) {\n                bool res_5586 = x_5581 || x_5583;\n                \n                res_5584 = x_5582;\n                res_5585 = res_5586;\n            } else {\n                int32_t res_5587 = add32(x_5580, x_5582);\n                bool res_5588 = x_5581 || x_5583;\n                \n                res_5584 = res_5587;\n                res_5585 = res_5588;\n            }\n            ((__local int32_t *) scan_arr_mem_5790)[local_tid_5786] = res_5584;\n            ((__local bool *) scan_arr_mem_5792)[local_tid_5786] = res_5585;\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        \n        int32_t x_5776;\n        bool x_5777;\n        int32_t x_5778;\n        bool x_5779;\n        int32_t x_5797;\n        bool x_5798;\n        int32_t x_5799;\n        bool x_5800;\n        int32_t skip_threads_5806;\n        \n        if (slt32(local_tid_5786, segscan_group_sizze_5691)) {",
                   "\n            x_5778 = ((volatile __local\n                       int32_t *) scan_arr_mem_5790)[local_tid_5786];\n            x_5779 = ((volatile __local\n                       bool *) scan_arr_mem_5792)[local_tid_5786];\n            if (sub32(local_tid_5786, mul32(squot32(local_tid_5786, 32), 32)) ==\n                0) {\n                x_5776 = x_5778;\n                x_5777 = x_5779;\n            }\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            skip_threads_5806 = 1;\n            while (slt32(skip_threads_5806, 32)) {\n                if (sle32(skip_threads_5806, sub32(local_tid_5786,\n                                                   mul32(squot32(local_tid_5786,\n                                                                 32), 32))) &&\n                    slt32(local_tid_5786, segscan_group_sizze_5691)) {\n                    // read operands\n                    {\n                        x_5776 = ((volatile __local\n                                   int32_t *) scan_arr_mem_5790)[sub32(local_tid_5786,\n                                                                       skip_threads_5806)];\n                        x_5777 = ((volatile __local\n                                   bool *) scan_arr_mem_5792)[sub32(local_tid_5786,\n                                                                    skip_threads_5806)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_5780;\n                        bool res_5781;\n                        \n                        if (x_5779) {\n                            bool res_5782 = x_5777 || x_5779;\n                            \n                            res_5780 = x_5778;\n                            res_5781 = res_5782;\n                        } else {\n                            int32_t res_5783 = add32(x_5776, x_5778);\n                            bool res_5784 = x_5777 || x_5779;\n                            \n               ",
                   "             res_5780 = res_5783;\n                            res_5781 = res_5784;\n                        }\n                        x_5776 = res_5780;\n                        x_5777 = res_5781;\n                    }\n                }\n                if (sle32(wave_sizze_5788, skip_threads_5806)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if (sle32(skip_threads_5806, sub32(local_tid_5786,\n                                                   mul32(squot32(local_tid_5786,\n                                                                 32), 32))) &&\n                    slt32(local_tid_5786, segscan_group_sizze_5691)) {\n                    // write result\n                    {\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_5790)[local_tid_5786] =\n                            x_5776;\n                        x_5778 = x_5776;\n                        ((volatile __local\n                          bool *) scan_arr_mem_5792)[local_tid_5786] = x_5777;\n                        x_5779 = x_5777;\n                    }\n                }\n                if (sle32(wave_sizze_5788, skip_threads_5806)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_5806 *= 2;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // last thread of block 'i' writes its result to offset 'i'\n        {\n            if (sub32(local_tid_5786, mul32(squot32(local_tid_5786, 32), 32)) ==\n                31 && slt32(local_tid_5786, segscan_group_sizze_5691)) {\n                ((volatile __local\n                  int32_t *) scan_arr_mem_5790)[squot32(local_tid_5786, 32)] =\n                    x_5776;\n                ((volatile __local\n                  bool *) scan_arr_mem_5792)[squot32(local_tid_5786, 32)] =\n                    x_5777;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // scan the first block, after which offset 'i' contains carr",
                   "y-in for block 'i+1'\n        {\n            int32_t skip_threads_5807;\n            \n            if (squot32(local_tid_5786, 32) == 0 && slt32(local_tid_5786,\n                                                          segscan_group_sizze_5691)) {\n                x_5799 = ((volatile __local\n                           int32_t *) scan_arr_mem_5790)[local_tid_5786];\n                x_5800 = ((volatile __local\n                           bool *) scan_arr_mem_5792)[local_tid_5786];\n                if (sub32(local_tid_5786, mul32(squot32(local_tid_5786, 32),\n                                                32)) == 0) {\n                    x_5797 = x_5799;\n                    x_5798 = x_5800;\n                }\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                skip_threads_5807 = 1;\n                while (slt32(skip_threads_5807, 32)) {\n                    if (sle32(skip_threads_5807, sub32(local_tid_5786,\n                                                       mul32(squot32(local_tid_5786,\n                                                                     32),\n                                                             32))) &&\n                        (squot32(local_tid_5786, 32) == 0 &&\n                         slt32(local_tid_5786, segscan_group_sizze_5691))) {\n                        // read operands\n                        {\n                            x_5797 = ((volatile __local\n                                       int32_t *) scan_arr_mem_5790)[sub32(local_tid_5786,\n                                                                           skip_threads_5807)];\n                            x_5798 = ((volatile __local\n                                       bool *) scan_arr_mem_5792)[sub32(local_tid_5786,\n                                                                        skip_threads_5807)];\n                        }\n                        // perform operation\n                        {\n                            i",
                   "nt32_t res_5801;\n                            bool res_5802;\n                            \n                            if (x_5800) {\n                                bool res_5803 = x_5798 || x_5800;\n                                \n                                res_5801 = x_5799;\n                                res_5802 = res_5803;\n                            } else {\n                                int32_t res_5804 = add32(x_5797, x_5799);\n                                bool res_5805 = x_5798 || x_5800;\n                                \n                                res_5801 = res_5804;\n                                res_5802 = res_5805;\n                            }\n                            x_5797 = res_5801;\n                            x_5798 = res_5802;\n                        }\n                    }\n                    if (sle32(wave_sizze_5788, skip_threads_5807)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (sle32(skip_threads_5807, sub32(local_tid_5786,\n                                                       mul32(squot32(local_tid_5786,\n                                                                     32),\n                                                             32))) &&\n                        (squot32(local_tid_5786, 32) == 0 &&\n                         slt32(local_tid_5786, segscan_group_sizze_5691))) {\n                        // write result\n                        {\n                            ((volatile __local\n                              int32_t *) scan_arr_mem_5790)[local_tid_5786] =\n                                x_5797;\n                            x_5799 = x_5797;\n                            ((volatile __local\n                              bool *) scan_arr_mem_5792)[local_tid_5786] =\n                                x_5798;\n                            x_5800 = x_5798;\n                        }\n                    }\n                    if (sle32(wave_sizze_5788, skip_threads_5807",
                   ")) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_5807 *= 2;\n                }\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // carry-in for every block except the first\n        {\n            if (!(squot32(local_tid_5786, 32) == 0 || !slt32(local_tid_5786,\n                                                             segscan_group_sizze_5691))) {\n                // read operands\n                {\n                    x_5778 = x_5776;\n                    x_5779 = x_5777;\n                    x_5776 = ((__local\n                               int32_t *) scan_arr_mem_5790)[sub32(squot32(local_tid_5786,\n                                                                           32),\n                                                                   1)];\n                    x_5777 = ((__local\n                               bool *) scan_arr_mem_5792)[sub32(squot32(local_tid_5786,\n                                                                        32),\n                                                                1)];\n                }\n                // perform operation\n                {\n                    int32_t res_5780;\n                    bool res_5781;\n                    \n                    if (x_5779) {\n                        bool res_5782 = x_5777 || x_5779;\n                        \n                        res_5780 = x_5778;\n                        res_5781 = res_5782;\n                    } else {\n                        int32_t res_5783 = add32(x_5776, x_5778);\n                        bool res_5784 = x_5777 || x_5779;\n                        \n                        res_5780 = res_5783;\n                        res_5781 = res_5784;\n                    }\n                    x_5776 = res_5780;\n                    x_5777 = res_5781;\n                }\n                // write final result\n                {\n                    ((__local int32_t *) scan_arr_mem_5790)[local_tid_",
                   "5786] =\n                        x_5776;\n                    ((__local bool *) scan_arr_mem_5792)[local_tid_5786] =\n                        x_5777;\n                }\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // restore correct values for first block\n        {\n            if (squot32(local_tid_5786, 32) == 0) {\n                ((__local int32_t *) scan_arr_mem_5790)[local_tid_5786] =\n                    x_5778;\n                ((__local bool *) scan_arr_mem_5792)[local_tid_5786] = x_5779;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // threads in bounds write partial scan result\n        {\n            if (slt32(gtid_5695, n_5540)) {\n                ((__global int32_t *) mem_5747)[gtid_5695] = ((__local\n                                                               int32_t *) scan_arr_mem_5790)[local_tid_5786];\n                ((__global bool *) mem_5749)[gtid_5695] = ((__local\n                                                            bool *) scan_arr_mem_5792)[local_tid_5786];\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // first thread reads last element as carry-in for next iteration\n        {\n            bool crosses_segment_5808 = 0;\n            bool should_load_carry_5809 = local_tid_5786 == 0 &&\n                 !crosses_segment_5808;\n            \n            if (should_load_carry_5809) {\n                x_5580 = ((__local\n                           int32_t *) scan_arr_mem_5790)[sub32(segscan_group_sizze_5691,\n                                                               1)];\n                x_5581 = ((__local\n                           bool *) scan_arr_mem_5792)[sub32(segscan_group_sizze_5691,\n                                                            1)];\n            }\n            if (!should_load_carry_5809) {\n                x_5580 = 0;\n                x_5581 = 0;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n    }\n    \n  error_1:\n    return;\n    #undef segscan_g",
                   "roup_sizze_5691\n}\n__kernel void scan_stage1_5705(__global int *global_failure, __local volatile\n                               int64_t *scan_arr_mem_5884_backing_aligned_0,\n                               __local volatile\n                               int64_t *scan_arr_mem_5882_backing_aligned_1,\n                               __local volatile\n                               int64_t *scan_arr_mem_5880_backing_aligned_2,\n                               int32_t n_5540, __global\n                               unsigned char *k_mem_5742, __global\n                               unsigned char *d_mem_5743, __global\n                               unsigned char *mem_5747, __global\n                               unsigned char *mem_5753, __global\n                               unsigned char *mem_5756, __global\n                               unsigned char *mem_5758, __global\n                               unsigned char *mem_5761,\n                               int32_t num_threads_5862)\n{\n    #define segscan_group_sizze_5700 (mainzisegscan_group_sizze_5699)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict scan_arr_mem_5884_backing_2 =\n                          (__local volatile\n                           char *) scan_arr_mem_5884_backing_aligned_0;\n    __local volatile char *restrict scan_arr_mem_5882_backing_1 =\n                          (__local volatile\n                           char *) scan_arr_mem_5882_backing_aligned_1;\n    __local volatile char *restrict scan_arr_mem_5880_backing_0 =\n                          (__local volatile\n                           char *) scan_arr_mem_5880_backing_aligned_2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_5875;\n    int32_t local_tid_5876;\n    int32_t group_sizze_5879;\n    int32_t wave_sizze_5878;\n    int32_t group_tid_5877;\n    \n    global_tid_5875 = get_global_id(0);\n    local_tid_5876 = get_local_id(0);\n    group_sizze_58",
                   "79 = get_local_size(0);\n    wave_sizze_5878 = LOCKSTEP_WIDTH;\n    group_tid_5877 = get_group_id(0);\n    \n    int32_t phys_tid_5705 = global_tid_5875;\n    __local char *scan_arr_mem_5880;\n    \n    scan_arr_mem_5880 = (__local char *) scan_arr_mem_5880_backing_0;\n    \n    __local char *scan_arr_mem_5882;\n    \n    scan_arr_mem_5882 = (__local char *) scan_arr_mem_5882_backing_1;\n    \n    __local char *scan_arr_mem_5884;\n    \n    scan_arr_mem_5884 = (__local char *) scan_arr_mem_5884_backing_2;\n    \n    int32_t x_5611;\n    int32_t x_5612;\n    bool x_5613;\n    int32_t x_5614;\n    int32_t x_5615;\n    bool x_5616;\n    \n    x_5611 = 0;\n    x_5612 = 0;\n    x_5613 = 0;\n    for (int32_t j_5886 = 0; j_5886 < squot32(sub32(add32(n_5540,\n                                                          num_threads_5862), 1),\n                                              num_threads_5862); j_5886++) {\n        int32_t chunk_offset_5887 = add32(mul32(segscan_group_sizze_5700,\n                                                j_5886), mul32(group_tid_5877,\n                                                               mul32(segscan_group_sizze_5700,\n                                                                     squot32(sub32(add32(n_5540,\n                                                                                         num_threads_5862),\n                                                                                   1),\n                                                                             num_threads_5862))));\n        int32_t flat_idx_5888 = add32(chunk_offset_5887, local_tid_5876);\n        int32_t gtid_5704 = flat_idx_5888;\n        \n        // threads in bounds read input; others get neutral element\n        {\n            if (slt32(gtid_5704, n_5540)) {\n                int32_t x_5623 = ((__global int32_t *) mem_5747)[gtid_5704];\n                bool x_5625 = ((__global bool *) d_mem_5743)[gtid_5704];\n                int32_t x_5626 = ((__global int32_t *) k_mem_5742)[gtid",
                   "_5704];\n                int32_t res_5629 = btoi_bool_i32(x_5625);\n                \n                // write to-scan values to parameters\n                {\n                    x_5614 = res_5629;\n                    x_5615 = x_5626;\n                    x_5616 = x_5625;\n                }\n                // write mapped values results to global memory\n                {\n                    ((__global int32_t *) mem_5761)[gtid_5704] = x_5623;\n                }\n            } else {\n                x_5614 = 0;\n                x_5615 = 0;\n                x_5616 = 0;\n            }\n        }\n        // combine with carry and write to local memory\n        {\n            int32_t res_5617 = add32(x_5611, x_5614);\n            int32_t res_5618;\n            bool res_5619;\n            \n            if (x_5616) {\n                bool res_5620 = x_5613 || x_5616;\n                \n                res_5618 = x_5615;\n                res_5619 = res_5620;\n            } else {\n                int32_t res_5621 = add32(x_5612, x_5615);\n                bool res_5622 = x_5613 || x_5616;\n                \n                res_5618 = res_5621;\n                res_5619 = res_5622;\n            }\n            ((__local int32_t *) scan_arr_mem_5880)[local_tid_5876] = res_5617;\n            ((__local int32_t *) scan_arr_mem_5882)[local_tid_5876] = res_5618;\n            ((__local bool *) scan_arr_mem_5884)[local_tid_5876] = res_5619;\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        \n        int32_t x_5863;\n        int32_t x_5864;\n        bool x_5865;\n        int32_t x_5866;\n        int32_t x_5867;\n        bool x_5868;\n        int32_t x_5889;\n        int32_t x_5890;\n        bool x_5891;\n        int32_t x_5892;\n        int32_t x_5893;\n        bool x_5894;\n        int32_t skip_threads_5901;\n        \n        if (slt32(local_tid_5876, segscan_group_sizze_5700)) {\n            x_5866 = ((volatile __local\n                       int32_t *) scan_arr_mem_5880)[local_tid_5876];\n            x_5867 = ((volatile __lo",
                   "cal\n                       int32_t *) scan_arr_mem_5882)[local_tid_5876];\n            x_5868 = ((volatile __local\n                       bool *) scan_arr_mem_5884)[local_tid_5876];\n            if (sub32(local_tid_5876, mul32(squot32(local_tid_5876, 32), 32)) ==\n                0) {\n                x_5863 = x_5866;\n                x_5864 = x_5867;\n                x_5865 = x_5868;\n            }\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            skip_threads_5901 = 1;\n            while (slt32(skip_threads_5901, 32)) {\n                if (sle32(skip_threads_5901, sub32(local_tid_5876,\n                                                   mul32(squot32(local_tid_5876,\n                                                                 32), 32))) &&\n                    slt32(local_tid_5876, segscan_group_sizze_5700)) {\n                    // read operands\n                    {\n                        x_5863 = ((volatile __local\n                                   int32_t *) scan_arr_mem_5880)[sub32(local_tid_5876,\n                                                                       skip_threads_5901)];\n                        x_5864 = ((volatile __local\n                                   int32_t *) scan_arr_mem_5882)[sub32(local_tid_5876,\n                                                                       skip_threads_5901)];\n                        x_5865 = ((volatile __local\n                                   bool *) scan_arr_mem_5884)[sub32(local_tid_5876,\n                                                                    skip_threads_5901)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_5869 = add32(x_5863, x_5866);\n                        int32_t res_5870;\n                        bool res_5871;\n                        \n                        if (x_5868) {\n                            bool res_5872 = x_5865 || x_5868;\n                            \n            ",
                   "                res_5870 = x_5867;\n                            res_5871 = res_5872;\n                        } else {\n                            int32_t res_5873 = add32(x_5864, x_5867);\n                            bool res_5874 = x_5865 || x_5868;\n                            \n                            res_5870 = res_5873;\n                            res_5871 = res_5874;\n                        }\n                        x_5863 = res_5869;\n                        x_5864 = res_5870;\n                        x_5865 = res_5871;\n                    }\n                }\n                if (sle32(wave_sizze_5878, skip_threads_5901)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if (sle32(skip_threads_5901, sub32(local_tid_5876,\n                                                   mul32(squot32(local_tid_5876,\n                                                                 32), 32))) &&\n                    slt32(local_tid_5876, segscan_group_sizze_5700)) {\n                    // write result\n                    {\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_5880)[local_tid_5876] =\n                            x_5863;\n                        x_5866 = x_5863;\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_5882)[local_tid_5876] =\n                            x_5864;\n                        x_5867 = x_5864;\n                        ((volatile __local\n                          bool *) scan_arr_mem_5884)[local_tid_5876] = x_5865;\n                        x_5868 = x_5865;\n                    }\n                }\n                if (sle32(wave_sizze_5878, skip_threads_5901)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_5901 *= 2;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // last thread of block 'i' writes its result to offset 'i'\n        {\n            if (sub32(local_tid_5876",
                   ", mul32(squot32(local_tid_5876, 32), 32)) ==\n                31 && slt32(local_tid_5876, segscan_group_sizze_5700)) {\n                ((volatile __local\n                  int32_t *) scan_arr_mem_5880)[squot32(local_tid_5876, 32)] =\n                    x_5863;\n                ((volatile __local\n                  int32_t *) scan_arr_mem_5882)[squot32(local_tid_5876, 32)] =\n                    x_5864;\n                ((volatile __local\n                  bool *) scan_arr_mem_5884)[squot32(local_tid_5876, 32)] =\n                    x_5865;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // scan the first block, after which offset 'i' contains carry-in for block 'i+1'\n        {\n            int32_t skip_threads_5902;\n            \n            if (squot32(local_tid_5876, 32) == 0 && slt32(local_tid_5876,\n                                                          segscan_group_sizze_5700)) {\n                x_5892 = ((volatile __local\n                           int32_t *) scan_arr_mem_5880)[local_tid_5876];\n                x_5893 = ((volatile __local\n                           int32_t *) scan_arr_mem_5882)[local_tid_5876];\n                x_5894 = ((volatile __local\n                           bool *) scan_arr_mem_5884)[local_tid_5876];\n                if (sub32(local_tid_5876, mul32(squot32(local_tid_5876, 32),\n                                                32)) == 0) {\n                    x_5889 = x_5892;\n                    x_5890 = x_5893;\n                    x_5891 = x_5894;\n                }\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                skip_threads_5902 = 1;\n                while (slt32(skip_threads_5902, 32)) {\n                    if (sle32(skip_threads_5902, sub32(local_tid_5876,\n                                                       mul32(squot32(local_tid_5876,\n                                                                     32),\n                                                      ",
                   "       32))) &&\n                        (squot32(local_tid_5876, 32) == 0 &&\n                         slt32(local_tid_5876, segscan_group_sizze_5700))) {\n                        // read operands\n                        {\n                            x_5889 = ((volatile __local\n                                       int32_t *) scan_arr_mem_5880)[sub32(local_tid_5876,\n                                                                           skip_threads_5902)];\n                            x_5890 = ((volatile __local\n                                       int32_t *) scan_arr_mem_5882)[sub32(local_tid_5876,\n                                                                           skip_threads_5902)];\n                            x_5891 = ((volatile __local\n                                       bool *) scan_arr_mem_5884)[sub32(local_tid_5876,\n                                                                        skip_threads_5902)];\n                        }\n                        // perform operation\n                        {\n                            int32_t res_5895 = add32(x_5889, x_5892);\n                            int32_t res_5896;\n                            bool res_5897;\n                            \n                            if (x_5894) {\n                                bool res_5898 = x_5891 || x_5894;\n                                \n                                res_5896 = x_5893;\n                                res_5897 = res_5898;\n                            } else {\n                                int32_t res_5899 = add32(x_5890, x_5893);\n                                bool res_5900 = x_5891 || x_5894;\n                                \n                                res_5896 = res_5899;\n                                res_5897 = res_5900;\n                            }\n                            x_5889 = res_5895;\n                            x_5890 = res_5896;\n                            x_5891 = res_5897;\n                        }\n             ",
                   "       }\n                    if (sle32(wave_sizze_5878, skip_threads_5902)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (sle32(skip_threads_5902, sub32(local_tid_5876,\n                                                       mul32(squot32(local_tid_5876,\n                                                                     32),\n                                                             32))) &&\n                        (squot32(local_tid_5876, 32) == 0 &&\n                         slt32(local_tid_5876, segscan_group_sizze_5700))) {\n                        // write result\n                        {\n                            ((volatile __local\n                              int32_t *) scan_arr_mem_5880)[local_tid_5876] =\n                                x_5889;\n                            x_5892 = x_5889;\n                            ((volatile __local\n                              int32_t *) scan_arr_mem_5882)[local_tid_5876] =\n                                x_5890;\n                            x_5893 = x_5890;\n                            ((volatile __local\n                              bool *) scan_arr_mem_5884)[local_tid_5876] =\n                                x_5891;\n                            x_5894 = x_5891;\n                        }\n                    }\n                    if (sle32(wave_sizze_5878, skip_threads_5902)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_5902 *= 2;\n                }\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // carry-in for every block except the first\n        {\n            if (!(squot32(local_tid_5876, 32) == 0 || !slt32(local_tid_5876,\n                                                             segscan_group_sizze_5700))) {\n                // read operands\n                {\n                    x_5866 = x_5863;\n                    x_5867 = x_5864;\n                    x_5868 = x_5865;\n     ",
                   "               x_5863 = ((__local\n                               int32_t *) scan_arr_mem_5880)[sub32(squot32(local_tid_5876,\n                                                                           32),\n                                                                   1)];\n                    x_5864 = ((__local\n                               int32_t *) scan_arr_mem_5882)[sub32(squot32(local_tid_5876,\n                                                                           32),\n                                                                   1)];\n                    x_5865 = ((__local\n                               bool *) scan_arr_mem_5884)[sub32(squot32(local_tid_5876,\n                                                                        32),\n                                                                1)];\n                }\n                // perform operation\n                {\n                    int32_t res_5869 = add32(x_5863, x_5866);\n                    int32_t res_5870;\n                    bool res_5871;\n                    \n                    if (x_5868) {\n                        bool res_5872 = x_5865 || x_5868;\n                        \n                        res_5870 = x_5867;\n                        res_5871 = res_5872;\n                    } else {\n                        int32_t res_5873 = add32(x_5864, x_5867);\n                        bool res_5874 = x_5865 || x_5868;\n                        \n                        res_5870 = res_5873;\n                        res_5871 = res_5874;\n                    }\n                    x_5863 = res_5869;\n                    x_5864 = res_5870;\n                    x_5865 = res_5871;\n                }\n                // write final result\n                {\n                    ((__local int32_t *) scan_arr_mem_5880)[local_tid_5876] =\n                        x_5863;\n                    ((__local int32_t *) scan_arr_mem_5882)[local_tid_5876] =\n                        x_5864;\n                    ((__local boo",
                   "l *) scan_arr_mem_5884)[local_tid_5876] =\n                        x_5865;\n                }\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // restore correct values for first block\n        {\n            if (squot32(local_tid_5876, 32) == 0) {\n                ((__local int32_t *) scan_arr_mem_5880)[local_tid_5876] =\n                    x_5866;\n                ((__local int32_t *) scan_arr_mem_5882)[local_tid_5876] =\n                    x_5867;\n                ((__local bool *) scan_arr_mem_5884)[local_tid_5876] = x_5868;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // threads in bounds write partial scan result\n        {\n            if (slt32(gtid_5704, n_5540)) {\n                ((__global int32_t *) mem_5753)[gtid_5704] = ((__local\n                                                               int32_t *) scan_arr_mem_5880)[local_tid_5876];\n                ((__global int32_t *) mem_5756)[gtid_5704] = ((__local\n                                                               int32_t *) scan_arr_mem_5882)[local_tid_5876];\n                ((__global bool *) mem_5758)[gtid_5704] = ((__local\n                                                            bool *) scan_arr_mem_5884)[local_tid_5876];\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // first thread reads last element as carry-in for next iteration\n        {\n            bool crosses_segment_5903 = 0;\n            bool should_load_carry_5904 = local_tid_5876 == 0 &&\n                 !crosses_segment_5903;\n            \n            if (should_load_carry_5904) {\n                x_5611 = ((__local\n                           int32_t *) scan_arr_mem_5880)[sub32(segscan_group_sizze_5700,\n                                                               1)];\n                x_5612 = ((__local\n                           int32_t *) scan_arr_mem_5882)[sub32(segscan_group_sizze_5700,\n                                                               1)];\n             ",
                   "   x_5613 = ((__local\n                           bool *) scan_arr_mem_5884)[sub32(segscan_group_sizze_5700,\n                                                            1)];\n            }\n            if (!should_load_carry_5904) {\n                x_5611 = 0;\n                x_5612 = 0;\n                x_5613 = 0;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n    }\n    \n  error_1:\n    return;\n    #undef segscan_group_sizze_5700\n}\n__kernel void scan_stage2_5696(__global int *global_failure, __local volatile\n                               int64_t *scan_arr_mem_5835_backing_aligned_0,\n                               __local volatile\n                               int64_t *scan_arr_mem_5833_backing_aligned_1,\n                               int32_t n_5540, int32_t num_groups_5693, __global\n                               unsigned char *mem_5747, __global\n                               unsigned char *mem_5749,\n                               int32_t num_threads_5775)\n{\n    #define segscan_group_sizze_5691 (mainzisegscan_group_sizze_5690)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict scan_arr_mem_5835_backing_1 =\n                          (__local volatile\n                           char *) scan_arr_mem_5835_backing_aligned_0;\n    __local volatile char *restrict scan_arr_mem_5833_backing_0 =\n                          (__local volatile\n                           char *) scan_arr_mem_5833_backing_aligned_1;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_5828;\n    int32_t local_tid_5829;\n    int32_t group_sizze_5832;\n    int32_t wave_sizze_5831;\n    int32_t group_tid_5830;\n    \n    global_tid_5828 = get_global_id(0);\n    local_tid_5829 = get_local_id(0);\n    group_sizze_5832 = get_local_size(0);\n    wave_sizze_5831 = LOCKSTEP_WIDTH;\n    group_tid_5830 = get_group_id(0);\n    \n    int32_t phys_tid_5696 = global_tid_5828;\n    __local char *scan_arr_mem_",
                   "5833;\n    \n    scan_arr_mem_5833 = (__local char *) scan_arr_mem_5833_backing_0;\n    \n    __local char *scan_arr_mem_5835;\n    \n    scan_arr_mem_5835 = (__local char *) scan_arr_mem_5835_backing_1;\n    \n    int32_t flat_idx_5837 = sub32(mul32(add32(local_tid_5829, 1),\n                                        mul32(segscan_group_sizze_5691,\n                                              squot32(sub32(add32(n_5540,\n                                                                  num_threads_5775),\n                                                            1),\n                                                      num_threads_5775))), 1);\n    int32_t gtid_5695 = flat_idx_5837;\n    \n    // threads in bound read carries; others get neutral element\n    {\n        if (slt32(gtid_5695, n_5540)) {\n            ((__local int32_t *) scan_arr_mem_5833)[local_tid_5829] = ((__global\n                                                                        int32_t *) mem_5747)[gtid_5695];\n            ((__local bool *) scan_arr_mem_5835)[local_tid_5829] = ((__global\n                                                                     bool *) mem_5749)[gtid_5695];\n        } else {\n            ((__local int32_t *) scan_arr_mem_5833)[local_tid_5829] = 0;\n            ((__local bool *) scan_arr_mem_5835)[local_tid_5829] = 0;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t x_5810;\n    bool x_5811;\n    int32_t x_5812;\n    bool x_5813;\n    int32_t x_5838;\n    bool x_5839;\n    int32_t x_5840;\n    bool x_5841;\n    int32_t skip_threads_5847;\n    \n    if (slt32(local_tid_5829, num_groups_5693)) {\n        x_5812 = ((volatile __local\n                   int32_t *) scan_arr_mem_5833)[local_tid_5829];\n        x_5813 = ((volatile __local bool *) scan_arr_mem_5835)[local_tid_5829];\n        if (sub32(local_tid_5829, mul32(squot32(local_tid_5829, 32), 32)) ==\n            0) {\n            x_5810 = x_5812;\n            x_5811 = x_5813;\n        }\n    }\n    // in-block scan (hopefully no barri",
                   "ers needed)\n    {\n        skip_threads_5847 = 1;\n        while (slt32(skip_threads_5847, 32)) {\n            if (sle32(skip_threads_5847, sub32(local_tid_5829,\n                                               mul32(squot32(local_tid_5829,\n                                                             32), 32))) &&\n                slt32(local_tid_5829, num_groups_5693)) {\n                // read operands\n                {\n                    x_5810 = ((volatile __local\n                               int32_t *) scan_arr_mem_5833)[sub32(local_tid_5829,\n                                                                   skip_threads_5847)];\n                    x_5811 = ((volatile __local\n                               bool *) scan_arr_mem_5835)[sub32(local_tid_5829,\n                                                                skip_threads_5847)];\n                }\n                // perform operation\n                {\n                    int32_t res_5814;\n                    bool res_5815;\n                    \n                    if (x_5813) {\n                        bool res_5816 = x_5811 || x_5813;\n                        \n                        res_5814 = x_5812;\n                        res_5815 = res_5816;\n                    } else {\n                        int32_t res_5817 = add32(x_5810, x_5812);\n                        bool res_5818 = x_5811 || x_5813;\n                        \n                        res_5814 = res_5817;\n                        res_5815 = res_5818;\n                    }\n                    x_5810 = res_5814;\n                    x_5811 = res_5815;\n                }\n            }\n            if (sle32(wave_sizze_5831, skip_threads_5847)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            if (sle32(skip_threads_5847, sub32(local_tid_5829,\n                                               mul32(squot32(local_tid_5829,\n                                                             32), 32))) &&\n                slt32(local_tid_5829, nu",
                   "m_groups_5693)) {\n                // write result\n                {\n                    ((volatile __local\n                      int32_t *) scan_arr_mem_5833)[local_tid_5829] = x_5810;\n                    x_5812 = x_5810;\n                    ((volatile __local\n                      bool *) scan_arr_mem_5835)[local_tid_5829] = x_5811;\n                    x_5813 = x_5811;\n                }\n            }\n            if (sle32(wave_sizze_5831, skip_threads_5847)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_5847 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if (sub32(local_tid_5829, mul32(squot32(local_tid_5829, 32), 32)) ==\n            31 && slt32(local_tid_5829, num_groups_5693)) {\n            ((volatile __local\n              int32_t *) scan_arr_mem_5833)[squot32(local_tid_5829, 32)] =\n                x_5810;\n            ((volatile __local\n              bool *) scan_arr_mem_5835)[squot32(local_tid_5829, 32)] = x_5811;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for block 'i+1'\n    {\n        int32_t skip_threads_5848;\n        \n        if (squot32(local_tid_5829, 32) == 0 && slt32(local_tid_5829,\n                                                      num_groups_5693)) {\n            x_5840 = ((volatile __local\n                       int32_t *) scan_arr_mem_5833)[local_tid_5829];\n            x_5841 = ((volatile __local\n                       bool *) scan_arr_mem_5835)[local_tid_5829];\n            if (sub32(local_tid_5829, mul32(squot32(local_tid_5829, 32), 32)) ==\n                0) {\n                x_5838 = x_5840;\n                x_5839 = x_5841;\n            }\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            skip_threads_5848 = 1;\n            while (slt32(skip_threads_5848, 32)) {\n                if (sle32(skip_threads_5848, sub3",
                   "2(local_tid_5829,\n                                                   mul32(squot32(local_tid_5829,\n                                                                 32), 32))) &&\n                    (squot32(local_tid_5829, 32) == 0 && slt32(local_tid_5829,\n                                                               num_groups_5693))) {\n                    // read operands\n                    {\n                        x_5838 = ((volatile __local\n                                   int32_t *) scan_arr_mem_5833)[sub32(local_tid_5829,\n                                                                       skip_threads_5848)];\n                        x_5839 = ((volatile __local\n                                   bool *) scan_arr_mem_5835)[sub32(local_tid_5829,\n                                                                    skip_threads_5848)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_5842;\n                        bool res_5843;\n                        \n                        if (x_5841) {\n                            bool res_5844 = x_5839 || x_5841;\n                            \n                            res_5842 = x_5840;\n                            res_5843 = res_5844;\n                        } else {\n                            int32_t res_5845 = add32(x_5838, x_5840);\n                            bool res_5846 = x_5839 || x_5841;\n                            \n                            res_5842 = res_5845;\n                            res_5843 = res_5846;\n                        }\n                        x_5838 = res_5842;\n                        x_5839 = res_5843;\n                    }\n                }\n                if (sle32(wave_sizze_5831, skip_threads_5848)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if (sle32(skip_threads_5848, sub32(local_tid_5829,\n                                                   mul32(squot32(local_tid_5829,\n   ",
                   "                                                              32), 32))) &&\n                    (squot32(local_tid_5829, 32) == 0 && slt32(local_tid_5829,\n                                                               num_groups_5693))) {\n                    // write result\n                    {\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_5833)[local_tid_5829] =\n                            x_5838;\n                        x_5840 = x_5838;\n                        ((volatile __local\n                          bool *) scan_arr_mem_5835)[local_tid_5829] = x_5839;\n                        x_5841 = x_5839;\n                    }\n                }\n                if (sle32(wave_sizze_5831, skip_threads_5848)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_5848 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_5829, 32) == 0 || !slt32(local_tid_5829,\n                                                         num_groups_5693))) {\n            // read operands\n            {\n                x_5812 = x_5810;\n                x_5813 = x_5811;\n                x_5810 = ((__local\n                           int32_t *) scan_arr_mem_5833)[sub32(squot32(local_tid_5829,\n                                                                       32), 1)];\n                x_5811 = ((__local\n                           bool *) scan_arr_mem_5835)[sub32(squot32(local_tid_5829,\n                                                                    32), 1)];\n            }\n            // perform operation\n            {\n                int32_t res_5814;\n                bool res_5815;\n                \n                if (x_5813) {\n                    bool res_5816 = x_5811 || x_5813;\n                    \n                    res_5814 = x_5812;\n                    res_5815 = res_5816;\n                } else {\n ",
                   "                   int32_t res_5817 = add32(x_5810, x_5812);\n                    bool res_5818 = x_5811 || x_5813;\n                    \n                    res_5814 = res_5817;\n                    res_5815 = res_5818;\n                }\n                x_5810 = res_5814;\n                x_5811 = res_5815;\n            }\n            // write final result\n            {\n                ((__local int32_t *) scan_arr_mem_5833)[local_tid_5829] =\n                    x_5810;\n                ((__local bool *) scan_arr_mem_5835)[local_tid_5829] = x_5811;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(local_tid_5829, 32) == 0) {\n            ((__local int32_t *) scan_arr_mem_5833)[local_tid_5829] = x_5812;\n            ((__local bool *) scan_arr_mem_5835)[local_tid_5829] = x_5813;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // threads in bounds write scanned carries\n    {\n        if (slt32(gtid_5695, n_5540)) {\n            ((__global int32_t *) mem_5747)[gtid_5695] = ((__local\n                                                           int32_t *) scan_arr_mem_5833)[local_tid_5829];\n            ((__global bool *) mem_5749)[gtid_5695] = ((__local\n                                                        bool *) scan_arr_mem_5835)[local_tid_5829];\n        }\n    }\n    \n  error_0:\n    return;\n    #undef segscan_group_sizze_5691\n}\n__kernel void scan_stage2_5705(__global int *global_failure, __local volatile\n                               int64_t *scan_arr_mem_5938_backing_aligned_0,\n                               __local volatile\n                               int64_t *scan_arr_mem_5936_backing_aligned_1,\n                               __local volatile\n                               int64_t *scan_arr_mem_5934_backing_aligned_2,\n                               int32_t n_5540, int32_t num_groups_5702, __global\n                               unsigned char *mem_5753, __global\n                     ",
                   "          unsigned char *mem_5756, __global\n                               unsigned char *mem_5758,\n                               int32_t num_threads_5862)\n{\n    #define segscan_group_sizze_5700 (mainzisegscan_group_sizze_5699)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict scan_arr_mem_5938_backing_2 =\n                          (__local volatile\n                           char *) scan_arr_mem_5938_backing_aligned_0;\n    __local volatile char *restrict scan_arr_mem_5936_backing_1 =\n                          (__local volatile\n                           char *) scan_arr_mem_5936_backing_aligned_1;\n    __local volatile char *restrict scan_arr_mem_5934_backing_0 =\n                          (__local volatile\n                           char *) scan_arr_mem_5934_backing_aligned_2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_5929;\n    int32_t local_tid_5930;\n    int32_t group_sizze_5933;\n    int32_t wave_sizze_5932;\n    int32_t group_tid_5931;\n    \n    global_tid_5929 = get_global_id(0);\n    local_tid_5930 = get_local_id(0);\n    group_sizze_5933 = get_local_size(0);\n    wave_sizze_5932 = LOCKSTEP_WIDTH;\n    group_tid_5931 = get_group_id(0);\n    \n    int32_t phys_tid_5705 = global_tid_5929;\n    __local char *scan_arr_mem_5934;\n    \n    scan_arr_mem_5934 = (__local char *) scan_arr_mem_5934_backing_0;\n    \n    __local char *scan_arr_mem_5936;\n    \n    scan_arr_mem_5936 = (__local char *) scan_arr_mem_5936_backing_1;\n    \n    __local char *scan_arr_mem_5938;\n    \n    scan_arr_mem_5938 = (__local char *) scan_arr_mem_5938_backing_2;\n    \n    int32_t flat_idx_5940 = sub32(mul32(add32(local_tid_5930, 1),\n                                        mul32(segscan_group_sizze_5700,\n                                              squot32(sub32(add32(n_5540,\n                                                                  num_threads_5862),\n                               ",
                   "                             1),\n                                                      num_threads_5862))), 1);\n    int32_t gtid_5704 = flat_idx_5940;\n    \n    // threads in bound read carries; others get neutral element\n    {\n        if (slt32(gtid_5704, n_5540)) {\n            ((__local int32_t *) scan_arr_mem_5934)[local_tid_5930] = ((__global\n                                                                        int32_t *) mem_5753)[gtid_5704];\n            ((__local int32_t *) scan_arr_mem_5936)[local_tid_5930] = ((__global\n                                                                        int32_t *) mem_5756)[gtid_5704];\n            ((__local bool *) scan_arr_mem_5938)[local_tid_5930] = ((__global\n                                                                     bool *) mem_5758)[gtid_5704];\n        } else {\n            ((__local int32_t *) scan_arr_mem_5934)[local_tid_5930] = 0;\n            ((__local int32_t *) scan_arr_mem_5936)[local_tid_5930] = 0;\n            ((__local bool *) scan_arr_mem_5938)[local_tid_5930] = 0;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t x_5905;\n    int32_t x_5906;\n    bool x_5907;\n    int32_t x_5908;\n    int32_t x_5909;\n    bool x_5910;\n    int32_t x_5941;\n    int32_t x_5942;\n    bool x_5943;\n    int32_t x_5944;\n    int32_t x_5945;\n    bool x_5946;\n    int32_t skip_threads_5953;\n    \n    if (slt32(local_tid_5930, num_groups_5702)) {\n        x_5908 = ((volatile __local\n                   int32_t *) scan_arr_mem_5934)[local_tid_5930];\n        x_5909 = ((volatile __local\n                   int32_t *) scan_arr_mem_5936)[local_tid_5930];\n        x_5910 = ((volatile __local bool *) scan_arr_mem_5938)[local_tid_5930];\n        if (sub32(local_tid_5930, mul32(squot32(local_tid_5930, 32), 32)) ==\n            0) {\n            x_5905 = x_5908;\n            x_5906 = x_5909;\n            x_5907 = x_5910;\n        }\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        skip_threads_5953 = 1;\n        whil",
                   "e (slt32(skip_threads_5953, 32)) {\n            if (sle32(skip_threads_5953, sub32(local_tid_5930,\n                                               mul32(squot32(local_tid_5930,\n                                                             32), 32))) &&\n                slt32(local_tid_5930, num_groups_5702)) {\n                // read operands\n                {\n                    x_5905 = ((volatile __local\n                               int32_t *) scan_arr_mem_5934)[sub32(local_tid_5930,\n                                                                   skip_threads_5953)];\n                    x_5906 = ((volatile __local\n                               int32_t *) scan_arr_mem_5936)[sub32(local_tid_5930,\n                                                                   skip_threads_5953)];\n                    x_5907 = ((volatile __local\n                               bool *) scan_arr_mem_5938)[sub32(local_tid_5930,\n                                                                skip_threads_5953)];\n                }\n                // perform operation\n                {\n                    int32_t res_5911 = add32(x_5905, x_5908);\n                    int32_t res_5912;\n                    bool res_5913;\n                    \n                    if (x_5910) {\n                        bool res_5914 = x_5907 || x_5910;\n                        \n                        res_5912 = x_5909;\n                        res_5913 = res_5914;\n                    } else {\n                        int32_t res_5915 = add32(x_5906, x_5909);\n                        bool res_5916 = x_5907 || x_5910;\n                        \n                        res_5912 = res_5915;\n                        res_5913 = res_5916;\n                    }\n                    x_5905 = res_5911;\n                    x_5906 = res_5912;\n                    x_5907 = res_5913;\n                }\n            }\n            if (sle32(wave_sizze_5932, skip_threads_5953)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n          ",
                   "  }\n            if (sle32(skip_threads_5953, sub32(local_tid_5930,\n                                               mul32(squot32(local_tid_5930,\n                                                             32), 32))) &&\n                slt32(local_tid_5930, num_groups_5702)) {\n                // write result\n                {\n                    ((volatile __local\n                      int32_t *) scan_arr_mem_5934)[local_tid_5930] = x_5905;\n                    x_5908 = x_5905;\n                    ((volatile __local\n                      int32_t *) scan_arr_mem_5936)[local_tid_5930] = x_5906;\n                    x_5909 = x_5906;\n                    ((volatile __local\n                      bool *) scan_arr_mem_5938)[local_tid_5930] = x_5907;\n                    x_5910 = x_5907;\n                }\n            }\n            if (sle32(wave_sizze_5932, skip_threads_5953)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_5953 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if (sub32(local_tid_5930, mul32(squot32(local_tid_5930, 32), 32)) ==\n            31 && slt32(local_tid_5930, num_groups_5702)) {\n            ((volatile __local\n              int32_t *) scan_arr_mem_5934)[squot32(local_tid_5930, 32)] =\n                x_5905;\n            ((volatile __local\n              int32_t *) scan_arr_mem_5936)[squot32(local_tid_5930, 32)] =\n                x_5906;\n            ((volatile __local\n              bool *) scan_arr_mem_5938)[squot32(local_tid_5930, 32)] = x_5907;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for block 'i+1'\n    {\n        int32_t skip_threads_5954;\n        \n        if (squot32(local_tid_5930, 32) == 0 && slt32(local_tid_5930,\n                                                      num_groups_5702)) {\n            x_5944 = ((volatile __local\n                       int3",
                   "2_t *) scan_arr_mem_5934)[local_tid_5930];\n            x_5945 = ((volatile __local\n                       int32_t *) scan_arr_mem_5936)[local_tid_5930];\n            x_5946 = ((volatile __local\n                       bool *) scan_arr_mem_5938)[local_tid_5930];\n            if (sub32(local_tid_5930, mul32(squot32(local_tid_5930, 32), 32)) ==\n                0) {\n                x_5941 = x_5944;\n                x_5942 = x_5945;\n                x_5943 = x_5946;\n            }\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            skip_threads_5954 = 1;\n            while (slt32(skip_threads_5954, 32)) {\n                if (sle32(skip_threads_5954, sub32(local_tid_5930,\n                                                   mul32(squot32(local_tid_5930,\n                                                                 32), 32))) &&\n                    (squot32(local_tid_5930, 32) == 0 && slt32(local_tid_5930,\n                                                               num_groups_5702))) {\n                    // read operands\n                    {\n                        x_5941 = ((volatile __local\n                                   int32_t *) scan_arr_mem_5934)[sub32(local_tid_5930,\n                                                                       skip_threads_5954)];\n                        x_5942 = ((volatile __local\n                                   int32_t *) scan_arr_mem_5936)[sub32(local_tid_5930,\n                                                                       skip_threads_5954)];\n                        x_5943 = ((volatile __local\n                                   bool *) scan_arr_mem_5938)[sub32(local_tid_5930,\n                                                                    skip_threads_5954)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_5947 = add32(x_5941, x_5944);\n                        int32_t res_5948;\n                        bool res_5",
                   "949;\n                        \n                        if (x_5946) {\n                            bool res_5950 = x_5943 || x_5946;\n                            \n                            res_5948 = x_5945;\n                            res_5949 = res_5950;\n                        } else {\n                            int32_t res_5951 = add32(x_5942, x_5945);\n                            bool res_5952 = x_5943 || x_5946;\n                            \n                            res_5948 = res_5951;\n                            res_5949 = res_5952;\n                        }\n                        x_5941 = res_5947;\n                        x_5942 = res_5948;\n                        x_5943 = res_5949;\n                    }\n                }\n                if (sle32(wave_sizze_5932, skip_threads_5954)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if (sle32(skip_threads_5954, sub32(local_tid_5930,\n                                                   mul32(squot32(local_tid_5930,\n                                                                 32), 32))) &&\n                    (squot32(local_tid_5930, 32) == 0 && slt32(local_tid_5930,\n                                                               num_groups_5702))) {\n                    // write result\n                    {\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_5934)[local_tid_5930] =\n                            x_5941;\n                        x_5944 = x_5941;\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_5936)[local_tid_5930] =\n                            x_5942;\n                        x_5945 = x_5942;\n                        ((volatile __local\n                          bool *) scan_arr_mem_5938)[local_tid_5930] = x_5943;\n                        x_5946 = x_5943;\n                    }\n                }\n                if (sle32(wave_sizze_5932, skip_threads_5954)) {\n                    b",
                   "arrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_5954 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_5930, 32) == 0 || !slt32(local_tid_5930,\n                                                         num_groups_5702))) {\n            // read operands\n            {\n                x_5908 = x_5905;\n                x_5909 = x_5906;\n                x_5910 = x_5907;\n                x_5905 = ((__local\n                           int32_t *) scan_arr_mem_5934)[sub32(squot32(local_tid_5930,\n                                                                       32), 1)];\n                x_5906 = ((__local\n                           int32_t *) scan_arr_mem_5936)[sub32(squot32(local_tid_5930,\n                                                                       32), 1)];\n                x_5907 = ((__local\n                           bool *) scan_arr_mem_5938)[sub32(squot32(local_tid_5930,\n                                                                    32), 1)];\n            }\n            // perform operation\n            {\n                int32_t res_5911 = add32(x_5905, x_5908);\n                int32_t res_5912;\n                bool res_5913;\n                \n                if (x_5910) {\n                    bool res_5914 = x_5907 || x_5910;\n                    \n                    res_5912 = x_5909;\n                    res_5913 = res_5914;\n                } else {\n                    int32_t res_5915 = add32(x_5906, x_5909);\n                    bool res_5916 = x_5907 || x_5910;\n                    \n                    res_5912 = res_5915;\n                    res_5913 = res_5916;\n                }\n                x_5905 = res_5911;\n                x_5906 = res_5912;\n                x_5907 = res_5913;\n            }\n            // write final result\n            {\n                ((__local int32_t *) scan_arr_mem_5934)[local_tid_5930] =\n         ",
                   "           x_5905;\n                ((__local int32_t *) scan_arr_mem_5936)[local_tid_5930] =\n                    x_5906;\n                ((__local bool *) scan_arr_mem_5938)[local_tid_5930] = x_5907;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(local_tid_5930, 32) == 0) {\n            ((__local int32_t *) scan_arr_mem_5934)[local_tid_5930] = x_5908;\n            ((__local int32_t *) scan_arr_mem_5936)[local_tid_5930] = x_5909;\n            ((__local bool *) scan_arr_mem_5938)[local_tid_5930] = x_5910;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // threads in bounds write scanned carries\n    {\n        if (slt32(gtid_5704, n_5540)) {\n            ((__global int32_t *) mem_5753)[gtid_5704] = ((__local\n                                                           int32_t *) scan_arr_mem_5934)[local_tid_5930];\n            ((__global int32_t *) mem_5756)[gtid_5704] = ((__local\n                                                           int32_t *) scan_arr_mem_5936)[local_tid_5930];\n            ((__global bool *) mem_5758)[gtid_5704] = ((__local\n                                                        bool *) scan_arr_mem_5938)[local_tid_5930];\n        }\n    }\n    \n  error_0:\n    return;\n    #undef segscan_group_sizze_5700\n}\n__kernel void scan_stage3_5696(__global int *global_failure, int32_t n_5540,\n                               int32_t num_groups_5693, __global\n                               unsigned char *mem_5747, __global\n                               unsigned char *mem_5749,\n                               int32_t num_threads_5775,\n                               int32_t required_groups_5849)\n{\n    #define segscan_group_sizze_5691 (mainzisegscan_group_sizze_5690)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_5850;\n    int32_t local_tid_5851;\n    int32",
                   "_t group_sizze_5854;\n    int32_t wave_sizze_5853;\n    int32_t group_tid_5852;\n    \n    global_tid_5850 = get_global_id(0);\n    local_tid_5851 = get_local_id(0);\n    group_sizze_5854 = get_local_size(0);\n    wave_sizze_5853 = LOCKSTEP_WIDTH;\n    group_tid_5852 = get_group_id(0);\n    \n    int32_t phys_tid_5696 = global_tid_5850;\n    int32_t phys_group_id_5855;\n    \n    phys_group_id_5855 = get_group_id(0);\n    for (int32_t i_5856 = 0; i_5856 <\n         squot32(sub32(add32(sub32(required_groups_5849, phys_group_id_5855),\n                             num_groups_5693), 1), num_groups_5693); i_5856++) {\n        int32_t virt_group_id_5857 = add32(phys_group_id_5855, mul32(i_5856,\n                                                                     num_groups_5693));\n        int32_t flat_idx_5858 = add32(mul32(virt_group_id_5857,\n                                            segscan_group_sizze_5691),\n                                      local_tid_5851);\n        int32_t gtid_5695 = flat_idx_5858;\n        int32_t orig_group_5859 = squot32(flat_idx_5858,\n                                          mul32(segscan_group_sizze_5691,\n                                                squot32(sub32(add32(n_5540,\n                                                                    num_threads_5775),\n                                                              1),\n                                                        num_threads_5775)));\n        int32_t carry_in_flat_idx_5860 = sub32(mul32(orig_group_5859,\n                                                     mul32(segscan_group_sizze_5691,\n                                                           squot32(sub32(add32(n_5540,\n                                                                               num_threads_5775),\n                                                                         1),\n                                                                   num_threads_5775))),\n                                               1);\n    ",
                   "    \n        if (slt32(gtid_5695, n_5540)) {\n            if (!(orig_group_5859 == 0 || flat_idx_5858 ==\n                  sub32(mul32(add32(orig_group_5859, 1),\n                              mul32(segscan_group_sizze_5691,\n                                    squot32(sub32(add32(n_5540,\n                                                        num_threads_5775), 1),\n                                            num_threads_5775))), 1))) {\n                int32_t x_5819;\n                bool x_5820;\n                int32_t x_5821;\n                bool x_5822;\n                \n                x_5819 = ((__global\n                           int32_t *) mem_5747)[carry_in_flat_idx_5860];\n                x_5820 = ((__global bool *) mem_5749)[carry_in_flat_idx_5860];\n                x_5821 = ((__global int32_t *) mem_5747)[gtid_5695];\n                x_5822 = ((__global bool *) mem_5749)[gtid_5695];\n                \n                int32_t res_5823;\n                bool res_5824;\n                \n                if (x_5822) {\n                    bool res_5825 = x_5820 || x_5822;\n                    \n                    res_5823 = x_5821;\n                    res_5824 = res_5825;\n                } else {\n                    int32_t res_5826 = add32(x_5819, x_5821);\n                    bool res_5827 = x_5820 || x_5822;\n                    \n                    res_5823 = res_5826;\n                    res_5824 = res_5827;\n                }\n                x_5819 = res_5823;\n                x_5820 = res_5824;\n                ((__global int32_t *) mem_5747)[gtid_5695] = x_5819;\n                ((__global bool *) mem_5749)[gtid_5695] = x_5820;\n            }\n        }\n        barrier(CLK_GLOBAL_MEM_FENCE);\n    }\n    \n  error_0:\n    return;\n    #undef segscan_group_sizze_5691\n}\n__kernel void scan_stage3_5705(__global int *global_failure, int32_t n_5540,\n                               int32_t num_groups_5702, __global\n                               unsigned char *mem_5753, __global\n       ",
                   "                        unsigned char *mem_5756, __global\n                               unsigned char *mem_5758,\n                               int32_t num_threads_5862,\n                               int32_t required_groups_5955)\n{\n    #define segscan_group_sizze_5700 (mainzisegscan_group_sizze_5699)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_5956;\n    int32_t local_tid_5957;\n    int32_t group_sizze_5960;\n    int32_t wave_sizze_5959;\n    int32_t group_tid_5958;\n    \n    global_tid_5956 = get_global_id(0);\n    local_tid_5957 = get_local_id(0);\n    group_sizze_5960 = get_local_size(0);\n    wave_sizze_5959 = LOCKSTEP_WIDTH;\n    group_tid_5958 = get_group_id(0);\n    \n    int32_t phys_tid_5705 = global_tid_5956;\n    int32_t phys_group_id_5961;\n    \n    phys_group_id_5961 = get_group_id(0);\n    for (int32_t i_5962 = 0; i_5962 <\n         squot32(sub32(add32(sub32(required_groups_5955, phys_group_id_5961),\n                             num_groups_5702), 1), num_groups_5702); i_5962++) {\n        int32_t virt_group_id_5963 = add32(phys_group_id_5961, mul32(i_5962,\n                                                                     num_groups_5702));\n        int32_t flat_idx_5964 = add32(mul32(virt_group_id_5963,\n                                            segscan_group_sizze_5700),\n                                      local_tid_5957);\n        int32_t gtid_5704 = flat_idx_5964;\n        int32_t orig_group_5965 = squot32(flat_idx_5964,\n                                          mul32(segscan_group_sizze_5700,\n                                                squot32(sub32(add32(n_5540,\n                                                                    num_threads_5862),\n                                                              1),\n                                                        num_threads_5862)));\n        int32_t carry_in_flat_idx_5966 ",
                   "= sub32(mul32(orig_group_5965,\n                                                     mul32(segscan_group_sizze_5700,\n                                                           squot32(sub32(add32(n_5540,\n                                                                               num_threads_5862),\n                                                                         1),\n                                                                   num_threads_5862))),\n                                               1);\n        \n        if (slt32(gtid_5704, n_5540)) {\n            if (!(orig_group_5965 == 0 || flat_idx_5964 ==\n                  sub32(mul32(add32(orig_group_5965, 1),\n                              mul32(segscan_group_sizze_5700,\n                                    squot32(sub32(add32(n_5540,\n                                                        num_threads_5862), 1),\n                                            num_threads_5862))), 1))) {\n                int32_t x_5917;\n                int32_t x_5918;\n                bool x_5919;\n                int32_t x_5920;\n                int32_t x_5921;\n                bool x_5922;\n                \n                x_5917 = ((__global\n                           int32_t *) mem_5753)[carry_in_flat_idx_5966];\n                x_5918 = ((__global\n                           int32_t *) mem_5756)[carry_in_flat_idx_5966];\n                x_5919 = ((__global bool *) mem_5758)[carry_in_flat_idx_5966];\n                x_5920 = ((__global int32_t *) mem_5753)[gtid_5704];\n                x_5921 = ((__global int32_t *) mem_5756)[gtid_5704];\n                x_5922 = ((__global bool *) mem_5758)[gtid_5704];\n                \n                int32_t res_5923 = add32(x_5917, x_5920);\n                int32_t res_5924;\n                bool res_5925;\n                \n                if (x_5922) {\n                    bool res_5926 = x_5919 || x_5922;\n                    \n                    res_5924 = x_5921;\n                    res_5925 = res_5",
                   "926;\n                } else {\n                    int32_t res_5927 = add32(x_5918, x_5921);\n                    bool res_5928 = x_5919 || x_5922;\n                    \n                    res_5924 = res_5927;\n                    res_5925 = res_5928;\n                }\n                x_5917 = res_5923;\n                x_5918 = res_5924;\n                x_5919 = res_5925;\n                ((__global int32_t *) mem_5753)[gtid_5704] = x_5917;\n                ((__global int32_t *) mem_5756)[gtid_5704] = x_5918;\n                ((__global bool *) mem_5758)[gtid_5704] = x_5919;\n            }\n        }\n        barrier(CLK_GLOBAL_MEM_FENCE);\n    }\n    \n  error_0:\n    return;\n    #undef segscan_group_sizze_5700\n}\n__kernel void segmap_5728(__global int *global_failure, int32_t n_5540, __global\n                          unsigned char *mem_5753, __global\n                          unsigned char *mem_5756, __global\n                          unsigned char *mem_5761)\n{\n    #define segmap_group_sizze_5732 (mainzisegmap_group_sizze_5731)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_5967;\n    int32_t local_tid_5968;\n    int32_t group_sizze_5971;\n    int32_t wave_sizze_5970;\n    int32_t group_tid_5969;\n    \n    global_tid_5967 = get_global_id(0);\n    local_tid_5968 = get_local_id(0);\n    group_sizze_5971 = get_local_size(0);\n    wave_sizze_5970 = LOCKSTEP_WIDTH;\n    group_tid_5969 = get_group_id(0);\n    \n    int32_t phys_tid_5728 = global_tid_5967;\n    int32_t write_i_5727 = add32(mul32(group_tid_5969, segmap_group_sizze_5732),\n                                 local_tid_5968);\n    \n    if (slt32(write_i_5727, n_5540)) {\n        int32_t write_index_5670 = ((__global\n                                     int32_t *) mem_5753)[write_i_5727];\n        int32_t write_value_5671 = ((__global\n                                     int32_t *) mem_5756)[write_i_5727];\n        ",
                   "\n        if (sle32(0, write_index_5670) && slt32(write_index_5670, n_5540)) {\n            ((__global int32_t *) mem_5761)[write_index_5670] =\n                write_value_5671;\n        }\n    }\n    \n  error_0:\n    return;\n    #undef segmap_group_sizze_5732\n}\n",
                   NULL};
static const char *size_names[] = {"main.segmap_group_size_5731",
                                   "main.segscan_group_size_5690",
                                   "main.segscan_group_size_5699",
                                   "main.segscan_num_groups_5692",
                                   "main.segscan_num_groups_5701"};
static const char *size_vars[] = {"mainzisegmap_group_sizze_5731",
                                  "mainzisegscan_group_sizze_5690",
                                  "mainzisegscan_group_sizze_5699",
                                  "mainzisegscan_num_groups_5692",
                                  "mainzisegscan_num_groups_5701"};
static const char *size_classes[] = {"group_size", "group_size", "group_size",
                                     "num_groups", "num_groups"};
int futhark_get_num_sizes(void)
{
    return 5;
}
const char *futhark_get_size_name(int i)
{
    return size_names[i];
}
const char *futhark_get_size_class(int i)
{
    return size_classes[i];
}
struct sizes {
    size_t mainzisegmap_group_sizze_5731;
    size_t mainzisegscan_group_sizze_5690;
    size_t mainzisegscan_group_sizze_5699;
    size_t mainzisegscan_num_groups_5692;
    size_t mainzisegscan_num_groups_5701;
} ;
struct futhark_context_config {
    struct opencl_config opencl;
    size_t sizes[5];
    int num_build_opts;
    const char **build_opts;
} ;
struct futhark_context_config *futhark_context_config_new(void)
{
    struct futhark_context_config *cfg =
                                  (struct futhark_context_config *) malloc(sizeof(struct futhark_context_config));
    
    if (cfg == NULL)
        return NULL;
    cfg->num_build_opts = 0;
    cfg->build_opts = (const char **) malloc(sizeof(const char *));
    cfg->build_opts[0] = NULL;
    cfg->sizes[0] = 0;
    cfg->sizes[1] = 0;
    cfg->sizes[2] = 0;
    cfg->sizes[3] = 0;
    cfg->sizes[4] = 0;
    opencl_config_init(&cfg->opencl, 5, size_names, size_vars, cfg->sizes,
                       size_classes);
    return cfg;
}
void futhark_context_config_free(struct futhark_context_config *cfg)
{
    free(cfg->build_opts);
    free(cfg);
}
void futhark_context_config_add_build_option(struct futhark_context_config *cfg,
                                             const char *opt)
{
    cfg->build_opts[cfg->num_build_opts] = opt;
    cfg->num_build_opts++;
    cfg->build_opts = (const char **) realloc(cfg->build_opts,
                                              (cfg->num_build_opts + 1) *
                                              sizeof(const char *));
    cfg->build_opts[cfg->num_build_opts] = NULL;
}
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int flag)
{
    cfg->opencl.profiling = cfg->opencl.logging = cfg->opencl.debugging = flag;
}
void futhark_context_config_set_profiling(struct futhark_context_config *cfg,
                                          int flag)
{
    cfg->opencl.profiling = flag;
}
void futhark_context_config_set_logging(struct futhark_context_config *cfg,
                                        int flag)
{
    cfg->opencl.logging = flag;
}
void futhark_context_config_set_device(struct futhark_context_config *cfg, const
                                       char *s)
{
    set_preferred_device(&cfg->opencl, s);
}
void futhark_context_config_set_platform(struct futhark_context_config *cfg,
                                         const char *s)
{
    set_preferred_platform(&cfg->opencl, s);
}
void futhark_context_config_select_device_interactively(struct futhark_context_config *cfg)
{
    select_device_interactively(&cfg->opencl);
}
void futhark_context_config_dump_program_to(struct futhark_context_config *cfg,
                                            const char *path)
{
    cfg->opencl.dump_program_to = path;
}
void futhark_context_config_load_program_from(struct futhark_context_config *cfg,
                                              const char *path)
{
    cfg->opencl.load_program_from = path;
}
void futhark_context_config_dump_binary_to(struct futhark_context_config *cfg,
                                           const char *path)
{
    cfg->opencl.dump_binary_to = path;
}
void futhark_context_config_load_binary_from(struct futhark_context_config *cfg,
                                             const char *path)
{
    cfg->opencl.load_binary_from = path;
}
void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg,
                                                   int size)
{
    cfg->opencl.default_group_size = size;
    cfg->opencl.default_group_size_changed = 1;
}
void futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg,
                                                   int num)
{
    cfg->opencl.default_num_groups = num;
}
void futhark_context_config_set_default_tile_size(struct futhark_context_config *cfg,
                                                  int size)
{
    cfg->opencl.default_tile_size = size;
    cfg->opencl.default_tile_size_changed = 1;
}
void futhark_context_config_set_default_threshold(struct futhark_context_config *cfg,
                                                  int size)
{
    cfg->opencl.default_threshold = size;
}
int futhark_context_config_set_size(struct futhark_context_config *cfg, const
                                    char *size_name, size_t size_value)
{
    for (int i = 0; i < 5; i++) {
        if (strcmp(size_name, size_names[i]) == 0) {
            cfg->sizes[i] = size_value;
            return 0;
        }
    }
    if (strcmp(size_name, "default_group_size") == 0) {
        cfg->opencl.default_group_size = size_value;
        return 0;
    }
    if (strcmp(size_name, "default_num_groups") == 0) {
        cfg->opencl.default_num_groups = size_value;
        return 0;
    }
    if (strcmp(size_name, "default_threshold") == 0) {
        cfg->opencl.default_threshold = size_value;
        return 0;
    }
    if (strcmp(size_name, "default_tile_size") == 0) {
        cfg->opencl.default_tile_size = size_value;
        return 0;
    }
    return 1;
}
struct futhark_context {
    int detail_memory;
    int debugging;
    int profiling;
    int profiling_paused;
    int logging;
    lock_t lock;
    char *error;
    int64_t peak_mem_usage_device;
    int64_t cur_mem_usage_device;
    int64_t peak_mem_usage_default;
    int64_t cur_mem_usage_default;
    struct { } constants;
    int total_runs;
    long total_runtime;
    cl_kernel scan_stage1_5696;
    cl_kernel scan_stage1_5705;
    cl_kernel scan_stage2_5696;
    cl_kernel scan_stage2_5705;
    cl_kernel scan_stage3_5696;
    cl_kernel scan_stage3_5705;
    cl_kernel segmap_5728;
    int64_t copy_dev_to_dev_total_runtime;
    int copy_dev_to_dev_runs;
    int64_t copy_dev_to_host_total_runtime;
    int copy_dev_to_host_runs;
    int64_t copy_host_to_dev_total_runtime;
    int copy_host_to_dev_runs;
    int64_t copy_scalar_to_dev_total_runtime;
    int copy_scalar_to_dev_runs;
    int64_t copy_scalar_from_dev_total_runtime;
    int copy_scalar_from_dev_runs;
    int64_t scan_stage1_5696_total_runtime;
    int scan_stage1_5696_runs;
    int64_t scan_stage1_5705_total_runtime;
    int scan_stage1_5705_runs;
    int64_t scan_stage2_5696_total_runtime;
    int scan_stage2_5696_runs;
    int64_t scan_stage2_5705_total_runtime;
    int scan_stage2_5705_runs;
    int64_t scan_stage3_5696_total_runtime;
    int scan_stage3_5696_runs;
    int64_t scan_stage3_5705_total_runtime;
    int scan_stage3_5705_runs;
    int64_t segmap_5728_total_runtime;
    int segmap_5728_runs;
    cl_mem global_failure;
    cl_mem global_failure_args;
    struct opencl_context opencl;
    struct sizes sizes;
    cl_int failure_is_an_option;
} ;
void post_opencl_setup(struct opencl_context *ctx,
                       struct opencl_device_option *option)
{
    if ((ctx->lockstep_width == 0 && strstr(option->platform_name,
                                            "NVIDIA CUDA") != NULL) &&
        (option->device_type & CL_DEVICE_TYPE_GPU) == CL_DEVICE_TYPE_GPU)
        ctx->lockstep_width = 32;
    if ((ctx->lockstep_width == 0 && strstr(option->platform_name,
                                            "AMD Accelerated Parallel Processing") !=
         NULL) && (option->device_type & CL_DEVICE_TYPE_GPU) ==
        CL_DEVICE_TYPE_GPU)
        ctx->lockstep_width = 32;
    if ((ctx->lockstep_width == 0 && strstr(option->platform_name, "") !=
         NULL) && (option->device_type & CL_DEVICE_TYPE_GPU) ==
        CL_DEVICE_TYPE_GPU)
        ctx->lockstep_width = 1;
    if ((ctx->cfg.default_num_groups == 0 && strstr(option->platform_name,
                                                    "") != NULL) &&
        (option->device_type & CL_DEVICE_TYPE_GPU) == CL_DEVICE_TYPE_GPU)
        ctx->cfg.default_num_groups = 256;
    if ((ctx->cfg.default_group_size == 0 && strstr(option->platform_name,
                                                    "") != NULL) &&
        (option->device_type & CL_DEVICE_TYPE_GPU) == CL_DEVICE_TYPE_GPU)
        ctx->cfg.default_group_size = 256;
    if ((ctx->cfg.default_tile_size == 0 && strstr(option->platform_name, "") !=
         NULL) && (option->device_type & CL_DEVICE_TYPE_GPU) ==
        CL_DEVICE_TYPE_GPU)
        ctx->cfg.default_tile_size = 32;
    if ((ctx->cfg.default_threshold == 0 && strstr(option->platform_name, "") !=
         NULL) && (option->device_type & CL_DEVICE_TYPE_GPU) ==
        CL_DEVICE_TYPE_GPU)
        ctx->cfg.default_threshold = 32768;
    if ((ctx->lockstep_width == 0 && strstr(option->platform_name, "") !=
         NULL) && (option->device_type & CL_DEVICE_TYPE_CPU) ==
        CL_DEVICE_TYPE_CPU)
        ctx->lockstep_width = 1;
    if ((ctx->cfg.default_num_groups == 0 && strstr(option->platform_name,
                                                    "") != NULL) &&
        (option->device_type & CL_DEVICE_TYPE_CPU) == CL_DEVICE_TYPE_CPU)
        clGetDeviceInfo(ctx->device, CL_DEVICE_MAX_COMPUTE_UNITS,
                        sizeof(ctx->cfg.default_num_groups),
                        &ctx->cfg.default_num_groups, NULL);
    if ((ctx->cfg.default_group_size == 0 && strstr(option->platform_name,
                                                    "") != NULL) &&
        (option->device_type & CL_DEVICE_TYPE_CPU) == CL_DEVICE_TYPE_CPU)
        ctx->cfg.default_group_size = 32;
    if ((ctx->cfg.default_tile_size == 0 && strstr(option->platform_name, "") !=
         NULL) && (option->device_type & CL_DEVICE_TYPE_CPU) ==
        CL_DEVICE_TYPE_CPU)
        ctx->cfg.default_tile_size = 4;
    if ((ctx->cfg.default_threshold == 0 && strstr(option->platform_name, "") !=
         NULL) && (option->device_type & CL_DEVICE_TYPE_CPU) ==
        CL_DEVICE_TYPE_CPU)
        clGetDeviceInfo(ctx->device, CL_DEVICE_MAX_COMPUTE_UNITS,
                        sizeof(ctx->cfg.default_threshold),
                        &ctx->cfg.default_threshold, NULL);
}
static void init_context_early(struct futhark_context_config *cfg,
                               struct futhark_context *ctx)
{
    ctx->opencl.cfg = cfg->opencl;
    ctx->detail_memory = cfg->opencl.debugging;
    ctx->debugging = cfg->opencl.debugging;
    ctx->profiling = cfg->opencl.profiling;
    ctx->profiling_paused = 0;
    ctx->logging = cfg->opencl.logging;
    ctx->error = NULL;
    ctx->opencl.profiling_records_capacity = 200;
    ctx->opencl.profiling_records_used = 0;
    ctx->opencl.profiling_records =
        malloc(ctx->opencl.profiling_records_capacity *
        sizeof(struct profiling_record));
    create_lock(&ctx->lock);
    ctx->failure_is_an_option = 0;
    ctx->peak_mem_usage_device = 0;
    ctx->cur_mem_usage_device = 0;
    ctx->peak_mem_usage_default = 0;
    ctx->cur_mem_usage_default = 0;
    ctx->total_runs = 0;
    ctx->total_runtime = 0;
    ctx->copy_dev_to_dev_total_runtime = 0;
    ctx->copy_dev_to_dev_runs = 0;
    ctx->copy_dev_to_host_total_runtime = 0;
    ctx->copy_dev_to_host_runs = 0;
    ctx->copy_host_to_dev_total_runtime = 0;
    ctx->copy_host_to_dev_runs = 0;
    ctx->copy_scalar_to_dev_total_runtime = 0;
    ctx->copy_scalar_to_dev_runs = 0;
    ctx->copy_scalar_from_dev_total_runtime = 0;
    ctx->copy_scalar_from_dev_runs = 0;
    ctx->scan_stage1_5696_total_runtime = 0;
    ctx->scan_stage1_5696_runs = 0;
    ctx->scan_stage1_5705_total_runtime = 0;
    ctx->scan_stage1_5705_runs = 0;
    ctx->scan_stage2_5696_total_runtime = 0;
    ctx->scan_stage2_5696_runs = 0;
    ctx->scan_stage2_5705_total_runtime = 0;
    ctx->scan_stage2_5705_runs = 0;
    ctx->scan_stage3_5696_total_runtime = 0;
    ctx->scan_stage3_5696_runs = 0;
    ctx->scan_stage3_5705_total_runtime = 0;
    ctx->scan_stage3_5705_runs = 0;
    ctx->segmap_5728_total_runtime = 0;
    ctx->segmap_5728_runs = 0;
}
static int init_context_late(struct futhark_context_config *cfg,
                             struct futhark_context *ctx, cl_program prog)
{
    cl_int error;
    cl_int no_error = -1;
    
    ctx->global_failure = clCreateBuffer(ctx->opencl.ctx, CL_MEM_READ_WRITE |
                                         CL_MEM_COPY_HOST_PTR, sizeof(cl_int),
                                         &no_error, &error);
    OPENCL_SUCCEED_OR_RETURN(error);
    // The +1 is to avoid zero-byte allocations.
    ctx->global_failure_args = clCreateBuffer(ctx->opencl.ctx,
                                              CL_MEM_READ_WRITE,
                                              sizeof(cl_int) * (0 + 1), NULL,
                                              &error);
    OPENCL_SUCCEED_OR_RETURN(error);
    {
        ctx->scan_stage1_5696 = clCreateKernel(prog, "scan_stage1_5696",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage1_5696, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage1_5696");
    }
    {
        ctx->scan_stage1_5705 = clCreateKernel(prog, "scan_stage1_5705",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage1_5705, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage1_5705");
    }
    {
        ctx->scan_stage2_5696 = clCreateKernel(prog, "scan_stage2_5696",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage2_5696, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage2_5696");
    }
    {
        ctx->scan_stage2_5705 = clCreateKernel(prog, "scan_stage2_5705",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage2_5705, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage2_5705");
    }
    {
        ctx->scan_stage3_5696 = clCreateKernel(prog, "scan_stage3_5696",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage3_5696, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage3_5696");
    }
    {
        ctx->scan_stage3_5705 = clCreateKernel(prog, "scan_stage3_5705",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage3_5705, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage3_5705");
    }
    {
        ctx->segmap_5728 = clCreateKernel(prog, "segmap_5728", &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->segmap_5728, 0, sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "segmap_5728");
    }
    ctx->sizes.mainzisegmap_group_sizze_5731 = cfg->sizes[0];
    ctx->sizes.mainzisegscan_group_sizze_5690 = cfg->sizes[1];
    ctx->sizes.mainzisegscan_group_sizze_5699 = cfg->sizes[2];
    ctx->sizes.mainzisegscan_num_groups_5692 = cfg->sizes[3];
    ctx->sizes.mainzisegscan_num_groups_5701 = cfg->sizes[4];
    init_constants(ctx);
    // Clear the free list of any deallocations that occurred while initialising constants.
    OPENCL_SUCCEED_OR_RETURN(opencl_free_all(&ctx->opencl));
    return futhark_context_sync(ctx);
}
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg)
{
    struct futhark_context *ctx =
                           (struct futhark_context *) malloc(sizeof(struct futhark_context));
    
    if (ctx == NULL)
        return NULL;
    
    int required_types = 0;
    
    init_context_early(cfg, ctx);
    
    cl_program prog = setup_opencl(&ctx->opencl, opencl_program, required_types,
                                   cfg->build_opts);
    
    init_context_late(cfg, ctx, prog);
    return ctx;
}
struct futhark_context *futhark_context_new_with_command_queue(struct futhark_context_config *cfg,
                                                               cl_command_queue queue)
{
    struct futhark_context *ctx =
                           (struct futhark_context *) malloc(sizeof(struct futhark_context));
    
    if (ctx == NULL)
        return NULL;
    
    int required_types = 0;
    
    init_context_early(cfg, ctx);
    
    cl_program prog = setup_opencl_with_command_queue(&ctx->opencl, queue,
                                                      opencl_program,
                                                      required_types,
                                                      cfg->build_opts);
    
    init_context_late(cfg, ctx, prog);
    return ctx;
}
void futhark_context_free(struct futhark_context *ctx)
{
    free_constants(ctx);
    free_lock(&ctx->lock);
    opencl_tally_profiling_records(&ctx->opencl);
    free(ctx->opencl.profiling_records);
    free(ctx);
}
int futhark_context_sync(struct futhark_context *ctx)
{
    cl_int failure_idx = -1;
    
    if (ctx->failure_is_an_option) {
        OPENCL_SUCCEED_OR_RETURN(clEnqueueReadBuffer(ctx->opencl.queue,
                                                     ctx->global_failure,
                                                     CL_FALSE, 0,
                                                     sizeof(cl_int),
                                                     &failure_idx, 0, NULL,
                                                     ctx->profiling_paused ||
                                                     !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                               &ctx->copy_scalar_from_dev_runs,
                                                                                               &ctx->copy_scalar_from_dev_total_runtime)));
        ctx->failure_is_an_option = 0;
    }
    OPENCL_SUCCEED_OR_RETURN(clFinish(ctx->opencl.queue));
    if (failure_idx >= 0) {
        cl_int args[0 + 1];
        
        OPENCL_SUCCEED_OR_RETURN(clEnqueueReadBuffer(ctx->opencl.queue,
                                                     ctx->global_failure_args,
                                                     CL_TRUE, 0, sizeof(args),
                                                     &args, 0, NULL,
                                                     ctx->profiling_paused ||
                                                     !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                               &ctx->copy_dev_to_host_runs,
                                                                                               &ctx->copy_dev_to_host_total_runtime)));
        switch (failure_idx) { }
        return 1;
    }
    return 0;
}
char *futhark_context_get_error(struct futhark_context *ctx)
{
    char *error = ctx->error;
    
    ctx->error = NULL;
    return error;
}
void futhark_context_pause_profiling(struct futhark_context *ctx)
{
    ctx->profiling_paused = 1;
}
void futhark_context_unpause_profiling(struct futhark_context *ctx)
{
    ctx->profiling_paused = 0;
}
int futhark_context_clear_caches(struct futhark_context *ctx)
{
    ctx->error = OPENCL_SUCCEED_NONFATAL(opencl_free_all(&ctx->opencl));
    return ctx->error != NULL;
}
cl_command_queue futhark_context_get_command_queue(struct futhark_context *ctx)
{
    return ctx->opencl.queue;
}
static int memblock_unref_device(struct futhark_context *ctx,
                                 struct memblock_device *block, const
                                 char *desc)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (ctx->detail_memory)
            fprintf(stderr,
                    "Unreferencing block %s (allocated as %s) in %s: %d references remaining.\n",
                    desc, block->desc, "space 'device'", *block->references);
        if (*block->references == 0) {
            ctx->cur_mem_usage_device -= block->size;
            OPENCL_SUCCEED_OR_RETURN(opencl_free(&ctx->opencl, block->mem,
                                                 block->desc));
            free(block->references);
            if (ctx->detail_memory)
                fprintf(stderr,
                        "%lld bytes freed (now allocated: %lld bytes)\n",
                        (long long) block->size,
                        (long long) ctx->cur_mem_usage_device);
        }
        block->references = NULL;
    }
    return 0;
}
static int memblock_alloc_device(struct futhark_context *ctx,
                                 struct memblock_device *block, int64_t size,
                                 const char *desc)
{
    if (size < 0)
        futhark_panic(1,
                      "Negative allocation of %lld bytes attempted for %s in %s.\n",
                      (long long) size, desc, "space 'device'",
                      ctx->cur_mem_usage_device);
    
    int ret = memblock_unref_device(ctx, block, desc);
    
    ctx->cur_mem_usage_device += size;
    if (ctx->detail_memory)
        fprintf(stderr,
                "Allocating %lld bytes for %s in %s (then allocated: %lld bytes)",
                (long long) size, desc, "space 'device'",
                (long long) ctx->cur_mem_usage_device);
    if (ctx->cur_mem_usage_device > ctx->peak_mem_usage_device) {
        ctx->peak_mem_usage_device = ctx->cur_mem_usage_device;
        if (ctx->detail_memory)
            fprintf(stderr, " (new peak).\n");
    } else if (ctx->detail_memory)
        fprintf(stderr, ".\n");
    OPENCL_SUCCEED_OR_RETURN(opencl_alloc(&ctx->opencl, size, desc,
                                          &block->mem));
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    block->desc = desc;
    return ret;
}
static int memblock_set_device(struct futhark_context *ctx,
                               struct memblock_device *lhs,
                               struct memblock_device *rhs, const
                               char *lhs_desc)
{
    int ret = memblock_unref_device(ctx, lhs, lhs_desc);
    
    (*rhs->references)++;
    *lhs = *rhs;
    return ret;
}
static int memblock_unref(struct futhark_context *ctx, struct memblock *block,
                          const char *desc)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (ctx->detail_memory)
            fprintf(stderr,
                    "Unreferencing block %s (allocated as %s) in %s: %d references remaining.\n",
                    desc, block->desc, "default space", *block->references);
        if (*block->references == 0) {
            ctx->cur_mem_usage_default -= block->size;
            free(block->mem);
            free(block->references);
            if (ctx->detail_memory)
                fprintf(stderr,
                        "%lld bytes freed (now allocated: %lld bytes)\n",
                        (long long) block->size,
                        (long long) ctx->cur_mem_usage_default);
        }
        block->references = NULL;
    }
    return 0;
}
static int memblock_alloc(struct futhark_context *ctx, struct memblock *block,
                          int64_t size, const char *desc)
{
    if (size < 0)
        futhark_panic(1,
                      "Negative allocation of %lld bytes attempted for %s in %s.\n",
                      (long long) size, desc, "default space",
                      ctx->cur_mem_usage_default);
    
    int ret = memblock_unref(ctx, block, desc);
    
    ctx->cur_mem_usage_default += size;
    if (ctx->detail_memory)
        fprintf(stderr,
                "Allocating %lld bytes for %s in %s (then allocated: %lld bytes)",
                (long long) size, desc, "default space",
                (long long) ctx->cur_mem_usage_default);
    if (ctx->cur_mem_usage_default > ctx->peak_mem_usage_default) {
        ctx->peak_mem_usage_default = ctx->cur_mem_usage_default;
        if (ctx->detail_memory)
            fprintf(stderr, " (new peak).\n");
    } else if (ctx->detail_memory)
        fprintf(stderr, ".\n");
    block->mem = (char *) malloc(size);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    block->desc = desc;
    return ret;
}
static int memblock_set(struct futhark_context *ctx, struct memblock *lhs,
                        struct memblock *rhs, const char *lhs_desc)
{
    int ret = memblock_unref(ctx, lhs, lhs_desc);
    
    (*rhs->references)++;
    *lhs = *rhs;
    return ret;
}
static int futrts_main(struct futhark_context *ctx,
                       struct memblock_device *out_mem_p_5972,
                       int32_t *out_out_arrsizze_5973,
                       struct memblock_device k_mem_5742,
                       struct memblock_device d_mem_5743, int32_t n_5540,
                       int32_t n_5541);
int init_constants(struct futhark_context *ctx)
{
    return 0;
}
int free_constants(struct futhark_context *ctx)
{
    return 0;
}
void futhark_debugging_report(struct futhark_context *ctx)
{
    if (ctx->detail_memory || ctx->profiling) {
        fprintf(stderr, "Peak memory usage for space 'device': %lld bytes.\n",
                (long long) ctx->peak_mem_usage_device);
        fprintf(stderr, "Peak memory usage for default space: %lld bytes.\n",
                (long long) ctx->peak_mem_usage_default);
    }
    if (ctx->profiling) {
        OPENCL_SUCCEED_FATAL(opencl_tally_profiling_records(&ctx->opencl));
        fprintf(stderr,
                "copy_dev_to_dev      ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->copy_dev_to_dev_runs,
                (long) ctx->copy_dev_to_dev_total_runtime /
                (ctx->copy_dev_to_dev_runs !=
                 0 ? ctx->copy_dev_to_dev_runs : 1),
                (long) ctx->copy_dev_to_dev_total_runtime);
        ctx->total_runtime += ctx->copy_dev_to_dev_total_runtime;
        ctx->total_runs += ctx->copy_dev_to_dev_runs;
        fprintf(stderr,
                "copy_dev_to_host     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->copy_dev_to_host_runs,
                (long) ctx->copy_dev_to_host_total_runtime /
                (ctx->copy_dev_to_host_runs !=
                 0 ? ctx->copy_dev_to_host_runs : 1),
                (long) ctx->copy_dev_to_host_total_runtime);
        ctx->total_runtime += ctx->copy_dev_to_host_total_runtime;
        ctx->total_runs += ctx->copy_dev_to_host_runs;
        fprintf(stderr,
                "copy_host_to_dev     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->copy_host_to_dev_runs,
                (long) ctx->copy_host_to_dev_total_runtime /
                (ctx->copy_host_to_dev_runs !=
                 0 ? ctx->copy_host_to_dev_runs : 1),
                (long) ctx->copy_host_to_dev_total_runtime);
        ctx->total_runtime += ctx->copy_host_to_dev_total_runtime;
        ctx->total_runs += ctx->copy_host_to_dev_runs;
        fprintf(stderr,
                "copy_scalar_to_dev   ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->copy_scalar_to_dev_runs,
                (long) ctx->copy_scalar_to_dev_total_runtime /
                (ctx->copy_scalar_to_dev_runs !=
                 0 ? ctx->copy_scalar_to_dev_runs : 1),
                (long) ctx->copy_scalar_to_dev_total_runtime);
        ctx->total_runtime += ctx->copy_scalar_to_dev_total_runtime;
        ctx->total_runs += ctx->copy_scalar_to_dev_runs;
        fprintf(stderr,
                "copy_scalar_from_dev ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->copy_scalar_from_dev_runs,
                (long) ctx->copy_scalar_from_dev_total_runtime /
                (ctx->copy_scalar_from_dev_runs !=
                 0 ? ctx->copy_scalar_from_dev_runs : 1),
                (long) ctx->copy_scalar_from_dev_total_runtime);
        ctx->total_runtime += ctx->copy_scalar_from_dev_total_runtime;
        ctx->total_runs += ctx->copy_scalar_from_dev_runs;
        fprintf(stderr,
                "scan_stage1_5696     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage1_5696_runs,
                (long) ctx->scan_stage1_5696_total_runtime /
                (ctx->scan_stage1_5696_runs !=
                 0 ? ctx->scan_stage1_5696_runs : 1),
                (long) ctx->scan_stage1_5696_total_runtime);
        ctx->total_runtime += ctx->scan_stage1_5696_total_runtime;
        ctx->total_runs += ctx->scan_stage1_5696_runs;
        fprintf(stderr,
                "scan_stage1_5705     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage1_5705_runs,
                (long) ctx->scan_stage1_5705_total_runtime /
                (ctx->scan_stage1_5705_runs !=
                 0 ? ctx->scan_stage1_5705_runs : 1),
                (long) ctx->scan_stage1_5705_total_runtime);
        ctx->total_runtime += ctx->scan_stage1_5705_total_runtime;
        ctx->total_runs += ctx->scan_stage1_5705_runs;
        fprintf(stderr,
                "scan_stage2_5696     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage2_5696_runs,
                (long) ctx->scan_stage2_5696_total_runtime /
                (ctx->scan_stage2_5696_runs !=
                 0 ? ctx->scan_stage2_5696_runs : 1),
                (long) ctx->scan_stage2_5696_total_runtime);
        ctx->total_runtime += ctx->scan_stage2_5696_total_runtime;
        ctx->total_runs += ctx->scan_stage2_5696_runs;
        fprintf(stderr,
                "scan_stage2_5705     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage2_5705_runs,
                (long) ctx->scan_stage2_5705_total_runtime /
                (ctx->scan_stage2_5705_runs !=
                 0 ? ctx->scan_stage2_5705_runs : 1),
                (long) ctx->scan_stage2_5705_total_runtime);
        ctx->total_runtime += ctx->scan_stage2_5705_total_runtime;
        ctx->total_runs += ctx->scan_stage2_5705_runs;
        fprintf(stderr,
                "scan_stage3_5696     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage3_5696_runs,
                (long) ctx->scan_stage3_5696_total_runtime /
                (ctx->scan_stage3_5696_runs !=
                 0 ? ctx->scan_stage3_5696_runs : 1),
                (long) ctx->scan_stage3_5696_total_runtime);
        ctx->total_runtime += ctx->scan_stage3_5696_total_runtime;
        ctx->total_runs += ctx->scan_stage3_5696_runs;
        fprintf(stderr,
                "scan_stage3_5705     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage3_5705_runs,
                (long) ctx->scan_stage3_5705_total_runtime /
                (ctx->scan_stage3_5705_runs !=
                 0 ? ctx->scan_stage3_5705_runs : 1),
                (long) ctx->scan_stage3_5705_total_runtime);
        ctx->total_runtime += ctx->scan_stage3_5705_total_runtime;
        ctx->total_runs += ctx->scan_stage3_5705_runs;
        fprintf(stderr,
                "segmap_5728          ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->segmap_5728_runs, (long) ctx->segmap_5728_total_runtime /
                (ctx->segmap_5728_runs != 0 ? ctx->segmap_5728_runs : 1),
                (long) ctx->segmap_5728_total_runtime);
        ctx->total_runtime += ctx->segmap_5728_total_runtime;
        ctx->total_runs += ctx->segmap_5728_runs;
        if (ctx->profiling)
            fprintf(stderr, "%d operations with cumulative runtime: %6ldus\n",
                    ctx->total_runs, ctx->total_runtime);
    }
}
static int futrts_main(struct futhark_context *ctx,
                       struct memblock_device *out_mem_p_5972,
                       int32_t *out_out_arrsizze_5973,
                       struct memblock_device k_mem_5742,
                       struct memblock_device d_mem_5743, int32_t n_5540,
                       int32_t n_5541)
{
    struct memblock_device out_mem_5772;
    
    out_mem_5772.references = NULL;
    
    int32_t out_arrsizze_5773;
    bool dim_match_5544 = n_5540 == n_5541;
    bool empty_or_match_cert_5545;
    
    if (!dim_match_5544) {
        ctx->error = msgprintf("Error: %s\n\nBacktrace:\n%s",
                               "function arguments of wrong shape",
                               "-> #0  segreduce.fut:31:1-62\n");
        if (memblock_unref_device(ctx, &out_mem_5772, "out_mem_5772") != 0)
            return 1;
        return 1;
    }
    
    int64_t chunk_5688 = sext_i32_i64(n_5540);
    int32_t segscan_group_sizze_5691;
    
    segscan_group_sizze_5691 = ctx->sizes.mainzisegscan_group_sizze_5690;
    
    int32_t num_groups_5693;
    int32_t max_num_groups_5774;
    
    max_num_groups_5774 = ctx->sizes.mainzisegscan_num_groups_5692;
    num_groups_5693 = sext_i64_i32(smax64(1,
                                          smin64(squot64(sub64(add64(chunk_5688,
                                                                     sext_i32_i64(segscan_group_sizze_5691)),
                                                               1),
                                                         sext_i32_i64(segscan_group_sizze_5691)),
                                                 sext_i32_i64(max_num_groups_5774))));
    
    int64_t bytes_5745 = mul64(4, chunk_5688);
    struct memblock_device mem_5747;
    
    mem_5747.references = NULL;
    if (memblock_alloc_device(ctx, &mem_5747, bytes_5745, "mem_5747"))
        return 1;
    
    struct memblock_device mem_5749;
    
    mem_5749.references = NULL;
    if (memblock_alloc_device(ctx, &mem_5749, chunk_5688, "mem_5749"))
        return 1;
    if (ctx->debugging)
        fprintf(stderr, "%s\n", "\n# SegScan");
    
    int32_t num_threads_5775 = mul32(num_groups_5693, segscan_group_sizze_5691);
    
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5696, 1,
                                            mul32((int32_t) sizeof(bool),
                                                  segscan_group_sizze_5691),
                                            NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5696, 2,
                                            mul32((int32_t) sizeof(int32_t),
                                                  segscan_group_sizze_5691),
                                            NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5696, 3,
                                            sizeof(n_5540), &n_5540));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5696, 4,
                                            sizeof(k_mem_5742.mem),
                                            &k_mem_5742.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5696, 5,
                                            sizeof(d_mem_5743.mem),
                                            &d_mem_5743.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5696, 6,
                                            sizeof(mem_5747.mem),
                                            &mem_5747.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5696, 7,
                                            sizeof(mem_5749.mem),
                                            &mem_5749.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5696, 8,
                                            sizeof(num_threads_5775),
                                            &num_threads_5775));
    if (1 * (num_groups_5693 * segscan_group_sizze_5691) != 0) {
        const size_t global_work_sizze_5974[1] = {num_groups_5693 *
                     segscan_group_sizze_5691};
        const size_t local_work_sizze_5978[1] = {segscan_group_sizze_5691};
        int64_t time_start_5975 = 0, time_end_5976 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage1_5696");
            fprintf(stderr, "%zu", global_work_sizze_5974[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_5978[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) (0 + mul32((int32_t) sizeof(bool),
                                     segscan_group_sizze_5691) +
                           mul32((int32_t) sizeof(int32_t),
                                 segscan_group_sizze_5691)));
            time_start_5975 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage1_5696,
                                                        1, NULL,
                                                        global_work_sizze_5974,
                                                        local_work_sizze_5978,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage1_5696_runs,
                                                                                                  &ctx->scan_stage1_5696_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_5976 = get_wall_time();
            
            long time_diff_5977 = time_end_5976 - time_start_5975;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage1_5696",
                    time_diff_5977);
        }
    }
    if (ctx->debugging)
        fprintf(stderr, "%s: %llu%c", "elems_per_group",
                (long long) mul32(segscan_group_sizze_5691,
                                  squot32(sub32(add32(n_5540, num_threads_5775),
                                                1), num_threads_5775)), '\n');
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5696, 1,
                                            mul32((int32_t) sizeof(bool),
                                                  num_groups_5693), NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5696, 2,
                                            mul32((int32_t) sizeof(int32_t),
                                                  num_groups_5693), NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5696, 3,
                                            sizeof(n_5540), &n_5540));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5696, 4,
                                            sizeof(num_groups_5693),
                                            &num_groups_5693));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5696, 5,
                                            sizeof(mem_5747.mem),
                                            &mem_5747.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5696, 6,
                                            sizeof(mem_5749.mem),
                                            &mem_5749.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5696, 7,
                                            sizeof(num_threads_5775),
                                            &num_threads_5775));
    if (1 * (1 * num_groups_5693) != 0) {
        const size_t global_work_sizze_5979[1] = {1 * num_groups_5693};
        const size_t local_work_sizze_5983[1] = {num_groups_5693};
        int64_t time_start_5980 = 0, time_end_5981 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage2_5696");
            fprintf(stderr, "%zu", global_work_sizze_5979[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_5983[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) (0 + mul32((int32_t) sizeof(bool), num_groups_5693) +
                           mul32((int32_t) sizeof(int32_t), num_groups_5693)));
            time_start_5980 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage2_5696,
                                                        1, NULL,
                                                        global_work_sizze_5979,
                                                        local_work_sizze_5983,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage2_5696_runs,
                                                                                                  &ctx->scan_stage2_5696_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_5981 = get_wall_time();
            
            long time_diff_5982 = time_end_5981 - time_start_5980;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage2_5696",
                    time_diff_5982);
        }
    }
    
    int32_t required_groups_5849 = squot32(sub32(add32(n_5540,
                                                       segscan_group_sizze_5691),
                                                 1), segscan_group_sizze_5691);
    
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5696, 1,
                                            sizeof(n_5540), &n_5540));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5696, 2,
                                            sizeof(num_groups_5693),
                                            &num_groups_5693));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5696, 3,
                                            sizeof(mem_5747.mem),
                                            &mem_5747.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5696, 4,
                                            sizeof(mem_5749.mem),
                                            &mem_5749.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5696, 5,
                                            sizeof(num_threads_5775),
                                            &num_threads_5775));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5696, 6,
                                            sizeof(required_groups_5849),
                                            &required_groups_5849));
    if (1 * (num_groups_5693 * segscan_group_sizze_5691) != 0) {
        const size_t global_work_sizze_5984[1] = {num_groups_5693 *
                     segscan_group_sizze_5691};
        const size_t local_work_sizze_5988[1] = {segscan_group_sizze_5691};
        int64_t time_start_5985 = 0, time_end_5986 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage3_5696");
            fprintf(stderr, "%zu", global_work_sizze_5984[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_5988[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) 0);
            time_start_5985 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage3_5696,
                                                        1, NULL,
                                                        global_work_sizze_5984,
                                                        local_work_sizze_5988,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage3_5696_runs,
                                                                                                  &ctx->scan_stage3_5696_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_5986 = get_wall_time();
            
            long time_diff_5987 = time_end_5986 - time_start_5985;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage3_5696",
                    time_diff_5987);
        }
    }
    if (memblock_unref_device(ctx, &mem_5749, "mem_5749") != 0)
        return 1;
    
    int32_t segscan_group_sizze_5700;
    
    segscan_group_sizze_5700 = ctx->sizes.mainzisegscan_group_sizze_5699;
    
    int32_t num_groups_5702;
    int32_t max_num_groups_5861;
    
    max_num_groups_5861 = ctx->sizes.mainzisegscan_num_groups_5701;
    num_groups_5702 = sext_i64_i32(smax64(1,
                                          smin64(squot64(sub64(add64(chunk_5688,
                                                                     sext_i32_i64(segscan_group_sizze_5700)),
                                                               1),
                                                         sext_i32_i64(segscan_group_sizze_5700)),
                                                 sext_i32_i64(max_num_groups_5861))));
    
    struct memblock_device mem_5753;
    
    mem_5753.references = NULL;
    if (memblock_alloc_device(ctx, &mem_5753, bytes_5745, "mem_5753"))
        return 1;
    
    struct memblock_device mem_5756;
    
    mem_5756.references = NULL;
    if (memblock_alloc_device(ctx, &mem_5756, bytes_5745, "mem_5756"))
        return 1;
    
    struct memblock_device mem_5758;
    
    mem_5758.references = NULL;
    if (memblock_alloc_device(ctx, &mem_5758, chunk_5688, "mem_5758"))
        return 1;
    
    struct memblock_device mem_5761;
    
    mem_5761.references = NULL;
    if (memblock_alloc_device(ctx, &mem_5761, bytes_5745, "mem_5761"))
        return 1;
    if (ctx->debugging)
        fprintf(stderr, "%s\n", "\n# SegScan");
    
    int32_t num_threads_5862 = mul32(num_groups_5702, segscan_group_sizze_5700);
    
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 1,
                                            mul32((int32_t) sizeof(bool),
                                                  segscan_group_sizze_5700),
                                            NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 2,
                                            mul32((int32_t) sizeof(int32_t),
                                                  segscan_group_sizze_5700),
                                            NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 3,
                                            mul32((int32_t) sizeof(int32_t),
                                                  segscan_group_sizze_5700),
                                            NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 4,
                                            sizeof(n_5540), &n_5540));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 5,
                                            sizeof(k_mem_5742.mem),
                                            &k_mem_5742.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 6,
                                            sizeof(d_mem_5743.mem),
                                            &d_mem_5743.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 7,
                                            sizeof(mem_5747.mem),
                                            &mem_5747.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 8,
                                            sizeof(mem_5753.mem),
                                            &mem_5753.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 9,
                                            sizeof(mem_5756.mem),
                                            &mem_5756.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 10,
                                            sizeof(mem_5758.mem),
                                            &mem_5758.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 11,
                                            sizeof(mem_5761.mem),
                                            &mem_5761.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_5705, 12,
                                            sizeof(num_threads_5862),
                                            &num_threads_5862));
    if (1 * (num_groups_5702 * segscan_group_sizze_5700) != 0) {
        const size_t global_work_sizze_5989[1] = {num_groups_5702 *
                     segscan_group_sizze_5700};
        const size_t local_work_sizze_5993[1] = {segscan_group_sizze_5700};
        int64_t time_start_5990 = 0, time_end_5991 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage1_5705");
            fprintf(stderr, "%zu", global_work_sizze_5989[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_5993[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) (0 + mul32((int32_t) sizeof(bool),
                                     segscan_group_sizze_5700) +
                           mul32((int32_t) sizeof(int32_t),
                                 segscan_group_sizze_5700) +
                           mul32((int32_t) sizeof(int32_t),
                                 segscan_group_sizze_5700)));
            time_start_5990 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage1_5705,
                                                        1, NULL,
                                                        global_work_sizze_5989,
                                                        local_work_sizze_5993,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage1_5705_runs,
                                                                                                  &ctx->scan_stage1_5705_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_5991 = get_wall_time();
            
            long time_diff_5992 = time_end_5991 - time_start_5990;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage1_5705",
                    time_diff_5992);
        }
    }
    if (ctx->debugging)
        fprintf(stderr, "%s: %llu%c", "elems_per_group",
                (long long) mul32(segscan_group_sizze_5700,
                                  squot32(sub32(add32(n_5540, num_threads_5862),
                                                1), num_threads_5862)), '\n');
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5705, 1,
                                            mul32((int32_t) sizeof(bool),
                                                  num_groups_5702), NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5705, 2,
                                            mul32((int32_t) sizeof(int32_t),
                                                  num_groups_5702), NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5705, 3,
                                            mul32((int32_t) sizeof(int32_t),
                                                  num_groups_5702), NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5705, 4,
                                            sizeof(n_5540), &n_5540));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5705, 5,
                                            sizeof(num_groups_5702),
                                            &num_groups_5702));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5705, 6,
                                            sizeof(mem_5753.mem),
                                            &mem_5753.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5705, 7,
                                            sizeof(mem_5756.mem),
                                            &mem_5756.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5705, 8,
                                            sizeof(mem_5758.mem),
                                            &mem_5758.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_5705, 9,
                                            sizeof(num_threads_5862),
                                            &num_threads_5862));
    if (1 * (1 * num_groups_5702) != 0) {
        const size_t global_work_sizze_5994[1] = {1 * num_groups_5702};
        const size_t local_work_sizze_5998[1] = {num_groups_5702};
        int64_t time_start_5995 = 0, time_end_5996 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage2_5705");
            fprintf(stderr, "%zu", global_work_sizze_5994[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_5998[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) (0 + mul32((int32_t) sizeof(bool), num_groups_5702) +
                           mul32((int32_t) sizeof(int32_t), num_groups_5702) +
                           mul32((int32_t) sizeof(int32_t), num_groups_5702)));
            time_start_5995 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage2_5705,
                                                        1, NULL,
                                                        global_work_sizze_5994,
                                                        local_work_sizze_5998,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage2_5705_runs,
                                                                                                  &ctx->scan_stage2_5705_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_5996 = get_wall_time();
            
            long time_diff_5997 = time_end_5996 - time_start_5995;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage2_5705",
                    time_diff_5997);
        }
    }
    
    int32_t required_groups_5955 = squot32(sub32(add32(n_5540,
                                                       segscan_group_sizze_5700),
                                                 1), segscan_group_sizze_5700);
    
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5705, 1,
                                            sizeof(n_5540), &n_5540));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5705, 2,
                                            sizeof(num_groups_5702),
                                            &num_groups_5702));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5705, 3,
                                            sizeof(mem_5753.mem),
                                            &mem_5753.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5705, 4,
                                            sizeof(mem_5756.mem),
                                            &mem_5756.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5705, 5,
                                            sizeof(mem_5758.mem),
                                            &mem_5758.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5705, 6,
                                            sizeof(num_threads_5862),
                                            &num_threads_5862));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_5705, 7,
                                            sizeof(required_groups_5955),
                                            &required_groups_5955));
    if (1 * (num_groups_5702 * segscan_group_sizze_5700) != 0) {
        const size_t global_work_sizze_5999[1] = {num_groups_5702 *
                     segscan_group_sizze_5700};
        const size_t local_work_sizze_6003[1] = {segscan_group_sizze_5700};
        int64_t time_start_6000 = 0, time_end_6001 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage3_5705");
            fprintf(stderr, "%zu", global_work_sizze_5999[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_6003[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) 0);
            time_start_6000 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage3_5705,
                                                        1, NULL,
                                                        global_work_sizze_5999,
                                                        local_work_sizze_6003,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage3_5705_runs,
                                                                                                  &ctx->scan_stage3_5705_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_6001 = get_wall_time();
            
            long time_diff_6002 = time_end_6001 - time_start_6000;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage3_5705",
                    time_diff_6002);
        }
    }
    if (memblock_unref_device(ctx, &mem_5747, "mem_5747") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_5758, "mem_5758") != 0)
        return 1;
    
    int32_t segmap_group_sizze_5732;
    
    segmap_group_sizze_5732 = ctx->sizes.mainzisegmap_group_sizze_5731;
    
    int64_t segmap_group_sizze_5733 = sext_i32_i64(segmap_group_sizze_5732);
    int64_t y_5734 = sub64(segmap_group_sizze_5733, 1);
    int64_t x_5735 = add64(chunk_5688, y_5734);
    int64_t segmap_usable_groups_64_5737 = squot64(x_5735,
                                                   segmap_group_sizze_5733);
    int32_t segmap_usable_groups_5738 =
            sext_i64_i32(segmap_usable_groups_64_5737);
    
    if (ctx->debugging)
        fprintf(stderr, "%s\n", "\n# SegMap");
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_5728, 1, sizeof(n_5540),
                                            &n_5540));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_5728, 2,
                                            sizeof(mem_5753.mem),
                                            &mem_5753.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_5728, 3,
                                            sizeof(mem_5756.mem),
                                            &mem_5756.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_5728, 4,
                                            sizeof(mem_5761.mem),
                                            &mem_5761.mem));
    if (1 * (segmap_usable_groups_5738 * segmap_group_sizze_5732) != 0) {
        const size_t global_work_sizze_6004[1] = {segmap_usable_groups_5738 *
                     segmap_group_sizze_5732};
        const size_t local_work_sizze_6008[1] = {segmap_group_sizze_5732};
        int64_t time_start_6005 = 0, time_end_6006 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "segmap_5728");
            fprintf(stderr, "%zu", global_work_sizze_6004[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_6008[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) 0);
            time_start_6005 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->segmap_5728, 1,
                                                        NULL,
                                                        global_work_sizze_6004,
                                                        local_work_sizze_6008,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->segmap_5728_runs,
                                                                                                  &ctx->segmap_5728_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_6006 = get_wall_time();
            
            long time_diff_6007 = time_end_6006 - time_start_6005;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "segmap_5728",
                    time_diff_6007);
        }
    }
    if (memblock_unref_device(ctx, &mem_5756, "mem_5756") != 0)
        return 1;
    
    int32_t i_5672 = sub32(n_5540, 1);
    bool x_5673 = sle32(0, i_5672);
    bool index_certs_5676;
    
    if (!x_5673) {
        ctx->error = msgprintf("Error: %s%d%s%d%s\n\nBacktrace:\n%s", "Index [",
                               i_5672, "] out of bounds for array of shape [",
                               n_5540, "].",
                               "-> #0  segreduce.fut:28:44-70\n   #1  segreduce.fut:31:38-62\n   #2  segreduce.fut:31:1-62\n");
        if (memblock_unref_device(ctx, &mem_5761, "mem_5761") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_5758, "mem_5758") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_5756, "mem_5756") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_5753, "mem_5753") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_5749, "mem_5749") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_5747, "mem_5747") != 0)
            return 1;
        if (memblock_unref_device(ctx, &out_mem_5772, "out_mem_5772") != 0)
            return 1;
        return 1;
    }
    
    int32_t read_res_6009;
    
    OPENCL_SUCCEED_OR_RETURN(clEnqueueReadBuffer(ctx->opencl.queue,
                                                 mem_5753.mem,
                                                 ctx->failure_is_an_option ? CL_FALSE : CL_TRUE,
                                                 i_5672 * sizeof(int32_t),
                                                 sizeof(int32_t),
                                                 &read_res_6009, 0, NULL,
                                                 ctx->profiling_paused ||
                                                 !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                           &ctx->copy_scalar_from_dev_runs,
                                                                                           &ctx->copy_scalar_from_dev_total_runtime)));
    if (ctx->failure_is_an_option && futhark_context_sync(ctx) != 0)
        return 1;
    
    int32_t x_5677 = read_res_6009;
    
    if (memblock_unref_device(ctx, &mem_5753, "mem_5753") != 0)
        return 1;
    
    int32_t j_5678 = add32(1, x_5677);
    bool empty_slice_5679 = j_5678 == 0;
    bool zzero_leq_i_p_m_t_s_5680 = sle32(0, x_5677);
    bool i_p_m_t_s_leq_w_5681 = slt32(x_5677, n_5540);
    bool i_lte_j_5682 = sle32(0, j_5678);
    bool y_5683 = zzero_leq_i_p_m_t_s_5680 && i_p_m_t_s_leq_w_5681;
    bool y_5684 = i_lte_j_5682 && y_5683;
    bool ok_or_empty_5685 = empty_slice_5679 || y_5684;
    bool index_certs_5686;
    
    if (!ok_or_empty_5685) {
        ctx->error = msgprintf("Error: %s%d%s%d%s%d%s\n\nBacktrace:\n%s",
                               "Index [", 0, ":", j_5678,
                               "] out of bounds for array of shape [", n_5540,
                               "].",
                               "-> #0  segreduce.fut:28:8-74\n   #1  segreduce.fut:31:38-62\n   #2  segreduce.fut:31:1-62\n");
        if (memblock_unref_device(ctx, &mem_5761, "mem_5761") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_5758, "mem_5758") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_5756, "mem_5756") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_5753, "mem_5753") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_5749, "mem_5749") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_5747, "mem_5747") != 0)
            return 1;
        if (memblock_unref_device(ctx, &out_mem_5772, "out_mem_5772") != 0)
            return 1;
        return 1;
    }
    
    int64_t binop_x_5763 = sext_i32_i64(j_5678);
    int64_t bytes_5762 = mul64(4, binop_x_5763);
    struct memblock_device mem_5764;
    
    mem_5764.references = NULL;
    if (memblock_alloc_device(ctx, &mem_5764, bytes_5762, "mem_5764"))
        return 1;
    if (mul64(sext_i32_i64(j_5678), (int32_t) sizeof(int32_t)) > 0) {
        OPENCL_SUCCEED_OR_RETURN(clEnqueueCopyBuffer(ctx->opencl.queue,
                                                     mem_5761.mem, mem_5764.mem,
                                                     0, 0,
                                                     mul64(sext_i32_i64(j_5678),
                                                           (int32_t) sizeof(int32_t)),
                                                     0, NULL,
                                                     ctx->profiling_paused ||
                                                     !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                               &ctx->copy_dev_to_dev_runs,
                                                                                               &ctx->copy_dev_to_dev_total_runtime)));
        if (ctx->debugging)
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
    }
    if (memblock_unref_device(ctx, &mem_5761, "mem_5761") != 0)
        return 1;
    out_arrsizze_5773 = j_5678;
    if (memblock_set_device(ctx, &out_mem_5772, &mem_5764, "mem_5764") != 0)
        return 1;
    (*out_mem_p_5972).references = NULL;
    if (memblock_set_device(ctx, &*out_mem_p_5972, &out_mem_5772,
                            "out_mem_5772") != 0)
        return 1;
    *out_out_arrsizze_5973 = out_arrsizze_5773;
    if (memblock_unref_device(ctx, &mem_5764, "mem_5764") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_5761, "mem_5761") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_5758, "mem_5758") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_5756, "mem_5756") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_5753, "mem_5753") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_5749, "mem_5749") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_5747, "mem_5747") != 0)
        return 1;
    if (memblock_unref_device(ctx, &out_mem_5772, "out_mem_5772") != 0)
        return 1;
    return 0;
}
struct futhark_bool_1d {
    struct memblock_device mem;
    int64_t shape[1];
} ;
struct futhark_bool_1d *futhark_new_bool_1d(struct futhark_context *ctx,
                                            bool *data, int64_t dim0)
{
    struct futhark_bool_1d *bad = NULL;
    struct futhark_bool_1d *arr =
                           (struct futhark_bool_1d *) malloc(sizeof(struct futhark_bool_1d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc_device(ctx, &arr->mem, (size_t) dim0 * sizeof(bool),
                              "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    if ((size_t) dim0 * sizeof(bool) > 0)
        OPENCL_SUCCEED_OR_RETURN(clEnqueueWriteBuffer(ctx->opencl.queue,
                                                      arr->mem.mem, CL_TRUE, 0,
                                                      (size_t) dim0 *
                                                      sizeof(bool), data + 0, 0,
                                                      NULL,
                                                      ctx->profiling_paused ||
                                                      !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                &ctx->copy_dev_to_host_runs,
                                                                                                &ctx->copy_dev_to_host_total_runtime)));
    lock_unlock(&ctx->lock);
    return arr;
}
struct futhark_bool_1d *futhark_new_raw_bool_1d(struct futhark_context *ctx,
                                                cl_mem data, int offset,
                                                int64_t dim0)
{
    struct futhark_bool_1d *bad = NULL;
    struct futhark_bool_1d *arr =
                           (struct futhark_bool_1d *) malloc(sizeof(struct futhark_bool_1d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc_device(ctx, &arr->mem, (size_t) dim0 * sizeof(bool),
                              "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    if ((size_t) dim0 * sizeof(bool) > 0) {
        OPENCL_SUCCEED_OR_RETURN(clEnqueueCopyBuffer(ctx->opencl.queue, data,
                                                     arr->mem.mem, offset, 0,
                                                     (size_t) dim0 *
                                                     sizeof(bool), 0, NULL,
                                                     ctx->profiling_paused ||
                                                     !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                               &ctx->copy_dev_to_dev_runs,
                                                                                               &ctx->copy_dev_to_dev_total_runtime)));
        if (ctx->debugging)
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
    }
    lock_unlock(&ctx->lock);
    return arr;
}
int futhark_free_bool_1d(struct futhark_context *ctx,
                         struct futhark_bool_1d *arr)
{
    lock_lock(&ctx->lock);
    if (memblock_unref_device(ctx, &arr->mem, "arr->mem") != 0)
        return 1;
    lock_unlock(&ctx->lock);
    free(arr);
    return 0;
}
int futhark_values_bool_1d(struct futhark_context *ctx,
                           struct futhark_bool_1d *arr, bool *data)
{
    lock_lock(&ctx->lock);
    if ((size_t) arr->shape[0] * sizeof(bool) > 0) {
        OPENCL_SUCCEED_OR_RETURN(clEnqueueReadBuffer(ctx->opencl.queue,
                                                     arr->mem.mem,
                                                     ctx->failure_is_an_option ? CL_FALSE : CL_TRUE,
                                                     0, (size_t) arr->shape[0] *
                                                     sizeof(bool), data + 0, 0,
                                                     NULL,
                                                     ctx->profiling_paused ||
                                                     !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                               &ctx->copy_host_to_dev_runs,
                                                                                               &ctx->copy_host_to_dev_total_runtime)));
        if (ctx->failure_is_an_option && futhark_context_sync(ctx) != 0)
            return 1;
    }
    lock_unlock(&ctx->lock);
    return 0;
}
cl_mem futhark_values_raw_bool_1d(struct futhark_context *ctx,
                                  struct futhark_bool_1d *arr)
{
    (void) ctx;
    return arr->mem.mem;
}
int64_t *futhark_shape_bool_1d(struct futhark_context *ctx,
                               struct futhark_bool_1d *arr)
{
    (void) ctx;
    return arr->shape;
}
struct futhark_i32_1d {
    struct memblock_device mem;
    int64_t shape[1];
} ;
struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx,
                                          int32_t *data, int64_t dim0)
{
    struct futhark_i32_1d *bad = NULL;
    struct futhark_i32_1d *arr =
                          (struct futhark_i32_1d *) malloc(sizeof(struct futhark_i32_1d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc_device(ctx, &arr->mem, (size_t) dim0 * sizeof(int32_t),
                              "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    if ((size_t) dim0 * sizeof(int32_t) > 0)
        OPENCL_SUCCEED_OR_RETURN(clEnqueueWriteBuffer(ctx->opencl.queue,
                                                      arr->mem.mem, CL_TRUE, 0,
                                                      (size_t) dim0 *
                                                      sizeof(int32_t), data + 0,
                                                      0, NULL,
                                                      ctx->profiling_paused ||
                                                      !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                &ctx->copy_dev_to_host_runs,
                                                                                                &ctx->copy_dev_to_host_total_runtime)));
    lock_unlock(&ctx->lock);
    return arr;
}
struct futhark_i32_1d *futhark_new_raw_i32_1d(struct futhark_context *ctx,
                                              cl_mem data, int offset,
                                              int64_t dim0)
{
    struct futhark_i32_1d *bad = NULL;
    struct futhark_i32_1d *arr =
                          (struct futhark_i32_1d *) malloc(sizeof(struct futhark_i32_1d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc_device(ctx, &arr->mem, (size_t) dim0 * sizeof(int32_t),
                              "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    if ((size_t) dim0 * sizeof(int32_t) > 0) {
        OPENCL_SUCCEED_OR_RETURN(clEnqueueCopyBuffer(ctx->opencl.queue, data,
                                                     arr->mem.mem, offset, 0,
                                                     (size_t) dim0 *
                                                     sizeof(int32_t), 0, NULL,
                                                     ctx->profiling_paused ||
                                                     !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                               &ctx->copy_dev_to_dev_runs,
                                                                                               &ctx->copy_dev_to_dev_total_runtime)));
        if (ctx->debugging)
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
    }
    lock_unlock(&ctx->lock);
    return arr;
}
int futhark_free_i32_1d(struct futhark_context *ctx, struct futhark_i32_1d *arr)
{
    lock_lock(&ctx->lock);
    if (memblock_unref_device(ctx, &arr->mem, "arr->mem") != 0)
        return 1;
    lock_unlock(&ctx->lock);
    free(arr);
    return 0;
}
int futhark_values_i32_1d(struct futhark_context *ctx,
                          struct futhark_i32_1d *arr, int32_t *data)
{
    lock_lock(&ctx->lock);
    if ((size_t) arr->shape[0] * sizeof(int32_t) > 0) {
        OPENCL_SUCCEED_OR_RETURN(clEnqueueReadBuffer(ctx->opencl.queue,
                                                     arr->mem.mem,
                                                     ctx->failure_is_an_option ? CL_FALSE : CL_TRUE,
                                                     0, (size_t) arr->shape[0] *
                                                     sizeof(int32_t), data + 0,
                                                     0, NULL,
                                                     ctx->profiling_paused ||
                                                     !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                               &ctx->copy_host_to_dev_runs,
                                                                                               &ctx->copy_host_to_dev_total_runtime)));
        if (ctx->failure_is_an_option && futhark_context_sync(ctx) != 0)
            return 1;
    }
    lock_unlock(&ctx->lock);
    return 0;
}
cl_mem futhark_values_raw_i32_1d(struct futhark_context *ctx,
                                 struct futhark_i32_1d *arr)
{
    (void) ctx;
    return arr->mem.mem;
}
int64_t *futhark_shape_i32_1d(struct futhark_context *ctx,
                              struct futhark_i32_1d *arr)
{
    (void) ctx;
    return arr->shape;
}
int futhark_entry_main(struct futhark_context *ctx,
                       struct futhark_i32_1d **out0, const
                       struct futhark_i32_1d *in0, const
                       struct futhark_bool_1d *in1)
{
    struct memblock_device k_mem_5742;
    
    k_mem_5742.references = NULL;
    
    struct memblock_device d_mem_5743;
    
    d_mem_5743.references = NULL;
    
    int32_t n_5540;
    int32_t n_5541;
    struct memblock_device out_mem_5772;
    
    out_mem_5772.references = NULL;
    
    int32_t out_arrsizze_5773;
    
    lock_lock(&ctx->lock);
    k_mem_5742 = in0->mem;
    n_5540 = in0->shape[0];
    d_mem_5743 = in1->mem;
    n_5541 = in1->shape[0];
    
    int ret = futrts_main(ctx, &out_mem_5772, &out_arrsizze_5773, k_mem_5742,
                          d_mem_5743, n_5540, n_5541);
    
    if (ret == 0) {
        assert((*out0 =
                (struct futhark_i32_1d *) malloc(sizeof(struct futhark_i32_1d))) !=
            NULL);
        (*out0)->mem = out_mem_5772;
        (*out0)->shape[0] = out_arrsizze_5773;
    }
    lock_unlock(&ctx->lock);
    return ret;
}