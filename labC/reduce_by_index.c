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
                       struct futhark_i32_1d *in1);

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
    
    struct futhark_i32_1d *read_value_9722;
    int64_t read_shape_9723[1];
    int32_t *read_arr_9724 = NULL;
    
    errno = 0;
    if (read_array(&i32_info, (void **) &read_arr_9724, read_shape_9723, 1) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s%s (errno: %s).\n", 0,
                      "[]", i32_info.type_name, strerror(errno));
    
    struct futhark_i32_1d *read_value_9725;
    int64_t read_shape_9726[1];
    int32_t *read_arr_9727 = NULL;
    
    errno = 0;
    if (read_array(&i32_info, (void **) &read_arr_9727, read_shape_9726, 1) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s%s (errno: %s).\n", 1,
                      "[]", i32_info.type_name, strerror(errno));
    if (end_of_input() != 0)
        futhark_panic(1, "Expected EOF on stdin after reading input for %s.\n",
                      "\"main\"");
    
    struct futhark_i32_1d *result_9728;
    
    if (perform_warmup) {
        int r;
        
        assert((read_value_9722 = futhark_new_i32_1d(ctx, read_arr_9724,
                                                     read_shape_9723[0])) != 0);
        assert((read_value_9725 = futhark_new_i32_1d(ctx, read_arr_9727,
                                                     read_shape_9726[0])) != 0);
        if (futhark_context_sync(ctx) != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        ;
        // Only profile last run.
        if (profile_run)
            futhark_context_unpause_profiling(ctx);
        t_start = get_wall_time();
        r = futhark_entry_main(ctx, &result_9728, read_value_9722,
                               read_value_9725);
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
        assert(futhark_free_i32_1d(ctx, read_value_9722) == 0);
        assert(futhark_free_i32_1d(ctx, read_value_9725) == 0);
        assert(futhark_free_i32_1d(ctx, result_9728) == 0);
    }
    time_runs = 1;
    // Proper run.
    for (int run = 0; run < num_runs; run++) {
        // Only profile last run.
        profile_run = run == num_runs - 1;
        
        int r;
        
        assert((read_value_9722 = futhark_new_i32_1d(ctx, read_arr_9724,
                                                     read_shape_9723[0])) != 0);
        assert((read_value_9725 = futhark_new_i32_1d(ctx, read_arr_9727,
                                                     read_shape_9726[0])) != 0);
        if (futhark_context_sync(ctx) != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        ;
        // Only profile last run.
        if (profile_run)
            futhark_context_unpause_profiling(ctx);
        t_start = get_wall_time();
        r = futhark_entry_main(ctx, &result_9728, read_value_9722,
                               read_value_9725);
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
        assert(futhark_free_i32_1d(ctx, read_value_9722) == 0);
        assert(futhark_free_i32_1d(ctx, read_value_9725) == 0);
        if (run < num_runs - 1) {
            assert(futhark_free_i32_1d(ctx, result_9728) == 0);
        }
    }
    free(read_arr_9724);
    free(read_arr_9727);
    if (binary_output)
        set_binary_mode(stdout);
    {
        int32_t *arr = calloc(sizeof(int32_t), futhark_shape_i32_1d(ctx,
                                                                    result_9728)[0]);
        
        assert(arr != NULL);
        assert(futhark_values_i32_1d(ctx, result_9728, arr) == 0);
        write_array(stdout, binary_output, &i32_info, arr,
                    futhark_shape_i32_1d(ctx, result_9728), 1);
        free(arr);
    }
    printf("\n");
    assert(futhark_free_i32_1d(ctx, result_9728) == 0);
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
static int32_t counter_mem_realtype_9669[10] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
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
                   ", int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicOr((int32_t*)p, x);\n#else\n  return atomic_or(p, x);\n#endif\n}\n\ninline int32_t atomic_xor_i32_global(volatile __global int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicXor((int32_t*)p, x);\n#else\n  return atomic_xor(p, x);\n#endif\n}\n\ninline int32_t atomic_xor_i32_local(volatile __local int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicXor((int32_t*)p, x);\n#else\n  return atomic_xor(p, x);\n#endif\n}\n\ninline int32_t atomic_xchg_i32_global(volatile __global int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicExch((int32_t*)p, x);\n#else\n  return atomic_xor(p, x);\n#endif\n}\n\ninline int32_t atomic_xchg_i32_local(volatile __local int32_t *p, int32_t x) {\n#ifdef FUTHARK_CUDA\n  return atomicExch((int32_t*)p, x);\n#else\n  return atomic_xor(p, x);\n#endif\n}\n\ninline int32_t atomic_cmpxchg_i32_global(volatile __global int32_t *p,\n                                         int32_t cmp, int32_t val) {\n#ifdef FUTHARK_CUDA\n  return atomicCAS((int32_t*)p, cmp, val);\n#else\n  return atomic_cmpxchg(p, cmp, val);\n#endif\n}\n\ninline int32_t atomic_cmpxchg_i32_local(volatile __local int32_t *p,\n                                         int32_t cmp, int32_t val) {\n#ifdef FUTHARK_CUDA\n  return atomicCAS((int32_t*)p, cmp, val);\n#else\n  return atomic_cmpxchg(p, cmp, val);\n#endif\n}\n\n// End of atomics.h\n\n__kernel void iota_9321(int32_t n_8851, __global unsigned char *mem_9227)\n{\n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    int32_t iota_gtid_9321;\n    int32_t iota_ltid_9322;\n    int32_t iota_gid_9323;\n    \n    iota_gtid_9321 = get_global_id(0);\n    iota_ltid_9322 = get_local_id(0);\n    iota_gid_9323 = get_group_id(0);\n    if (slt32(iota_gtid_9321, n_8851)) {\n        ((__global int32_t *) mem_9227)[iota_gtid_9321] =\n            sext_i32_i32(iota_gtid_9321);\n    }\n    \n  error_0:\n    return;\n}\n__kernel void scan_stage1_9114(__global int *global_failure, __local volatile\n                ",
                   "               int64_t *scan_arr_mem_9351_backing_aligned_0,\n                               __local volatile\n                               int64_t *scan_arr_mem_9349_backing_aligned_1,\n                               int32_t n_8851, __global unsigned char *mem_9232,\n                               __global unsigned char *mem_9236, __global\n                               unsigned char *mem_9239, __global\n                               unsigned char *mem_9242,\n                               int32_t num_threads_9337)\n{\n    #define segscan_group_sizze_9109 (mainzisegscan_group_sizze_9108)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict scan_arr_mem_9351_backing_1 =\n                          (__local volatile\n                           char *) scan_arr_mem_9351_backing_aligned_0;\n    __local volatile char *restrict scan_arr_mem_9349_backing_0 =\n                          (__local volatile\n                           char *) scan_arr_mem_9349_backing_aligned_1;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9344;\n    int32_t local_tid_9345;\n    int32_t group_sizze_9348;\n    int32_t wave_sizze_9347;\n    int32_t group_tid_9346;\n    \n    global_tid_9344 = get_global_id(0);\n    local_tid_9345 = get_local_id(0);\n    group_sizze_9348 = get_local_size(0);\n    wave_sizze_9347 = LOCKSTEP_WIDTH;\n    group_tid_9346 = get_group_id(0);\n    \n    int32_t phys_tid_9114 = global_tid_9344;\n    __local char *scan_arr_mem_9349;\n    \n    scan_arr_mem_9349 = (__local char *) scan_arr_mem_9349_backing_0;\n    \n    __local char *scan_arr_mem_9351;\n    \n    scan_arr_mem_9351 = (__local char *) scan_arr_mem_9351_backing_1;\n    \n    int32_t x_8880;\n    int32_t x_8881;\n    int32_t x_8882;\n    int32_t x_8883;\n    \n    x_8880 = 0;\n    x_8881 = 0;\n    for (int32_t j_9353 = 0; j_9353 < squot32(sub32(add32(n_8851,\n                                                          num_threads_9337), 1),\n ",
                   "                                             num_threads_9337); j_9353++) {\n        int32_t chunk_offset_9354 = add32(mul32(segscan_group_sizze_9109,\n                                                j_9353), mul32(group_tid_9346,\n                                                               mul32(segscan_group_sizze_9109,\n                                                                     squot32(sub32(add32(n_8851,\n                                                                                         num_threads_9337),\n                                                                                   1),\n                                                                             num_threads_9337))));\n        int32_t flat_idx_9355 = add32(chunk_offset_9354, local_tid_9345);\n        int32_t gtid_9113 = flat_idx_9355;\n        \n        // threads in bounds read input; others get neutral element\n        {\n            if (slt32(gtid_9113, n_8851)) {\n                int32_t x_8886 = ((__global int32_t *) mem_9232)[gtid_9113];\n                int32_t res_8887 = sub32(1, x_8886);\n                \n                // write to-scan values to parameters\n                {\n                    x_8882 = res_8887;\n                    x_8883 = x_8886;\n                }\n                // write mapped values results to global memory\n                {\n                    ((__global int32_t *) mem_9242)[gtid_9113] = res_8887;\n                }\n            } else {\n                x_8882 = 0;\n                x_8883 = 0;\n            }\n        }\n        // combine with carry and write to local memory\n        {\n            int32_t res_8884 = add32(x_8880, x_8882);\n            int32_t res_8885 = add32(x_8881, x_8883);\n            \n            ((__local int32_t *) scan_arr_mem_9349)[local_tid_9345] = res_8884;\n            ((__local int32_t *) scan_arr_mem_9351)[local_tid_9345] = res_8885;\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        \n        int32_t x_9338;\n        int32_t x_93",
                   "39;\n        int32_t x_9340;\n        int32_t x_9341;\n        int32_t x_9356;\n        int32_t x_9357;\n        int32_t x_9358;\n        int32_t x_9359;\n        int32_t skip_threads_9362;\n        \n        if (slt32(local_tid_9345, segscan_group_sizze_9109)) {\n            x_9340 = ((volatile __local\n                       int32_t *) scan_arr_mem_9349)[local_tid_9345];\n            x_9341 = ((volatile __local\n                       int32_t *) scan_arr_mem_9351)[local_tid_9345];\n            if (sub32(local_tid_9345, mul32(squot32(local_tid_9345, 32), 32)) ==\n                0) {\n                x_9338 = x_9340;\n                x_9339 = x_9341;\n            }\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            skip_threads_9362 = 1;\n            while (slt32(skip_threads_9362, 32)) {\n                if (sle32(skip_threads_9362, sub32(local_tid_9345,\n                                                   mul32(squot32(local_tid_9345,\n                                                                 32), 32))) &&\n                    slt32(local_tid_9345, segscan_group_sizze_9109)) {\n                    // read operands\n                    {\n                        x_9338 = ((volatile __local\n                                   int32_t *) scan_arr_mem_9349)[sub32(local_tid_9345,\n                                                                       skip_threads_9362)];\n                        x_9339 = ((volatile __local\n                                   int32_t *) scan_arr_mem_9351)[sub32(local_tid_9345,\n                                                                       skip_threads_9362)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_9342 = add32(x_9338, x_9340);\n                        int32_t res_9343 = add32(x_9339, x_9341);\n                        \n                        x_9338 = res_9342;\n                        x_9339 = res_9343;\n                    }\n            ",
                   "    }\n                if (sle32(wave_sizze_9347, skip_threads_9362)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if (sle32(skip_threads_9362, sub32(local_tid_9345,\n                                                   mul32(squot32(local_tid_9345,\n                                                                 32), 32))) &&\n                    slt32(local_tid_9345, segscan_group_sizze_9109)) {\n                    // write result\n                    {\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_9349)[local_tid_9345] =\n                            x_9338;\n                        x_9340 = x_9338;\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_9351)[local_tid_9345] =\n                            x_9339;\n                        x_9341 = x_9339;\n                    }\n                }\n                if (sle32(wave_sizze_9347, skip_threads_9362)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_9362 *= 2;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // last thread of block 'i' writes its result to offset 'i'\n        {\n            if (sub32(local_tid_9345, mul32(squot32(local_tid_9345, 32), 32)) ==\n                31 && slt32(local_tid_9345, segscan_group_sizze_9109)) {\n                ((volatile __local\n                  int32_t *) scan_arr_mem_9349)[squot32(local_tid_9345, 32)] =\n                    x_9338;\n                ((volatile __local\n                  int32_t *) scan_arr_mem_9351)[squot32(local_tid_9345, 32)] =\n                    x_9339;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // scan the first block, after which offset 'i' contains carry-in for block 'i+1'\n        {\n            int32_t skip_threads_9363;\n            \n            if (squot32(local_tid_9345, 32) == 0 && slt32(local_tid_9345,\n                                      ",
                   "                    segscan_group_sizze_9109)) {\n                x_9358 = ((volatile __local\n                           int32_t *) scan_arr_mem_9349)[local_tid_9345];\n                x_9359 = ((volatile __local\n                           int32_t *) scan_arr_mem_9351)[local_tid_9345];\n                if (sub32(local_tid_9345, mul32(squot32(local_tid_9345, 32),\n                                                32)) == 0) {\n                    x_9356 = x_9358;\n                    x_9357 = x_9359;\n                }\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                skip_threads_9363 = 1;\n                while (slt32(skip_threads_9363, 32)) {\n                    if (sle32(skip_threads_9363, sub32(local_tid_9345,\n                                                       mul32(squot32(local_tid_9345,\n                                                                     32),\n                                                             32))) &&\n                        (squot32(local_tid_9345, 32) == 0 &&\n                         slt32(local_tid_9345, segscan_group_sizze_9109))) {\n                        // read operands\n                        {\n                            x_9356 = ((volatile __local\n                                       int32_t *) scan_arr_mem_9349)[sub32(local_tid_9345,\n                                                                           skip_threads_9363)];\n                            x_9357 = ((volatile __local\n                                       int32_t *) scan_arr_mem_9351)[sub32(local_tid_9345,\n                                                                           skip_threads_9363)];\n                        }\n                        // perform operation\n                        {\n                            int32_t res_9360 = add32(x_9356, x_9358);\n                            int32_t res_9361 = add32(x_9357, x_9359);\n                            \n                            x_9356 = res_9360;",
                   "\n                            x_9357 = res_9361;\n                        }\n                    }\n                    if (sle32(wave_sizze_9347, skip_threads_9363)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (sle32(skip_threads_9363, sub32(local_tid_9345,\n                                                       mul32(squot32(local_tid_9345,\n                                                                     32),\n                                                             32))) &&\n                        (squot32(local_tid_9345, 32) == 0 &&\n                         slt32(local_tid_9345, segscan_group_sizze_9109))) {\n                        // write result\n                        {\n                            ((volatile __local\n                              int32_t *) scan_arr_mem_9349)[local_tid_9345] =\n                                x_9356;\n                            x_9358 = x_9356;\n                            ((volatile __local\n                              int32_t *) scan_arr_mem_9351)[local_tid_9345] =\n                                x_9357;\n                            x_9359 = x_9357;\n                        }\n                    }\n                    if (sle32(wave_sizze_9347, skip_threads_9363)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_9363 *= 2;\n                }\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // carry-in for every block except the first\n        {\n            if (!(squot32(local_tid_9345, 32) == 0 || !slt32(local_tid_9345,\n                                                             segscan_group_sizze_9109))) {\n                // read operands\n                {\n                    x_9340 = x_9338;\n                    x_9341 = x_9339;\n                    x_9338 = ((__local\n                               int32_t *) scan_arr_mem_9349)[sub32(squot32(local_tid_9345,\n                                ",
                   "                                           32),\n                                                                   1)];\n                    x_9339 = ((__local\n                               int32_t *) scan_arr_mem_9351)[sub32(squot32(local_tid_9345,\n                                                                           32),\n                                                                   1)];\n                }\n                // perform operation\n                {\n                    int32_t res_9342 = add32(x_9338, x_9340);\n                    int32_t res_9343 = add32(x_9339, x_9341);\n                    \n                    x_9338 = res_9342;\n                    x_9339 = res_9343;\n                }\n                // write final result\n                {\n                    ((__local int32_t *) scan_arr_mem_9349)[local_tid_9345] =\n                        x_9338;\n                    ((__local int32_t *) scan_arr_mem_9351)[local_tid_9345] =\n                        x_9339;\n                }\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // restore correct values for first block\n        {\n            if (squot32(local_tid_9345, 32) == 0) {\n                ((__local int32_t *) scan_arr_mem_9349)[local_tid_9345] =\n                    x_9340;\n                ((__local int32_t *) scan_arr_mem_9351)[local_tid_9345] =\n                    x_9341;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // threads in bounds write partial scan result\n        {\n            if (slt32(gtid_9113, n_8851)) {\n                ((__global int32_t *) mem_9236)[gtid_9113] = ((__local\n                                                               int32_t *) scan_arr_mem_9349)[local_tid_9345];\n                ((__global int32_t *) mem_9239)[gtid_9113] = ((__local\n                                                               int32_t *) scan_arr_mem_9351)[local_tid_9345];\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // first ",
                   "thread reads last element as carry-in for next iteration\n        {\n            bool crosses_segment_9364 = 0;\n            bool should_load_carry_9365 = local_tid_9345 == 0 &&\n                 !crosses_segment_9364;\n            \n            if (should_load_carry_9365) {\n                x_8880 = ((__local\n                           int32_t *) scan_arr_mem_9349)[sub32(segscan_group_sizze_9109,\n                                                               1)];\n                x_8881 = ((__local\n                           int32_t *) scan_arr_mem_9351)[sub32(segscan_group_sizze_9109,\n                                                               1)];\n            }\n            if (!should_load_carry_9365) {\n                x_8880 = 0;\n                x_8881 = 0;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n    }\n    \n  error_1:\n    return;\n    #undef segscan_group_sizze_9109\n}\n__kernel void scan_stage1_9163(__global int *global_failure, __local volatile\n                               int64_t *scan_arr_mem_9462_backing_aligned_0,\n                               __local volatile\n                               int64_t *scan_arr_mem_9460_backing_aligned_1,\n                               int32_t n_8851, int32_t rotate_arg_8927, __global\n                               unsigned char *mem_9269, __global\n                               unsigned char *mem_9272, __global\n                               unsigned char *mem_9276, __global\n                               unsigned char *mem_9278,\n                               int32_t num_threads_9445)\n{\n    #define segscan_group_sizze_9158 (mainzisegscan_group_sizze_9157)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict scan_arr_mem_9462_backing_1 =\n                          (__local volatile\n                           char *) scan_arr_mem_9462_backing_aligned_0;\n    __local volatile char *restrict scan_arr_mem_9460_backing_0 =\n               ",
                   "           (__local volatile\n                           char *) scan_arr_mem_9460_backing_aligned_1;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9455;\n    int32_t local_tid_9456;\n    int32_t group_sizze_9459;\n    int32_t wave_sizze_9458;\n    int32_t group_tid_9457;\n    \n    global_tid_9455 = get_global_id(0);\n    local_tid_9456 = get_local_id(0);\n    group_sizze_9459 = get_local_size(0);\n    wave_sizze_9458 = LOCKSTEP_WIDTH;\n    group_tid_9457 = get_group_id(0);\n    \n    int32_t phys_tid_9163 = global_tid_9455;\n    __local char *scan_arr_mem_9460;\n    \n    scan_arr_mem_9460 = (__local char *) scan_arr_mem_9460_backing_0;\n    \n    __local char *scan_arr_mem_9462;\n    \n    scan_arr_mem_9462 = (__local char *) scan_arr_mem_9462_backing_1;\n    \n    int32_t x_8974;\n    bool x_8975;\n    int32_t x_8976;\n    bool x_8977;\n    \n    x_8974 = 0;\n    x_8975 = 0;\n    for (int32_t j_9464 = 0; j_9464 < squot32(sub32(add32(n_8851,\n                                                          num_threads_9445), 1),\n                                              num_threads_9445); j_9464++) {\n        int32_t chunk_offset_9465 = add32(mul32(segscan_group_sizze_9158,\n                                                j_9464), mul32(group_tid_9457,\n                                                               mul32(segscan_group_sizze_9158,\n                                                                     squot32(sub32(add32(n_8851,\n                                                                                         num_threads_9445),\n                                                                                   1),\n                                                                             num_threads_9445))));\n        int32_t flat_idx_9466 = add32(chunk_offset_9465, local_tid_9456);\n        int32_t gtid_9162 = flat_idx_9466;\n        \n        // threads in bounds read input; others get neutral element\n        {\n            if (slt32(gtid_9162, n_8",
                   "851)) {\n                int32_t x_8983 = ((__global int32_t *) mem_9269)[gtid_9162];\n                int32_t i_p_o_9211 = add32(rotate_arg_8927, gtid_9162);\n                int32_t rot_i_9212 = smod32(i_p_o_9211, n_8851);\n                int32_t x_8984 = ((__global int32_t *) mem_9269)[rot_i_9212];\n                int32_t x_8985 = ((__global int32_t *) mem_9272)[gtid_9162];\n                int32_t x_8986 = sub32(x_8984, x_8983);\n                bool cond_8987 = sle32(0, x_8986);\n                bool x_8988 = !cond_8987;\n                \n                // write to-scan values to parameters\n                {\n                    x_8976 = x_8985;\n                    x_8977 = x_8988;\n                }\n                // write mapped values results to global memory\n                { }\n            } else {\n                x_8976 = 0;\n                x_8977 = 0;\n            }\n        }\n        // combine with carry and write to local memory\n        {\n            int32_t res_8978;\n            bool res_8979;\n            \n            if (x_8977) {\n                bool res_8980 = x_8975 || x_8977;\n                \n                res_8978 = x_8976;\n                res_8979 = res_8980;\n            } else {\n                int32_t res_8981 = add32(x_8974, x_8976);\n                bool res_8982 = x_8975 || x_8977;\n                \n                res_8978 = res_8981;\n                res_8979 = res_8982;\n            }\n            ((__local int32_t *) scan_arr_mem_9460)[local_tid_9456] = res_8978;\n            ((__local bool *) scan_arr_mem_9462)[local_tid_9456] = res_8979;\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        \n        int32_t x_9446;\n        bool x_9447;\n        int32_t x_9448;\n        bool x_9449;\n        int32_t x_9467;\n        bool x_9468;\n        int32_t x_9469;\n        bool x_9470;\n        int32_t skip_threads_9476;\n        \n        if (slt32(local_tid_9456, segscan_group_sizze_9158)) {\n            x_9448 = ((volatile __local\n                       int32_t *",
                   ") scan_arr_mem_9460)[local_tid_9456];\n            x_9449 = ((volatile __local\n                       bool *) scan_arr_mem_9462)[local_tid_9456];\n            if (sub32(local_tid_9456, mul32(squot32(local_tid_9456, 32), 32)) ==\n                0) {\n                x_9446 = x_9448;\n                x_9447 = x_9449;\n            }\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            skip_threads_9476 = 1;\n            while (slt32(skip_threads_9476, 32)) {\n                if (sle32(skip_threads_9476, sub32(local_tid_9456,\n                                                   mul32(squot32(local_tid_9456,\n                                                                 32), 32))) &&\n                    slt32(local_tid_9456, segscan_group_sizze_9158)) {\n                    // read operands\n                    {\n                        x_9446 = ((volatile __local\n                                   int32_t *) scan_arr_mem_9460)[sub32(local_tid_9456,\n                                                                       skip_threads_9476)];\n                        x_9447 = ((volatile __local\n                                   bool *) scan_arr_mem_9462)[sub32(local_tid_9456,\n                                                                    skip_threads_9476)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_9450;\n                        bool res_9451;\n                        \n                        if (x_9449) {\n                            bool res_9452 = x_9447 || x_9449;\n                            \n                            res_9450 = x_9448;\n                            res_9451 = res_9452;\n                        } else {\n                            int32_t res_9453 = add32(x_9446, x_9448);\n                            bool res_9454 = x_9447 || x_9449;\n                            \n                            res_9450 = res_9453;\n                            res_9451 = ",
                   "res_9454;\n                        }\n                        x_9446 = res_9450;\n                        x_9447 = res_9451;\n                    }\n                }\n                if (sle32(wave_sizze_9458, skip_threads_9476)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if (sle32(skip_threads_9476, sub32(local_tid_9456,\n                                                   mul32(squot32(local_tid_9456,\n                                                                 32), 32))) &&\n                    slt32(local_tid_9456, segscan_group_sizze_9158)) {\n                    // write result\n                    {\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_9460)[local_tid_9456] =\n                            x_9446;\n                        x_9448 = x_9446;\n                        ((volatile __local\n                          bool *) scan_arr_mem_9462)[local_tid_9456] = x_9447;\n                        x_9449 = x_9447;\n                    }\n                }\n                if (sle32(wave_sizze_9458, skip_threads_9476)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_9476 *= 2;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // last thread of block 'i' writes its result to offset 'i'\n        {\n            if (sub32(local_tid_9456, mul32(squot32(local_tid_9456, 32), 32)) ==\n                31 && slt32(local_tid_9456, segscan_group_sizze_9158)) {\n                ((volatile __local\n                  int32_t *) scan_arr_mem_9460)[squot32(local_tid_9456, 32)] =\n                    x_9446;\n                ((volatile __local\n                  bool *) scan_arr_mem_9462)[squot32(local_tid_9456, 32)] =\n                    x_9447;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // scan the first block, after which offset 'i' contains carry-in for block 'i+1'\n        {\n            int32_t skip_threads_9477;\n   ",
                   "         \n            if (squot32(local_tid_9456, 32) == 0 && slt32(local_tid_9456,\n                                                          segscan_group_sizze_9158)) {\n                x_9469 = ((volatile __local\n                           int32_t *) scan_arr_mem_9460)[local_tid_9456];\n                x_9470 = ((volatile __local\n                           bool *) scan_arr_mem_9462)[local_tid_9456];\n                if (sub32(local_tid_9456, mul32(squot32(local_tid_9456, 32),\n                                                32)) == 0) {\n                    x_9467 = x_9469;\n                    x_9468 = x_9470;\n                }\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                skip_threads_9477 = 1;\n                while (slt32(skip_threads_9477, 32)) {\n                    if (sle32(skip_threads_9477, sub32(local_tid_9456,\n                                                       mul32(squot32(local_tid_9456,\n                                                                     32),\n                                                             32))) &&\n                        (squot32(local_tid_9456, 32) == 0 &&\n                         slt32(local_tid_9456, segscan_group_sizze_9158))) {\n                        // read operands\n                        {\n                            x_9467 = ((volatile __local\n                                       int32_t *) scan_arr_mem_9460)[sub32(local_tid_9456,\n                                                                           skip_threads_9477)];\n                            x_9468 = ((volatile __local\n                                       bool *) scan_arr_mem_9462)[sub32(local_tid_9456,\n                                                                        skip_threads_9477)];\n                        }\n                        // perform operation\n                        {\n                            int32_t res_9471;\n                            bool res_9472;\n             ",
                   "               \n                            if (x_9470) {\n                                bool res_9473 = x_9468 || x_9470;\n                                \n                                res_9471 = x_9469;\n                                res_9472 = res_9473;\n                            } else {\n                                int32_t res_9474 = add32(x_9467, x_9469);\n                                bool res_9475 = x_9468 || x_9470;\n                                \n                                res_9471 = res_9474;\n                                res_9472 = res_9475;\n                            }\n                            x_9467 = res_9471;\n                            x_9468 = res_9472;\n                        }\n                    }\n                    if (sle32(wave_sizze_9458, skip_threads_9477)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (sle32(skip_threads_9477, sub32(local_tid_9456,\n                                                       mul32(squot32(local_tid_9456,\n                                                                     32),\n                                                             32))) &&\n                        (squot32(local_tid_9456, 32) == 0 &&\n                         slt32(local_tid_9456, segscan_group_sizze_9158))) {\n                        // write result\n                        {\n                            ((volatile __local\n                              int32_t *) scan_arr_mem_9460)[local_tid_9456] =\n                                x_9467;\n                            x_9469 = x_9467;\n                            ((volatile __local\n                              bool *) scan_arr_mem_9462)[local_tid_9456] =\n                                x_9468;\n                            x_9470 = x_9468;\n                        }\n                    }\n                    if (sle32(wave_sizze_9458, skip_threads_9477)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n              ",
                   "      }\n                    skip_threads_9477 *= 2;\n                }\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // carry-in for every block except the first\n        {\n            if (!(squot32(local_tid_9456, 32) == 0 || !slt32(local_tid_9456,\n                                                             segscan_group_sizze_9158))) {\n                // read operands\n                {\n                    x_9448 = x_9446;\n                    x_9449 = x_9447;\n                    x_9446 = ((__local\n                               int32_t *) scan_arr_mem_9460)[sub32(squot32(local_tid_9456,\n                                                                           32),\n                                                                   1)];\n                    x_9447 = ((__local\n                               bool *) scan_arr_mem_9462)[sub32(squot32(local_tid_9456,\n                                                                        32),\n                                                                1)];\n                }\n                // perform operation\n                {\n                    int32_t res_9450;\n                    bool res_9451;\n                    \n                    if (x_9449) {\n                        bool res_9452 = x_9447 || x_9449;\n                        \n                        res_9450 = x_9448;\n                        res_9451 = res_9452;\n                    } else {\n                        int32_t res_9453 = add32(x_9446, x_9448);\n                        bool res_9454 = x_9447 || x_9449;\n                        \n                        res_9450 = res_9453;\n                        res_9451 = res_9454;\n                    }\n                    x_9446 = res_9450;\n                    x_9447 = res_9451;\n                }\n                // write final result\n                {\n                    ((__local int32_t *) scan_arr_mem_9460)[local_tid_9456] =\n                        x_9446;\n                    ((__local boo",
                   "l *) scan_arr_mem_9462)[local_tid_9456] =\n                        x_9447;\n                }\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // restore correct values for first block\n        {\n            if (squot32(local_tid_9456, 32) == 0) {\n                ((__local int32_t *) scan_arr_mem_9460)[local_tid_9456] =\n                    x_9448;\n                ((__local bool *) scan_arr_mem_9462)[local_tid_9456] = x_9449;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // threads in bounds write partial scan result\n        {\n            if (slt32(gtid_9162, n_8851)) {\n                ((__global int32_t *) mem_9276)[gtid_9162] = ((__local\n                                                               int32_t *) scan_arr_mem_9460)[local_tid_9456];\n                ((__global bool *) mem_9278)[gtid_9162] = ((__local\n                                                            bool *) scan_arr_mem_9462)[local_tid_9456];\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // first thread reads last element as carry-in for next iteration\n        {\n            bool crosses_segment_9478 = 0;\n            bool should_load_carry_9479 = local_tid_9456 == 0 &&\n                 !crosses_segment_9478;\n            \n            if (should_load_carry_9479) {\n                x_8974 = ((__local\n                           int32_t *) scan_arr_mem_9460)[sub32(segscan_group_sizze_9158,\n                                                               1)];\n                x_8975 = ((__local\n                           bool *) scan_arr_mem_9462)[sub32(segscan_group_sizze_9158,\n                                                            1)];\n            }\n            if (!should_load_carry_9479) {\n                x_8974 = 0;\n                x_8975 = 0;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n    }\n    \n  error_1:\n    return;\n    #undef segscan_group_sizze_9158\n}\n__kernel void scan_stage1_9172(__global int *global_fai",
                   "lure, __local volatile\n                               int64_t *scan_arr_mem_9554_backing_aligned_0,\n                               __local volatile\n                               int64_t *scan_arr_mem_9552_backing_aligned_1,\n                               __local volatile\n                               int64_t *scan_arr_mem_9550_backing_aligned_2,\n                               int32_t n_8851, int32_t rotate_arg_8927, __global\n                               unsigned char *mem_9269, __global\n                               unsigned char *mem_9272, __global\n                               unsigned char *mem_9276, __global\n                               unsigned char *mem_9282, __global\n                               unsigned char *mem_9285, __global\n                               unsigned char *mem_9287, __global\n                               unsigned char *mem_9290,\n                               int32_t num_threads_9532)\n{\n    #define segscan_group_sizze_9167 (mainzisegscan_group_sizze_9166)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict scan_arr_mem_9554_backing_2 =\n                          (__local volatile\n                           char *) scan_arr_mem_9554_backing_aligned_0;\n    __local volatile char *restrict scan_arr_mem_9552_backing_1 =\n                          (__local volatile\n                           char *) scan_arr_mem_9552_backing_aligned_1;\n    __local volatile char *restrict scan_arr_mem_9550_backing_0 =\n                          (__local volatile\n                           char *) scan_arr_mem_9550_backing_aligned_2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9545;\n    int32_t local_tid_9546;\n    int32_t group_sizze_9549;\n    int32_t wave_sizze_9548;\n    int32_t group_tid_9547;\n    \n    global_tid_9545 = get_global_id(0);\n    local_tid_9546 = get_local_id(0);\n    group_sizze_9549 = get_local_size(0);\n    wave_sizze_9548 = LOCKST",
                   "EP_WIDTH;\n    group_tid_9547 = get_group_id(0);\n    \n    int32_t phys_tid_9172 = global_tid_9545;\n    __local char *scan_arr_mem_9550;\n    \n    scan_arr_mem_9550 = (__local char *) scan_arr_mem_9550_backing_0;\n    \n    __local char *scan_arr_mem_9552;\n    \n    scan_arr_mem_9552 = (__local char *) scan_arr_mem_9552_backing_1;\n    \n    __local char *scan_arr_mem_9554;\n    \n    scan_arr_mem_9554 = (__local char *) scan_arr_mem_9554_backing_2;\n    \n    int32_t x_9011;\n    int32_t x_9012;\n    bool x_9013;\n    int32_t x_9014;\n    int32_t x_9015;\n    bool x_9016;\n    \n    x_9011 = 0;\n    x_9012 = 0;\n    x_9013 = 0;\n    for (int32_t j_9556 = 0; j_9556 < squot32(sub32(add32(n_8851,\n                                                          num_threads_9532), 1),\n                                              num_threads_9532); j_9556++) {\n        int32_t chunk_offset_9557 = add32(mul32(segscan_group_sizze_9167,\n                                                j_9556), mul32(group_tid_9547,\n                                                               mul32(segscan_group_sizze_9167,\n                                                                     squot32(sub32(add32(n_8851,\n                                                                                         num_threads_9532),\n                                                                                   1),\n                                                                             num_threads_9532))));\n        int32_t flat_idx_9558 = add32(chunk_offset_9557, local_tid_9546);\n        int32_t gtid_9171 = flat_idx_9558;\n        \n        // threads in bounds read input; others get neutral element\n        {\n            if (slt32(gtid_9171, n_8851)) {\n                int32_t x_9023 = ((__global int32_t *) mem_9276)[gtid_9171];\n                int32_t x_9025 = ((__global int32_t *) mem_9269)[gtid_9171];\n                int32_t i_p_o_9213 = add32(rotate_arg_8927, gtid_9171);\n                int32_t rot_i_9214 = smod32(i_p",
                   "_o_9213, n_8851);\n                int32_t x_9026 = ((__global int32_t *) mem_9269)[rot_i_9214];\n                int32_t x_9027 = ((__global int32_t *) mem_9272)[gtid_9171];\n                int32_t x_9030 = sub32(x_9026, x_9025);\n                bool cond_9031 = sle32(0, x_9030);\n                bool x_9032 = !cond_9031;\n                int32_t res_9033 = btoi_bool_i32(x_9032);\n                \n                // write to-scan values to parameters\n                {\n                    x_9014 = res_9033;\n                    x_9015 = x_9027;\n                    x_9016 = x_9032;\n                }\n                // write mapped values results to global memory\n                {\n                    ((__global int32_t *) mem_9290)[gtid_9171] = x_9023;\n                }\n            } else {\n                x_9014 = 0;\n                x_9015 = 0;\n                x_9016 = 0;\n            }\n        }\n        // combine with carry and write to local memory\n        {\n            int32_t res_9017 = add32(x_9011, x_9014);\n            int32_t res_9018;\n            bool res_9019;\n            \n            if (x_9016) {\n                bool res_9020 = x_9013 || x_9016;\n                \n                res_9018 = x_9015;\n                res_9019 = res_9020;\n            } else {\n                int32_t res_9021 = add32(x_9012, x_9015);\n                bool res_9022 = x_9013 || x_9016;\n                \n                res_9018 = res_9021;\n                res_9019 = res_9022;\n            }\n            ((__local int32_t *) scan_arr_mem_9550)[local_tid_9546] = res_9017;\n            ((__local int32_t *) scan_arr_mem_9552)[local_tid_9546] = res_9018;\n            ((__local bool *) scan_arr_mem_9554)[local_tid_9546] = res_9019;\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        \n        int32_t x_9533;\n        int32_t x_9534;\n        bool x_9535;\n        int32_t x_9536;\n        int32_t x_9537;\n        bool x_9538;\n        int32_t x_9559;\n        int32_t x_9560;\n        bool x_9561;\n        ",
                   "int32_t x_9562;\n        int32_t x_9563;\n        bool x_9564;\n        int32_t skip_threads_9571;\n        \n        if (slt32(local_tid_9546, segscan_group_sizze_9167)) {\n            x_9536 = ((volatile __local\n                       int32_t *) scan_arr_mem_9550)[local_tid_9546];\n            x_9537 = ((volatile __local\n                       int32_t *) scan_arr_mem_9552)[local_tid_9546];\n            x_9538 = ((volatile __local\n                       bool *) scan_arr_mem_9554)[local_tid_9546];\n            if (sub32(local_tid_9546, mul32(squot32(local_tid_9546, 32), 32)) ==\n                0) {\n                x_9533 = x_9536;\n                x_9534 = x_9537;\n                x_9535 = x_9538;\n            }\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            skip_threads_9571 = 1;\n            while (slt32(skip_threads_9571, 32)) {\n                if (sle32(skip_threads_9571, sub32(local_tid_9546,\n                                                   mul32(squot32(local_tid_9546,\n                                                                 32), 32))) &&\n                    slt32(local_tid_9546, segscan_group_sizze_9167)) {\n                    // read operands\n                    {\n                        x_9533 = ((volatile __local\n                                   int32_t *) scan_arr_mem_9550)[sub32(local_tid_9546,\n                                                                       skip_threads_9571)];\n                        x_9534 = ((volatile __local\n                                   int32_t *) scan_arr_mem_9552)[sub32(local_tid_9546,\n                                                                       skip_threads_9571)];\n                        x_9535 = ((volatile __local\n                                   bool *) scan_arr_mem_9554)[sub32(local_tid_9546,\n                                                                    skip_threads_9571)];\n                    }\n                    // perform operation\n                    {",
                   "\n                        int32_t res_9539 = add32(x_9533, x_9536);\n                        int32_t res_9540;\n                        bool res_9541;\n                        \n                        if (x_9538) {\n                            bool res_9542 = x_9535 || x_9538;\n                            \n                            res_9540 = x_9537;\n                            res_9541 = res_9542;\n                        } else {\n                            int32_t res_9543 = add32(x_9534, x_9537);\n                            bool res_9544 = x_9535 || x_9538;\n                            \n                            res_9540 = res_9543;\n                            res_9541 = res_9544;\n                        }\n                        x_9533 = res_9539;\n                        x_9534 = res_9540;\n                        x_9535 = res_9541;\n                    }\n                }\n                if (sle32(wave_sizze_9548, skip_threads_9571)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if (sle32(skip_threads_9571, sub32(local_tid_9546,\n                                                   mul32(squot32(local_tid_9546,\n                                                                 32), 32))) &&\n                    slt32(local_tid_9546, segscan_group_sizze_9167)) {\n                    // write result\n                    {\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_9550)[local_tid_9546] =\n                            x_9533;\n                        x_9536 = x_9533;\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_9552)[local_tid_9546] =\n                            x_9534;\n                        x_9537 = x_9534;\n                        ((volatile __local\n                          bool *) scan_arr_mem_9554)[local_tid_9546] = x_9535;\n                        x_9538 = x_9535;\n                    }\n                }\n                if (sle32(wave_sizz",
                   "e_9548, skip_threads_9571)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_9571 *= 2;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // last thread of block 'i' writes its result to offset 'i'\n        {\n            if (sub32(local_tid_9546, mul32(squot32(local_tid_9546, 32), 32)) ==\n                31 && slt32(local_tid_9546, segscan_group_sizze_9167)) {\n                ((volatile __local\n                  int32_t *) scan_arr_mem_9550)[squot32(local_tid_9546, 32)] =\n                    x_9533;\n                ((volatile __local\n                  int32_t *) scan_arr_mem_9552)[squot32(local_tid_9546, 32)] =\n                    x_9534;\n                ((volatile __local\n                  bool *) scan_arr_mem_9554)[squot32(local_tid_9546, 32)] =\n                    x_9535;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // scan the first block, after which offset 'i' contains carry-in for block 'i+1'\n        {\n            int32_t skip_threads_9572;\n            \n            if (squot32(local_tid_9546, 32) == 0 && slt32(local_tid_9546,\n                                                          segscan_group_sizze_9167)) {\n                x_9562 = ((volatile __local\n                           int32_t *) scan_arr_mem_9550)[local_tid_9546];\n                x_9563 = ((volatile __local\n                           int32_t *) scan_arr_mem_9552)[local_tid_9546];\n                x_9564 = ((volatile __local\n                           bool *) scan_arr_mem_9554)[local_tid_9546];\n                if (sub32(local_tid_9546, mul32(squot32(local_tid_9546, 32),\n                                                32)) == 0) {\n                    x_9559 = x_9562;\n                    x_9560 = x_9563;\n                    x_9561 = x_9564;\n                }\n            }\n            // in-block scan (hopefully no barriers needed)\n            {\n                skip_threads_9572 = 1;\n                while (sl",
                   "t32(skip_threads_9572, 32)) {\n                    if (sle32(skip_threads_9572, sub32(local_tid_9546,\n                                                       mul32(squot32(local_tid_9546,\n                                                                     32),\n                                                             32))) &&\n                        (squot32(local_tid_9546, 32) == 0 &&\n                         slt32(local_tid_9546, segscan_group_sizze_9167))) {\n                        // read operands\n                        {\n                            x_9559 = ((volatile __local\n                                       int32_t *) scan_arr_mem_9550)[sub32(local_tid_9546,\n                                                                           skip_threads_9572)];\n                            x_9560 = ((volatile __local\n                                       int32_t *) scan_arr_mem_9552)[sub32(local_tid_9546,\n                                                                           skip_threads_9572)];\n                            x_9561 = ((volatile __local\n                                       bool *) scan_arr_mem_9554)[sub32(local_tid_9546,\n                                                                        skip_threads_9572)];\n                        }\n                        // perform operation\n                        {\n                            int32_t res_9565 = add32(x_9559, x_9562);\n                            int32_t res_9566;\n                            bool res_9567;\n                            \n                            if (x_9564) {\n                                bool res_9568 = x_9561 || x_9564;\n                                \n                                res_9566 = x_9563;\n                                res_9567 = res_9568;\n                            } else {\n                                int32_t res_9569 = add32(x_9560, x_9563);\n                                bool res_9570 = x_9561 || x_9564;\n                                \n  ",
                   "                              res_9566 = res_9569;\n                                res_9567 = res_9570;\n                            }\n                            x_9559 = res_9565;\n                            x_9560 = res_9566;\n                            x_9561 = res_9567;\n                        }\n                    }\n                    if (sle32(wave_sizze_9548, skip_threads_9572)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    if (sle32(skip_threads_9572, sub32(local_tid_9546,\n                                                       mul32(squot32(local_tid_9546,\n                                                                     32),\n                                                             32))) &&\n                        (squot32(local_tid_9546, 32) == 0 &&\n                         slt32(local_tid_9546, segscan_group_sizze_9167))) {\n                        // write result\n                        {\n                            ((volatile __local\n                              int32_t *) scan_arr_mem_9550)[local_tid_9546] =\n                                x_9559;\n                            x_9562 = x_9559;\n                            ((volatile __local\n                              int32_t *) scan_arr_mem_9552)[local_tid_9546] =\n                                x_9560;\n                            x_9563 = x_9560;\n                            ((volatile __local\n                              bool *) scan_arr_mem_9554)[local_tid_9546] =\n                                x_9561;\n                            x_9564 = x_9561;\n                        }\n                    }\n                    if (sle32(wave_sizze_9548, skip_threads_9572)) {\n                        barrier(CLK_LOCAL_MEM_FENCE);\n                    }\n                    skip_threads_9572 *= 2;\n                }\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // carry-in for every block except the first\n        {\n            if (!(squ",
                   "ot32(local_tid_9546, 32) == 0 || !slt32(local_tid_9546,\n                                                             segscan_group_sizze_9167))) {\n                // read operands\n                {\n                    x_9536 = x_9533;\n                    x_9537 = x_9534;\n                    x_9538 = x_9535;\n                    x_9533 = ((__local\n                               int32_t *) scan_arr_mem_9550)[sub32(squot32(local_tid_9546,\n                                                                           32),\n                                                                   1)];\n                    x_9534 = ((__local\n                               int32_t *) scan_arr_mem_9552)[sub32(squot32(local_tid_9546,\n                                                                           32),\n                                                                   1)];\n                    x_9535 = ((__local\n                               bool *) scan_arr_mem_9554)[sub32(squot32(local_tid_9546,\n                                                                        32),\n                                                                1)];\n                }\n                // perform operation\n                {\n                    int32_t res_9539 = add32(x_9533, x_9536);\n                    int32_t res_9540;\n                    bool res_9541;\n                    \n                    if (x_9538) {\n                        bool res_9542 = x_9535 || x_9538;\n                        \n                        res_9540 = x_9537;\n                        res_9541 = res_9542;\n                    } else {\n                        int32_t res_9543 = add32(x_9534, x_9537);\n                        bool res_9544 = x_9535 || x_9538;\n                        \n                        res_9540 = res_9543;\n                        res_9541 = res_9544;\n                    }\n                    x_9533 = res_9539;\n                    x_9534 = res_9540;\n                    x_9535 = res_9541;\n             ",
                   "   }\n                // write final result\n                {\n                    ((__local int32_t *) scan_arr_mem_9550)[local_tid_9546] =\n                        x_9533;\n                    ((__local int32_t *) scan_arr_mem_9552)[local_tid_9546] =\n                        x_9534;\n                    ((__local bool *) scan_arr_mem_9554)[local_tid_9546] =\n                        x_9535;\n                }\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // restore correct values for first block\n        {\n            if (squot32(local_tid_9546, 32) == 0) {\n                ((__local int32_t *) scan_arr_mem_9550)[local_tid_9546] =\n                    x_9536;\n                ((__local int32_t *) scan_arr_mem_9552)[local_tid_9546] =\n                    x_9537;\n                ((__local bool *) scan_arr_mem_9554)[local_tid_9546] = x_9538;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // threads in bounds write partial scan result\n        {\n            if (slt32(gtid_9171, n_8851)) {\n                ((__global int32_t *) mem_9282)[gtid_9171] = ((__local\n                                                               int32_t *) scan_arr_mem_9550)[local_tid_9546];\n                ((__global int32_t *) mem_9285)[gtid_9171] = ((__local\n                                                               int32_t *) scan_arr_mem_9552)[local_tid_9546];\n                ((__global bool *) mem_9287)[gtid_9171] = ((__local\n                                                            bool *) scan_arr_mem_9554)[local_tid_9546];\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // first thread reads last element as carry-in for next iteration\n        {\n            bool crosses_segment_9573 = 0;\n            bool should_load_carry_9574 = local_tid_9546 == 0 &&\n                 !crosses_segment_9573;\n            \n            if (should_load_carry_9574) {\n                x_9011 = ((__local\n                           int32_t *) scan_arr_m",
                   "em_9550)[sub32(segscan_group_sizze_9167,\n                                                               1)];\n                x_9012 = ((__local\n                           int32_t *) scan_arr_mem_9552)[sub32(segscan_group_sizze_9167,\n                                                               1)];\n                x_9013 = ((__local\n                           bool *) scan_arr_mem_9554)[sub32(segscan_group_sizze_9167,\n                                                            1)];\n            }\n            if (!should_load_carry_9574) {\n                x_9011 = 0;\n                x_9012 = 0;\n                x_9013 = 0;\n            }\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n    }\n    \n  error_1:\n    return;\n    #undef segscan_group_sizze_9167\n}\n__kernel void scan_stage2_9114(__global int *global_failure, __local volatile\n                               int64_t *scan_arr_mem_9385_backing_aligned_0,\n                               __local volatile\n                               int64_t *scan_arr_mem_9383_backing_aligned_1,\n                               int32_t n_8851, int32_t num_groups_9111, __global\n                               unsigned char *mem_9236, __global\n                               unsigned char *mem_9239,\n                               int32_t num_threads_9337)\n{\n    #define segscan_group_sizze_9109 (mainzisegscan_group_sizze_9108)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict scan_arr_mem_9385_backing_1 =\n                          (__local volatile\n                           char *) scan_arr_mem_9385_backing_aligned_0;\n    __local volatile char *restrict scan_arr_mem_9383_backing_0 =\n                          (__local volatile\n                           char *) scan_arr_mem_9383_backing_aligned_1;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9378;\n    int32_t local_tid_9379;\n    int32_t group_sizze_9382;\n    int32_t wave_sizze_93",
                   "81;\n    int32_t group_tid_9380;\n    \n    global_tid_9378 = get_global_id(0);\n    local_tid_9379 = get_local_id(0);\n    group_sizze_9382 = get_local_size(0);\n    wave_sizze_9381 = LOCKSTEP_WIDTH;\n    group_tid_9380 = get_group_id(0);\n    \n    int32_t phys_tid_9114 = global_tid_9378;\n    __local char *scan_arr_mem_9383;\n    \n    scan_arr_mem_9383 = (__local char *) scan_arr_mem_9383_backing_0;\n    \n    __local char *scan_arr_mem_9385;\n    \n    scan_arr_mem_9385 = (__local char *) scan_arr_mem_9385_backing_1;\n    \n    int32_t flat_idx_9387 = sub32(mul32(add32(local_tid_9379, 1),\n                                        mul32(segscan_group_sizze_9109,\n                                              squot32(sub32(add32(n_8851,\n                                                                  num_threads_9337),\n                                                            1),\n                                                      num_threads_9337))), 1);\n    int32_t gtid_9113 = flat_idx_9387;\n    \n    // threads in bound read carries; others get neutral element\n    {\n        if (slt32(gtid_9113, n_8851)) {\n            ((__local int32_t *) scan_arr_mem_9383)[local_tid_9379] = ((__global\n                                                                        int32_t *) mem_9236)[gtid_9113];\n            ((__local int32_t *) scan_arr_mem_9385)[local_tid_9379] = ((__global\n                                                                        int32_t *) mem_9239)[gtid_9113];\n        } else {\n            ((__local int32_t *) scan_arr_mem_9383)[local_tid_9379] = 0;\n            ((__local int32_t *) scan_arr_mem_9385)[local_tid_9379] = 0;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t x_9366;\n    int32_t x_9367;\n    int32_t x_9368;\n    int32_t x_9369;\n    int32_t x_9388;\n    int32_t x_9389;\n    int32_t x_9390;\n    int32_t x_9391;\n    int32_t skip_threads_9394;\n    \n    if (slt32(local_tid_9379, num_groups_9111)) {\n        x_9368 = ((volatile __local\n                ",
                   "   int32_t *) scan_arr_mem_9383)[local_tid_9379];\n        x_9369 = ((volatile __local\n                   int32_t *) scan_arr_mem_9385)[local_tid_9379];\n        if (sub32(local_tid_9379, mul32(squot32(local_tid_9379, 32), 32)) ==\n            0) {\n            x_9366 = x_9368;\n            x_9367 = x_9369;\n        }\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        skip_threads_9394 = 1;\n        while (slt32(skip_threads_9394, 32)) {\n            if (sle32(skip_threads_9394, sub32(local_tid_9379,\n                                               mul32(squot32(local_tid_9379,\n                                                             32), 32))) &&\n                slt32(local_tid_9379, num_groups_9111)) {\n                // read operands\n                {\n                    x_9366 = ((volatile __local\n                               int32_t *) scan_arr_mem_9383)[sub32(local_tid_9379,\n                                                                   skip_threads_9394)];\n                    x_9367 = ((volatile __local\n                               int32_t *) scan_arr_mem_9385)[sub32(local_tid_9379,\n                                                                   skip_threads_9394)];\n                }\n                // perform operation\n                {\n                    int32_t res_9370 = add32(x_9366, x_9368);\n                    int32_t res_9371 = add32(x_9367, x_9369);\n                    \n                    x_9366 = res_9370;\n                    x_9367 = res_9371;\n                }\n            }\n            if (sle32(wave_sizze_9381, skip_threads_9394)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            if (sle32(skip_threads_9394, sub32(local_tid_9379,\n                                               mul32(squot32(local_tid_9379,\n                                                             32), 32))) &&\n                slt32(local_tid_9379, num_groups_9111)) {\n                // write result\n                {\n          ",
                   "          ((volatile __local\n                      int32_t *) scan_arr_mem_9383)[local_tid_9379] = x_9366;\n                    x_9368 = x_9366;\n                    ((volatile __local\n                      int32_t *) scan_arr_mem_9385)[local_tid_9379] = x_9367;\n                    x_9369 = x_9367;\n                }\n            }\n            if (sle32(wave_sizze_9381, skip_threads_9394)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_9394 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if (sub32(local_tid_9379, mul32(squot32(local_tid_9379, 32), 32)) ==\n            31 && slt32(local_tid_9379, num_groups_9111)) {\n            ((volatile __local\n              int32_t *) scan_arr_mem_9383)[squot32(local_tid_9379, 32)] =\n                x_9366;\n            ((volatile __local\n              int32_t *) scan_arr_mem_9385)[squot32(local_tid_9379, 32)] =\n                x_9367;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for block 'i+1'\n    {\n        int32_t skip_threads_9395;\n        \n        if (squot32(local_tid_9379, 32) == 0 && slt32(local_tid_9379,\n                                                      num_groups_9111)) {\n            x_9390 = ((volatile __local\n                       int32_t *) scan_arr_mem_9383)[local_tid_9379];\n            x_9391 = ((volatile __local\n                       int32_t *) scan_arr_mem_9385)[local_tid_9379];\n            if (sub32(local_tid_9379, mul32(squot32(local_tid_9379, 32), 32)) ==\n                0) {\n                x_9388 = x_9390;\n                x_9389 = x_9391;\n            }\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            skip_threads_9395 = 1;\n            while (slt32(skip_threads_9395, 32)) {\n                if (sle32(skip_threads_9395, sub32(local_tid_9379,\n                                   ",
                   "                mul32(squot32(local_tid_9379,\n                                                                 32), 32))) &&\n                    (squot32(local_tid_9379, 32) == 0 && slt32(local_tid_9379,\n                                                               num_groups_9111))) {\n                    // read operands\n                    {\n                        x_9388 = ((volatile __local\n                                   int32_t *) scan_arr_mem_9383)[sub32(local_tid_9379,\n                                                                       skip_threads_9395)];\n                        x_9389 = ((volatile __local\n                                   int32_t *) scan_arr_mem_9385)[sub32(local_tid_9379,\n                                                                       skip_threads_9395)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_9392 = add32(x_9388, x_9390);\n                        int32_t res_9393 = add32(x_9389, x_9391);\n                        \n                        x_9388 = res_9392;\n                        x_9389 = res_9393;\n                    }\n                }\n                if (sle32(wave_sizze_9381, skip_threads_9395)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if (sle32(skip_threads_9395, sub32(local_tid_9379,\n                                                   mul32(squot32(local_tid_9379,\n                                                                 32), 32))) &&\n                    (squot32(local_tid_9379, 32) == 0 && slt32(local_tid_9379,\n                                                               num_groups_9111))) {\n                    // write result\n                    {\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_9383)[local_tid_9379] =\n                            x_9388;\n                        x_9390 = x_9388;\n                        ((volatile __local\n     ",
                   "                     int32_t *) scan_arr_mem_9385)[local_tid_9379] =\n                            x_9389;\n                        x_9391 = x_9389;\n                    }\n                }\n                if (sle32(wave_sizze_9381, skip_threads_9395)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_9395 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_9379, 32) == 0 || !slt32(local_tid_9379,\n                                                         num_groups_9111))) {\n            // read operands\n            {\n                x_9368 = x_9366;\n                x_9369 = x_9367;\n                x_9366 = ((__local\n                           int32_t *) scan_arr_mem_9383)[sub32(squot32(local_tid_9379,\n                                                                       32), 1)];\n                x_9367 = ((__local\n                           int32_t *) scan_arr_mem_9385)[sub32(squot32(local_tid_9379,\n                                                                       32), 1)];\n            }\n            // perform operation\n            {\n                int32_t res_9370 = add32(x_9366, x_9368);\n                int32_t res_9371 = add32(x_9367, x_9369);\n                \n                x_9366 = res_9370;\n                x_9367 = res_9371;\n            }\n            // write final result\n            {\n                ((__local int32_t *) scan_arr_mem_9383)[local_tid_9379] =\n                    x_9366;\n                ((__local int32_t *) scan_arr_mem_9385)[local_tid_9379] =\n                    x_9367;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(local_tid_9379, 32) == 0) {\n            ((__local int32_t *) scan_arr_mem_9383)[local_tid_9379] = x_9368;\n            ((__local int32_t *) scan_arr_mem_9385)[local_tid_9379] = x_9369;\n      ",
                   "  }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // threads in bounds write scanned carries\n    {\n        if (slt32(gtid_9113, n_8851)) {\n            ((__global int32_t *) mem_9236)[gtid_9113] = ((__local\n                                                           int32_t *) scan_arr_mem_9383)[local_tid_9379];\n            ((__global int32_t *) mem_9239)[gtid_9113] = ((__local\n                                                           int32_t *) scan_arr_mem_9385)[local_tid_9379];\n        }\n    }\n    \n  error_0:\n    return;\n    #undef segscan_group_sizze_9109\n}\n__kernel void scan_stage2_9163(__global int *global_failure, __local volatile\n                               int64_t *scan_arr_mem_9505_backing_aligned_0,\n                               __local volatile\n                               int64_t *scan_arr_mem_9503_backing_aligned_1,\n                               int32_t n_8851, int32_t num_groups_9160, __global\n                               unsigned char *mem_9276, __global\n                               unsigned char *mem_9278,\n                               int32_t num_threads_9445)\n{\n    #define segscan_group_sizze_9158 (mainzisegscan_group_sizze_9157)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict scan_arr_mem_9505_backing_1 =\n                          (__local volatile\n                           char *) scan_arr_mem_9505_backing_aligned_0;\n    __local volatile char *restrict scan_arr_mem_9503_backing_0 =\n                          (__local volatile\n                           char *) scan_arr_mem_9503_backing_aligned_1;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9498;\n    int32_t local_tid_9499;\n    int32_t group_sizze_9502;\n    int32_t wave_sizze_9501;\n    int32_t group_tid_9500;\n    \n    global_tid_9498 = get_global_id(0);\n    local_tid_9499 = get_local_id(0);\n    group_sizze_9502 = get_local_size(0);\n    wave_sizze_9501 = LOCKSTEP_WIDTH;\n  ",
                   "  group_tid_9500 = get_group_id(0);\n    \n    int32_t phys_tid_9163 = global_tid_9498;\n    __local char *scan_arr_mem_9503;\n    \n    scan_arr_mem_9503 = (__local char *) scan_arr_mem_9503_backing_0;\n    \n    __local char *scan_arr_mem_9505;\n    \n    scan_arr_mem_9505 = (__local char *) scan_arr_mem_9505_backing_1;\n    \n    int32_t flat_idx_9507 = sub32(mul32(add32(local_tid_9499, 1),\n                                        mul32(segscan_group_sizze_9158,\n                                              squot32(sub32(add32(n_8851,\n                                                                  num_threads_9445),\n                                                            1),\n                                                      num_threads_9445))), 1);\n    int32_t gtid_9162 = flat_idx_9507;\n    \n    // threads in bound read carries; others get neutral element\n    {\n        if (slt32(gtid_9162, n_8851)) {\n            ((__local int32_t *) scan_arr_mem_9503)[local_tid_9499] = ((__global\n                                                                        int32_t *) mem_9276)[gtid_9162];\n            ((__local bool *) scan_arr_mem_9505)[local_tid_9499] = ((__global\n                                                                     bool *) mem_9278)[gtid_9162];\n        } else {\n            ((__local int32_t *) scan_arr_mem_9503)[local_tid_9499] = 0;\n            ((__local bool *) scan_arr_mem_9505)[local_tid_9499] = 0;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t x_9480;\n    bool x_9481;\n    int32_t x_9482;\n    bool x_9483;\n    int32_t x_9508;\n    bool x_9509;\n    int32_t x_9510;\n    bool x_9511;\n    int32_t skip_threads_9517;\n    \n    if (slt32(local_tid_9499, num_groups_9160)) {\n        x_9482 = ((volatile __local\n                   int32_t *) scan_arr_mem_9503)[local_tid_9499];\n        x_9483 = ((volatile __local bool *) scan_arr_mem_9505)[local_tid_9499];\n        if (sub32(local_tid_9499, mul32(squot32(local_tid_9499, 32), 32)) ==\n            0)",
                   " {\n            x_9480 = x_9482;\n            x_9481 = x_9483;\n        }\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        skip_threads_9517 = 1;\n        while (slt32(skip_threads_9517, 32)) {\n            if (sle32(skip_threads_9517, sub32(local_tid_9499,\n                                               mul32(squot32(local_tid_9499,\n                                                             32), 32))) &&\n                slt32(local_tid_9499, num_groups_9160)) {\n                // read operands\n                {\n                    x_9480 = ((volatile __local\n                               int32_t *) scan_arr_mem_9503)[sub32(local_tid_9499,\n                                                                   skip_threads_9517)];\n                    x_9481 = ((volatile __local\n                               bool *) scan_arr_mem_9505)[sub32(local_tid_9499,\n                                                                skip_threads_9517)];\n                }\n                // perform operation\n                {\n                    int32_t res_9484;\n                    bool res_9485;\n                    \n                    if (x_9483) {\n                        bool res_9486 = x_9481 || x_9483;\n                        \n                        res_9484 = x_9482;\n                        res_9485 = res_9486;\n                    } else {\n                        int32_t res_9487 = add32(x_9480, x_9482);\n                        bool res_9488 = x_9481 || x_9483;\n                        \n                        res_9484 = res_9487;\n                        res_9485 = res_9488;\n                    }\n                    x_9480 = res_9484;\n                    x_9481 = res_9485;\n                }\n            }\n            if (sle32(wave_sizze_9501, skip_threads_9517)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            if (sle32(skip_threads_9517, sub32(local_tid_9499,\n                                               mul32(squot32(local_tid_9499",
                   ",\n                                                             32), 32))) &&\n                slt32(local_tid_9499, num_groups_9160)) {\n                // write result\n                {\n                    ((volatile __local\n                      int32_t *) scan_arr_mem_9503)[local_tid_9499] = x_9480;\n                    x_9482 = x_9480;\n                    ((volatile __local\n                      bool *) scan_arr_mem_9505)[local_tid_9499] = x_9481;\n                    x_9483 = x_9481;\n                }\n            }\n            if (sle32(wave_sizze_9501, skip_threads_9517)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_9517 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if (sub32(local_tid_9499, mul32(squot32(local_tid_9499, 32), 32)) ==\n            31 && slt32(local_tid_9499, num_groups_9160)) {\n            ((volatile __local\n              int32_t *) scan_arr_mem_9503)[squot32(local_tid_9499, 32)] =\n                x_9480;\n            ((volatile __local\n              bool *) scan_arr_mem_9505)[squot32(local_tid_9499, 32)] = x_9481;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for block 'i+1'\n    {\n        int32_t skip_threads_9518;\n        \n        if (squot32(local_tid_9499, 32) == 0 && slt32(local_tid_9499,\n                                                      num_groups_9160)) {\n            x_9510 = ((volatile __local\n                       int32_t *) scan_arr_mem_9503)[local_tid_9499];\n            x_9511 = ((volatile __local\n                       bool *) scan_arr_mem_9505)[local_tid_9499];\n            if (sub32(local_tid_9499, mul32(squot32(local_tid_9499, 32), 32)) ==\n                0) {\n                x_9508 = x_9510;\n                x_9509 = x_9511;\n            }\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            skip_t",
                   "hreads_9518 = 1;\n            while (slt32(skip_threads_9518, 32)) {\n                if (sle32(skip_threads_9518, sub32(local_tid_9499,\n                                                   mul32(squot32(local_tid_9499,\n                                                                 32), 32))) &&\n                    (squot32(local_tid_9499, 32) == 0 && slt32(local_tid_9499,\n                                                               num_groups_9160))) {\n                    // read operands\n                    {\n                        x_9508 = ((volatile __local\n                                   int32_t *) scan_arr_mem_9503)[sub32(local_tid_9499,\n                                                                       skip_threads_9518)];\n                        x_9509 = ((volatile __local\n                                   bool *) scan_arr_mem_9505)[sub32(local_tid_9499,\n                                                                    skip_threads_9518)];\n                    }\n                    // perform operation\n                    {\n                        int32_t res_9512;\n                        bool res_9513;\n                        \n                        if (x_9511) {\n                            bool res_9514 = x_9509 || x_9511;\n                            \n                            res_9512 = x_9510;\n                            res_9513 = res_9514;\n                        } else {\n                            int32_t res_9515 = add32(x_9508, x_9510);\n                            bool res_9516 = x_9509 || x_9511;\n                            \n                            res_9512 = res_9515;\n                            res_9513 = res_9516;\n                        }\n                        x_9508 = res_9512;\n                        x_9509 = res_9513;\n                    }\n                }\n                if (sle32(wave_sizze_9501, skip_threads_9518)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if (sle32(skip_thr",
                   "eads_9518, sub32(local_tid_9499,\n                                                   mul32(squot32(local_tid_9499,\n                                                                 32), 32))) &&\n                    (squot32(local_tid_9499, 32) == 0 && slt32(local_tid_9499,\n                                                               num_groups_9160))) {\n                    // write result\n                    {\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_9503)[local_tid_9499] =\n                            x_9508;\n                        x_9510 = x_9508;\n                        ((volatile __local\n                          bool *) scan_arr_mem_9505)[local_tid_9499] = x_9509;\n                        x_9511 = x_9509;\n                    }\n                }\n                if (sle32(wave_sizze_9501, skip_threads_9518)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_9518 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_9499, 32) == 0 || !slt32(local_tid_9499,\n                                                         num_groups_9160))) {\n            // read operands\n            {\n                x_9482 = x_9480;\n                x_9483 = x_9481;\n                x_9480 = ((__local\n                           int32_t *) scan_arr_mem_9503)[sub32(squot32(local_tid_9499,\n                                                                       32), 1)];\n                x_9481 = ((__local\n                           bool *) scan_arr_mem_9505)[sub32(squot32(local_tid_9499,\n                                                                    32), 1)];\n            }\n            // perform operation\n            {\n                int32_t res_9484;\n                bool res_9485;\n                \n                if (x_9483) {\n                    bool res_9486 = x_9481 || x_9483;\n          ",
                   "          \n                    res_9484 = x_9482;\n                    res_9485 = res_9486;\n                } else {\n                    int32_t res_9487 = add32(x_9480, x_9482);\n                    bool res_9488 = x_9481 || x_9483;\n                    \n                    res_9484 = res_9487;\n                    res_9485 = res_9488;\n                }\n                x_9480 = res_9484;\n                x_9481 = res_9485;\n            }\n            // write final result\n            {\n                ((__local int32_t *) scan_arr_mem_9503)[local_tid_9499] =\n                    x_9480;\n                ((__local bool *) scan_arr_mem_9505)[local_tid_9499] = x_9481;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(local_tid_9499, 32) == 0) {\n            ((__local int32_t *) scan_arr_mem_9503)[local_tid_9499] = x_9482;\n            ((__local bool *) scan_arr_mem_9505)[local_tid_9499] = x_9483;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // threads in bounds write scanned carries\n    {\n        if (slt32(gtid_9162, n_8851)) {\n            ((__global int32_t *) mem_9276)[gtid_9162] = ((__local\n                                                           int32_t *) scan_arr_mem_9503)[local_tid_9499];\n            ((__global bool *) mem_9278)[gtid_9162] = ((__local\n                                                        bool *) scan_arr_mem_9505)[local_tid_9499];\n        }\n    }\n    \n  error_0:\n    return;\n    #undef segscan_group_sizze_9158\n}\n__kernel void scan_stage2_9172(__global int *global_failure, __local volatile\n                               int64_t *scan_arr_mem_9608_backing_aligned_0,\n                               __local volatile\n                               int64_t *scan_arr_mem_9606_backing_aligned_1,\n                               __local volatile\n                               int64_t *scan_arr_mem_9604_backing_aligned_2,\n                               int32_t n_8851, int",
                   "32_t num_groups_9169, __global\n                               unsigned char *mem_9282, __global\n                               unsigned char *mem_9285, __global\n                               unsigned char *mem_9287,\n                               int32_t num_threads_9532)\n{\n    #define segscan_group_sizze_9167 (mainzisegscan_group_sizze_9166)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict scan_arr_mem_9608_backing_2 =\n                          (__local volatile\n                           char *) scan_arr_mem_9608_backing_aligned_0;\n    __local volatile char *restrict scan_arr_mem_9606_backing_1 =\n                          (__local volatile\n                           char *) scan_arr_mem_9606_backing_aligned_1;\n    __local volatile char *restrict scan_arr_mem_9604_backing_0 =\n                          (__local volatile\n                           char *) scan_arr_mem_9604_backing_aligned_2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9599;\n    int32_t local_tid_9600;\n    int32_t group_sizze_9603;\n    int32_t wave_sizze_9602;\n    int32_t group_tid_9601;\n    \n    global_tid_9599 = get_global_id(0);\n    local_tid_9600 = get_local_id(0);\n    group_sizze_9603 = get_local_size(0);\n    wave_sizze_9602 = LOCKSTEP_WIDTH;\n    group_tid_9601 = get_group_id(0);\n    \n    int32_t phys_tid_9172 = global_tid_9599;\n    __local char *scan_arr_mem_9604;\n    \n    scan_arr_mem_9604 = (__local char *) scan_arr_mem_9604_backing_0;\n    \n    __local char *scan_arr_mem_9606;\n    \n    scan_arr_mem_9606 = (__local char *) scan_arr_mem_9606_backing_1;\n    \n    __local char *scan_arr_mem_9608;\n    \n    scan_arr_mem_9608 = (__local char *) scan_arr_mem_9608_backing_2;\n    \n    int32_t flat_idx_9610 = sub32(mul32(add32(local_tid_9600, 1),\n                                        mul32(segscan_group_sizze_9167,\n                                              squot32(sub32(add32(n_8851,",
                   "\n                                                                  num_threads_9532),\n                                                            1),\n                                                      num_threads_9532))), 1);\n    int32_t gtid_9171 = flat_idx_9610;\n    \n    // threads in bound read carries; others get neutral element\n    {\n        if (slt32(gtid_9171, n_8851)) {\n            ((__local int32_t *) scan_arr_mem_9604)[local_tid_9600] = ((__global\n                                                                        int32_t *) mem_9282)[gtid_9171];\n            ((__local int32_t *) scan_arr_mem_9606)[local_tid_9600] = ((__global\n                                                                        int32_t *) mem_9285)[gtid_9171];\n            ((__local bool *) scan_arr_mem_9608)[local_tid_9600] = ((__global\n                                                                     bool *) mem_9287)[gtid_9171];\n        } else {\n            ((__local int32_t *) scan_arr_mem_9604)[local_tid_9600] = 0;\n            ((__local int32_t *) scan_arr_mem_9606)[local_tid_9600] = 0;\n            ((__local bool *) scan_arr_mem_9608)[local_tid_9600] = 0;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t x_9575;\n    int32_t x_9576;\n    bool x_9577;\n    int32_t x_9578;\n    int32_t x_9579;\n    bool x_9580;\n    int32_t x_9611;\n    int32_t x_9612;\n    bool x_9613;\n    int32_t x_9614;\n    int32_t x_9615;\n    bool x_9616;\n    int32_t skip_threads_9623;\n    \n    if (slt32(local_tid_9600, num_groups_9169)) {\n        x_9578 = ((volatile __local\n                   int32_t *) scan_arr_mem_9604)[local_tid_9600];\n        x_9579 = ((volatile __local\n                   int32_t *) scan_arr_mem_9606)[local_tid_9600];\n        x_9580 = ((volatile __local bool *) scan_arr_mem_9608)[local_tid_9600];\n        if (sub32(local_tid_9600, mul32(squot32(local_tid_9600, 32), 32)) ==\n            0) {\n            x_9575 = x_9578;\n            x_9576 = x_9579;\n            x_9577 = x_9580;\n",
                   "        }\n    }\n    // in-block scan (hopefully no barriers needed)\n    {\n        skip_threads_9623 = 1;\n        while (slt32(skip_threads_9623, 32)) {\n            if (sle32(skip_threads_9623, sub32(local_tid_9600,\n                                               mul32(squot32(local_tid_9600,\n                                                             32), 32))) &&\n                slt32(local_tid_9600, num_groups_9169)) {\n                // read operands\n                {\n                    x_9575 = ((volatile __local\n                               int32_t *) scan_arr_mem_9604)[sub32(local_tid_9600,\n                                                                   skip_threads_9623)];\n                    x_9576 = ((volatile __local\n                               int32_t *) scan_arr_mem_9606)[sub32(local_tid_9600,\n                                                                   skip_threads_9623)];\n                    x_9577 = ((volatile __local\n                               bool *) scan_arr_mem_9608)[sub32(local_tid_9600,\n                                                                skip_threads_9623)];\n                }\n                // perform operation\n                {\n                    int32_t res_9581 = add32(x_9575, x_9578);\n                    int32_t res_9582;\n                    bool res_9583;\n                    \n                    if (x_9580) {\n                        bool res_9584 = x_9577 || x_9580;\n                        \n                        res_9582 = x_9579;\n                        res_9583 = res_9584;\n                    } else {\n                        int32_t res_9585 = add32(x_9576, x_9579);\n                        bool res_9586 = x_9577 || x_9580;\n                        \n                        res_9582 = res_9585;\n                        res_9583 = res_9586;\n                    }\n                    x_9575 = res_9581;\n                    x_9576 = res_9582;\n                    x_9577 = res_9583;\n                }\n            }\n",
                   "            if (sle32(wave_sizze_9602, skip_threads_9623)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            if (sle32(skip_threads_9623, sub32(local_tid_9600,\n                                               mul32(squot32(local_tid_9600,\n                                                             32), 32))) &&\n                slt32(local_tid_9600, num_groups_9169)) {\n                // write result\n                {\n                    ((volatile __local\n                      int32_t *) scan_arr_mem_9604)[local_tid_9600] = x_9575;\n                    x_9578 = x_9575;\n                    ((volatile __local\n                      int32_t *) scan_arr_mem_9606)[local_tid_9600] = x_9576;\n                    x_9579 = x_9576;\n                    ((volatile __local\n                      bool *) scan_arr_mem_9608)[local_tid_9600] = x_9577;\n                    x_9580 = x_9577;\n                }\n            }\n            if (sle32(wave_sizze_9602, skip_threads_9623)) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n            }\n            skip_threads_9623 *= 2;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // last thread of block 'i' writes its result to offset 'i'\n    {\n        if (sub32(local_tid_9600, mul32(squot32(local_tid_9600, 32), 32)) ==\n            31 && slt32(local_tid_9600, num_groups_9169)) {\n            ((volatile __local\n              int32_t *) scan_arr_mem_9604)[squot32(local_tid_9600, 32)] =\n                x_9575;\n            ((volatile __local\n              int32_t *) scan_arr_mem_9606)[squot32(local_tid_9600, 32)] =\n                x_9576;\n            ((volatile __local\n              bool *) scan_arr_mem_9608)[squot32(local_tid_9600, 32)] = x_9577;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // scan the first block, after which offset 'i' contains carry-in for block 'i+1'\n    {\n        int32_t skip_threads_9624;\n        \n        if (squot32(local_tid_9600, 32) == 0 && slt32(local_tid_9600,\n                        ",
                   "                              num_groups_9169)) {\n            x_9614 = ((volatile __local\n                       int32_t *) scan_arr_mem_9604)[local_tid_9600];\n            x_9615 = ((volatile __local\n                       int32_t *) scan_arr_mem_9606)[local_tid_9600];\n            x_9616 = ((volatile __local\n                       bool *) scan_arr_mem_9608)[local_tid_9600];\n            if (sub32(local_tid_9600, mul32(squot32(local_tid_9600, 32), 32)) ==\n                0) {\n                x_9611 = x_9614;\n                x_9612 = x_9615;\n                x_9613 = x_9616;\n            }\n        }\n        // in-block scan (hopefully no barriers needed)\n        {\n            skip_threads_9624 = 1;\n            while (slt32(skip_threads_9624, 32)) {\n                if (sle32(skip_threads_9624, sub32(local_tid_9600,\n                                                   mul32(squot32(local_tid_9600,\n                                                                 32), 32))) &&\n                    (squot32(local_tid_9600, 32) == 0 && slt32(local_tid_9600,\n                                                               num_groups_9169))) {\n                    // read operands\n                    {\n                        x_9611 = ((volatile __local\n                                   int32_t *) scan_arr_mem_9604)[sub32(local_tid_9600,\n                                                                       skip_threads_9624)];\n                        x_9612 = ((volatile __local\n                                   int32_t *) scan_arr_mem_9606)[sub32(local_tid_9600,\n                                                                       skip_threads_9624)];\n                        x_9613 = ((volatile __local\n                                   bool *) scan_arr_mem_9608)[sub32(local_tid_9600,\n                                                                    skip_threads_9624)];\n                    }\n                    // perform operation\n                    {\n                        i",
                   "nt32_t res_9617 = add32(x_9611, x_9614);\n                        int32_t res_9618;\n                        bool res_9619;\n                        \n                        if (x_9616) {\n                            bool res_9620 = x_9613 || x_9616;\n                            \n                            res_9618 = x_9615;\n                            res_9619 = res_9620;\n                        } else {\n                            int32_t res_9621 = add32(x_9612, x_9615);\n                            bool res_9622 = x_9613 || x_9616;\n                            \n                            res_9618 = res_9621;\n                            res_9619 = res_9622;\n                        }\n                        x_9611 = res_9617;\n                        x_9612 = res_9618;\n                        x_9613 = res_9619;\n                    }\n                }\n                if (sle32(wave_sizze_9602, skip_threads_9624)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                if (sle32(skip_threads_9624, sub32(local_tid_9600,\n                                                   mul32(squot32(local_tid_9600,\n                                                                 32), 32))) &&\n                    (squot32(local_tid_9600, 32) == 0 && slt32(local_tid_9600,\n                                                               num_groups_9169))) {\n                    // write result\n                    {\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_9604)[local_tid_9600] =\n                            x_9611;\n                        x_9614 = x_9611;\n                        ((volatile __local\n                          int32_t *) scan_arr_mem_9606)[local_tid_9600] =\n                            x_9612;\n                        x_9615 = x_9612;\n                        ((volatile __local\n                          bool *) scan_arr_mem_9608)[local_tid_9600] = x_9613;\n                        x_9616 = x_9613;\n         ",
                   "           }\n                }\n                if (sle32(wave_sizze_9602, skip_threads_9624)) {\n                    barrier(CLK_LOCAL_MEM_FENCE);\n                }\n                skip_threads_9624 *= 2;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // carry-in for every block except the first\n    {\n        if (!(squot32(local_tid_9600, 32) == 0 || !slt32(local_tid_9600,\n                                                         num_groups_9169))) {\n            // read operands\n            {\n                x_9578 = x_9575;\n                x_9579 = x_9576;\n                x_9580 = x_9577;\n                x_9575 = ((__local\n                           int32_t *) scan_arr_mem_9604)[sub32(squot32(local_tid_9600,\n                                                                       32), 1)];\n                x_9576 = ((__local\n                           int32_t *) scan_arr_mem_9606)[sub32(squot32(local_tid_9600,\n                                                                       32), 1)];\n                x_9577 = ((__local\n                           bool *) scan_arr_mem_9608)[sub32(squot32(local_tid_9600,\n                                                                    32), 1)];\n            }\n            // perform operation\n            {\n                int32_t res_9581 = add32(x_9575, x_9578);\n                int32_t res_9582;\n                bool res_9583;\n                \n                if (x_9580) {\n                    bool res_9584 = x_9577 || x_9580;\n                    \n                    res_9582 = x_9579;\n                    res_9583 = res_9584;\n                } else {\n                    int32_t res_9585 = add32(x_9576, x_9579);\n                    bool res_9586 = x_9577 || x_9580;\n                    \n                    res_9582 = res_9585;\n                    res_9583 = res_9586;\n                }\n                x_9575 = res_9581;\n                x_9576 = res_9582;\n                x_9577 = res_9583;\n            }\n            //",
                   " write final result\n            {\n                ((__local int32_t *) scan_arr_mem_9604)[local_tid_9600] =\n                    x_9575;\n                ((__local int32_t *) scan_arr_mem_9606)[local_tid_9600] =\n                    x_9576;\n                ((__local bool *) scan_arr_mem_9608)[local_tid_9600] = x_9577;\n            }\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // restore correct values for first block\n    {\n        if (squot32(local_tid_9600, 32) == 0) {\n            ((__local int32_t *) scan_arr_mem_9604)[local_tid_9600] = x_9578;\n            ((__local int32_t *) scan_arr_mem_9606)[local_tid_9600] = x_9579;\n            ((__local bool *) scan_arr_mem_9608)[local_tid_9600] = x_9580;\n        }\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // threads in bounds write scanned carries\n    {\n        if (slt32(gtid_9171, n_8851)) {\n            ((__global int32_t *) mem_9282)[gtid_9171] = ((__local\n                                                           int32_t *) scan_arr_mem_9604)[local_tid_9600];\n            ((__global int32_t *) mem_9285)[gtid_9171] = ((__local\n                                                           int32_t *) scan_arr_mem_9606)[local_tid_9600];\n            ((__global bool *) mem_9287)[gtid_9171] = ((__local\n                                                        bool *) scan_arr_mem_9608)[local_tid_9600];\n        }\n    }\n    \n  error_0:\n    return;\n    #undef segscan_group_sizze_9167\n}\n__kernel void scan_stage3_9114(__global int *global_failure, int32_t n_8851,\n                               int32_t num_groups_9111, __global\n                               unsigned char *mem_9236, __global\n                               unsigned char *mem_9239,\n                               int32_t num_threads_9337,\n                               int32_t required_groups_9396)\n{\n    #define segscan_group_sizze_9109 (mainzisegscan_group_sizze_9108)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    \n",
                   "    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9397;\n    int32_t local_tid_9398;\n    int32_t group_sizze_9401;\n    int32_t wave_sizze_9400;\n    int32_t group_tid_9399;\n    \n    global_tid_9397 = get_global_id(0);\n    local_tid_9398 = get_local_id(0);\n    group_sizze_9401 = get_local_size(0);\n    wave_sizze_9400 = LOCKSTEP_WIDTH;\n    group_tid_9399 = get_group_id(0);\n    \n    int32_t phys_tid_9114 = global_tid_9397;\n    int32_t phys_group_id_9402;\n    \n    phys_group_id_9402 = get_group_id(0);\n    for (int32_t i_9403 = 0; i_9403 <\n         squot32(sub32(add32(sub32(required_groups_9396, phys_group_id_9402),\n                             num_groups_9111), 1), num_groups_9111); i_9403++) {\n        int32_t virt_group_id_9404 = add32(phys_group_id_9402, mul32(i_9403,\n                                                                     num_groups_9111));\n        int32_t flat_idx_9405 = add32(mul32(virt_group_id_9404,\n                                            segscan_group_sizze_9109),\n                                      local_tid_9398);\n        int32_t gtid_9113 = flat_idx_9405;\n        int32_t orig_group_9406 = squot32(flat_idx_9405,\n                                          mul32(segscan_group_sizze_9109,\n                                                squot32(sub32(add32(n_8851,\n                                                                    num_threads_9337),\n                                                              1),\n                                                        num_threads_9337)));\n        int32_t carry_in_flat_idx_9407 = sub32(mul32(orig_group_9406,\n                                                     mul32(segscan_group_sizze_9109,\n                                                           squot32(sub32(add32(n_8851,\n                                                                               num_threads_9337),\n                                                                         1),\n                          ",
                   "                                         num_threads_9337))),\n                                               1);\n        \n        if (slt32(gtid_9113, n_8851)) {\n            if (!(orig_group_9406 == 0 || flat_idx_9405 ==\n                  sub32(mul32(add32(orig_group_9406, 1),\n                              mul32(segscan_group_sizze_9109,\n                                    squot32(sub32(add32(n_8851,\n                                                        num_threads_9337), 1),\n                                            num_threads_9337))), 1))) {\n                int32_t x_9372;\n                int32_t x_9373;\n                int32_t x_9374;\n                int32_t x_9375;\n                \n                x_9372 = ((__global\n                           int32_t *) mem_9236)[carry_in_flat_idx_9407];\n                x_9373 = ((__global\n                           int32_t *) mem_9239)[carry_in_flat_idx_9407];\n                x_9374 = ((__global int32_t *) mem_9236)[gtid_9113];\n                x_9375 = ((__global int32_t *) mem_9239)[gtid_9113];\n                \n                int32_t res_9376 = add32(x_9372, x_9374);\n                int32_t res_9377 = add32(x_9373, x_9375);\n                \n                x_9372 = res_9376;\n                x_9373 = res_9377;\n                ((__global int32_t *) mem_9236)[gtid_9113] = x_9372;\n                ((__global int32_t *) mem_9239)[gtid_9113] = x_9373;\n            }\n        }\n        barrier(CLK_GLOBAL_MEM_FENCE);\n    }\n    \n  error_0:\n    return;\n    #undef segscan_group_sizze_9109\n}\n__kernel void scan_stage3_9163(__global int *global_failure, int32_t n_8851,\n                               int32_t num_groups_9160, __global\n                               unsigned char *mem_9276, __global\n                               unsigned char *mem_9278,\n                               int32_t num_threads_9445,\n                               int32_t required_groups_9519)\n{\n    #define segscan_group_sizze_9158 (mainzisegscan_group_sizze_9157",
                   ")\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9520;\n    int32_t local_tid_9521;\n    int32_t group_sizze_9524;\n    int32_t wave_sizze_9523;\n    int32_t group_tid_9522;\n    \n    global_tid_9520 = get_global_id(0);\n    local_tid_9521 = get_local_id(0);\n    group_sizze_9524 = get_local_size(0);\n    wave_sizze_9523 = LOCKSTEP_WIDTH;\n    group_tid_9522 = get_group_id(0);\n    \n    int32_t phys_tid_9163 = global_tid_9520;\n    int32_t phys_group_id_9525;\n    \n    phys_group_id_9525 = get_group_id(0);\n    for (int32_t i_9526 = 0; i_9526 <\n         squot32(sub32(add32(sub32(required_groups_9519, phys_group_id_9525),\n                             num_groups_9160), 1), num_groups_9160); i_9526++) {\n        int32_t virt_group_id_9527 = add32(phys_group_id_9525, mul32(i_9526,\n                                                                     num_groups_9160));\n        int32_t flat_idx_9528 = add32(mul32(virt_group_id_9527,\n                                            segscan_group_sizze_9158),\n                                      local_tid_9521);\n        int32_t gtid_9162 = flat_idx_9528;\n        int32_t orig_group_9529 = squot32(flat_idx_9528,\n                                          mul32(segscan_group_sizze_9158,\n                                                squot32(sub32(add32(n_8851,\n                                                                    num_threads_9445),\n                                                              1),\n                                                        num_threads_9445)));\n        int32_t carry_in_flat_idx_9530 = sub32(mul32(orig_group_9529,\n                                                     mul32(segscan_group_sizze_9158,\n                                                           squot32(sub32(add32(n_8851,\n                                                                               num_threads_9445),\n ",
                   "                                                                        1),\n                                                                   num_threads_9445))),\n                                               1);\n        \n        if (slt32(gtid_9162, n_8851)) {\n            if (!(orig_group_9529 == 0 || flat_idx_9528 ==\n                  sub32(mul32(add32(orig_group_9529, 1),\n                              mul32(segscan_group_sizze_9158,\n                                    squot32(sub32(add32(n_8851,\n                                                        num_threads_9445), 1),\n                                            num_threads_9445))), 1))) {\n                int32_t x_9489;\n                bool x_9490;\n                int32_t x_9491;\n                bool x_9492;\n                \n                x_9489 = ((__global\n                           int32_t *) mem_9276)[carry_in_flat_idx_9530];\n                x_9490 = ((__global bool *) mem_9278)[carry_in_flat_idx_9530];\n                x_9491 = ((__global int32_t *) mem_9276)[gtid_9162];\n                x_9492 = ((__global bool *) mem_9278)[gtid_9162];\n                \n                int32_t res_9493;\n                bool res_9494;\n                \n                if (x_9492) {\n                    bool res_9495 = x_9490 || x_9492;\n                    \n                    res_9493 = x_9491;\n                    res_9494 = res_9495;\n                } else {\n                    int32_t res_9496 = add32(x_9489, x_9491);\n                    bool res_9497 = x_9490 || x_9492;\n                    \n                    res_9493 = res_9496;\n                    res_9494 = res_9497;\n                }\n                x_9489 = res_9493;\n                x_9490 = res_9494;\n                ((__global int32_t *) mem_9276)[gtid_9162] = x_9489;\n                ((__global bool *) mem_9278)[gtid_9162] = x_9490;\n            }\n        }\n        barrier(CLK_GLOBAL_MEM_FENCE);\n    }\n    \n  error_0:\n    return;\n    #undef segscan_group_sizze_91",
                   "58\n}\n__kernel void scan_stage3_9172(__global int *global_failure, int32_t n_8851,\n                               int32_t num_groups_9169, __global\n                               unsigned char *mem_9282, __global\n                               unsigned char *mem_9285, __global\n                               unsigned char *mem_9287,\n                               int32_t num_threads_9532,\n                               int32_t required_groups_9625)\n{\n    #define segscan_group_sizze_9167 (mainzisegscan_group_sizze_9166)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9626;\n    int32_t local_tid_9627;\n    int32_t group_sizze_9630;\n    int32_t wave_sizze_9629;\n    int32_t group_tid_9628;\n    \n    global_tid_9626 = get_global_id(0);\n    local_tid_9627 = get_local_id(0);\n    group_sizze_9630 = get_local_size(0);\n    wave_sizze_9629 = LOCKSTEP_WIDTH;\n    group_tid_9628 = get_group_id(0);\n    \n    int32_t phys_tid_9172 = global_tid_9626;\n    int32_t phys_group_id_9631;\n    \n    phys_group_id_9631 = get_group_id(0);\n    for (int32_t i_9632 = 0; i_9632 <\n         squot32(sub32(add32(sub32(required_groups_9625, phys_group_id_9631),\n                             num_groups_9169), 1), num_groups_9169); i_9632++) {\n        int32_t virt_group_id_9633 = add32(phys_group_id_9631, mul32(i_9632,\n                                                                     num_groups_9169));\n        int32_t flat_idx_9634 = add32(mul32(virt_group_id_9633,\n                                            segscan_group_sizze_9167),\n                                      local_tid_9627);\n        int32_t gtid_9171 = flat_idx_9634;\n        int32_t orig_group_9635 = squot32(flat_idx_9634,\n                                          mul32(segscan_group_sizze_9167,\n                                                squot32(sub32(add32(n_8851,\n                                                  ",
                   "                  num_threads_9532),\n                                                              1),\n                                                        num_threads_9532)));\n        int32_t carry_in_flat_idx_9636 = sub32(mul32(orig_group_9635,\n                                                     mul32(segscan_group_sizze_9167,\n                                                           squot32(sub32(add32(n_8851,\n                                                                               num_threads_9532),\n                                                                         1),\n                                                                   num_threads_9532))),\n                                               1);\n        \n        if (slt32(gtid_9171, n_8851)) {\n            if (!(orig_group_9635 == 0 || flat_idx_9634 ==\n                  sub32(mul32(add32(orig_group_9635, 1),\n                              mul32(segscan_group_sizze_9167,\n                                    squot32(sub32(add32(n_8851,\n                                                        num_threads_9532), 1),\n                                            num_threads_9532))), 1))) {\n                int32_t x_9587;\n                int32_t x_9588;\n                bool x_9589;\n                int32_t x_9590;\n                int32_t x_9591;\n                bool x_9592;\n                \n                x_9587 = ((__global\n                           int32_t *) mem_9282)[carry_in_flat_idx_9636];\n                x_9588 = ((__global\n                           int32_t *) mem_9285)[carry_in_flat_idx_9636];\n                x_9589 = ((__global bool *) mem_9287)[carry_in_flat_idx_9636];\n                x_9590 = ((__global int32_t *) mem_9282)[gtid_9171];\n                x_9591 = ((__global int32_t *) mem_9285)[gtid_9171];\n                x_9592 = ((__global bool *) mem_9287)[gtid_9171];\n                \n                int32_t res_9593 = add32(x_9587, x_9590);\n                int32_t res_9594;\n         ",
                   "       bool res_9595;\n                \n                if (x_9592) {\n                    bool res_9596 = x_9589 || x_9592;\n                    \n                    res_9594 = x_9591;\n                    res_9595 = res_9596;\n                } else {\n                    int32_t res_9597 = add32(x_9588, x_9591);\n                    bool res_9598 = x_9589 || x_9592;\n                    \n                    res_9594 = res_9597;\n                    res_9595 = res_9598;\n                }\n                x_9587 = res_9593;\n                x_9588 = res_9594;\n                x_9589 = res_9595;\n                ((__global int32_t *) mem_9282)[gtid_9171] = x_9587;\n                ((__global int32_t *) mem_9285)[gtid_9171] = x_9588;\n                ((__global bool *) mem_9287)[gtid_9171] = x_9589;\n            }\n        }\n        barrier(CLK_GLOBAL_MEM_FENCE);\n    }\n    \n  error_0:\n    return;\n    #undef segscan_group_sizze_9167\n}\n__kernel void segmap_9092(__global int *global_failure, int32_t n_8851,\n                          int32_t i_8865, __global unsigned char *xs_mem_9228,\n                          __global unsigned char *mem_9232)\n{\n    #define segmap_group_sizze_9096 (mainzisegmap_group_sizze_9095)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9332;\n    int32_t local_tid_9333;\n    int32_t group_sizze_9336;\n    int32_t wave_sizze_9335;\n    int32_t group_tid_9334;\n    \n    global_tid_9332 = get_global_id(0);\n    local_tid_9333 = get_local_id(0);\n    group_sizze_9336 = get_local_size(0);\n    wave_sizze_9335 = LOCKSTEP_WIDTH;\n    group_tid_9334 = get_group_id(0);\n    \n    int32_t phys_tid_9092 = global_tid_9332;\n    int32_t gtid_9091 = add32(mul32(group_tid_9334, segmap_group_sizze_9096),\n                              local_tid_9333);\n    \n    if (slt32(gtid_9091, n_8851)) {\n        int32_t x_9103 = ((__global int32_t *) xs_mem_9228)[gtid_9091];\n     ",
                   "   int32_t res_9104 = ashr32(x_9103, i_8865);\n        int32_t res_9105 = 1 & res_9104;\n        \n        ((__global int32_t *) mem_9232)[gtid_9091] = res_9105;\n    }\n    \n  error_0:\n    return;\n    #undef segmap_group_sizze_9096\n}\n__kernel void segmap_9129(__global int *global_failure, int32_t n_8851,\n                          int32_t res_8897, __global unsigned char *xs_mem_9228,\n                          __global unsigned char *xs_mem_9229, __global\n                          unsigned char *mem_9232, __global\n                          unsigned char *mem_9236, __global\n                          unsigned char *mem_9239, __global\n                          unsigned char *mem_9242, __global\n                          unsigned char *mem_9251, __global\n                          unsigned char *mem_9256)\n{\n    #define segmap_group_sizze_9133 (mainzisegmap_group_sizze_9132)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9434;\n    int32_t local_tid_9435;\n    int32_t group_sizze_9438;\n    int32_t wave_sizze_9437;\n    int32_t group_tid_9436;\n    \n    global_tid_9434 = get_global_id(0);\n    local_tid_9435 = get_local_id(0);\n    group_sizze_9438 = get_local_size(0);\n    wave_sizze_9437 = LOCKSTEP_WIDTH;\n    group_tid_9436 = get_group_id(0);\n    \n    int32_t phys_tid_9129 = global_tid_9434;\n    int32_t write_i_9128 = add32(mul32(group_tid_9436, segmap_group_sizze_9133),\n                                 local_tid_9435);\n    \n    if (slt32(write_i_9128, n_8851)) {\n        int32_t x_8911 = ((__global int32_t *) mem_9242)[write_i_9128];\n        int32_t x_8912 = ((__global int32_t *) mem_9236)[write_i_9128];\n        int32_t x_8913 = ((__global int32_t *) mem_9239)[write_i_9128];\n        int32_t x_8914 = ((__global int32_t *) mem_9232)[write_i_9128];\n        int32_t write_value_8915 = ((__global\n                                     int32_t *) xs_mem_9228)[write_i_",
                   "9128];\n        int32_t write_value_8916 = ((__global\n                                     int32_t *) xs_mem_9229)[write_i_9128];\n        int32_t res_8917 = mul32(x_8911, x_8912);\n        int32_t res_8918 = add32(res_8897, x_8913);\n        int32_t res_8919 = mul32(x_8914, res_8918);\n        int32_t res_8920 = add32(res_8917, res_8919);\n        int32_t res_8921 = sub32(res_8920, 1);\n        \n        if (sle32(0, res_8921) && slt32(res_8921, n_8851)) {\n            ((__global int32_t *) mem_9256)[add32(mul32(0, n_8851), res_8921)] =\n                write_value_8915;\n        }\n        if (sle32(0, res_8921) && slt32(res_8921, n_8851)) {\n            ((__global int32_t *) mem_9251)[add32(mul32(0, n_8851), res_8921)] =\n                write_value_8916;\n        }\n    }\n    \n  error_0:\n    return;\n    #undef segmap_group_sizze_9133\n}\n__kernel void segmap_9141(__global int *global_failure, int32_t n_8851, __global\n                          unsigned char *k_mem_9223, __global\n                          unsigned char *d_mem_9224, __global\n                          unsigned char *res_mem_9266, __global\n                          unsigned char *mem_9269, __global\n                          unsigned char *mem_9272)\n{\n    #define segmap_group_sizze_9145 (mainzisegmap_group_sizze_9144)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9439;\n    int32_t local_tid_9440;\n    int32_t group_sizze_9443;\n    int32_t wave_sizze_9442;\n    int32_t group_tid_9441;\n    \n    global_tid_9439 = get_global_id(0);\n    local_tid_9440 = get_local_id(0);\n    group_sizze_9443 = get_local_size(0);\n    wave_sizze_9442 = LOCKSTEP_WIDTH;\n    group_tid_9441 = get_group_id(0);\n    \n    int32_t phys_tid_9141 = global_tid_9439;\n    int32_t gtid_9140 = add32(mul32(group_tid_9441, segmap_group_sizze_9145),\n                              local_tid_9440);\n    \n    if (slt32(gtid_9140, n_8851)) {\n  ",
                   "      int32_t x_9152 = ((__global int32_t *) res_mem_9266)[gtid_9140];\n        int32_t res_9153 = ((__global int32_t *) k_mem_9223)[x_9152];\n        int32_t res_9154 = ((__global int32_t *) d_mem_9224)[x_9152];\n        \n        ((__global int32_t *) mem_9269)[gtid_9140] = res_9153;\n        ((__global int32_t *) mem_9272)[gtid_9140] = res_9154;\n    }\n    \n  error_0:\n    return;\n    #undef segmap_group_sizze_9145\n}\n__kernel void segmap_9195(__global int *global_failure, int32_t n_8851, __global\n                          unsigned char *mem_9282, __global\n                          unsigned char *mem_9285, __global\n                          unsigned char *mem_9290)\n{\n    #define segmap_group_sizze_9199 (mainzisegmap_group_sizze_9198)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9637;\n    int32_t local_tid_9638;\n    int32_t group_sizze_9641;\n    int32_t wave_sizze_9640;\n    int32_t group_tid_9639;\n    \n    global_tid_9637 = get_global_id(0);\n    local_tid_9638 = get_local_id(0);\n    group_sizze_9641 = get_local_size(0);\n    wave_sizze_9640 = LOCKSTEP_WIDTH;\n    group_tid_9639 = get_group_id(0);\n    \n    int32_t phys_tid_9195 = global_tid_9637;\n    int32_t write_i_9194 = add32(mul32(group_tid_9639, segmap_group_sizze_9199),\n                                 local_tid_9638);\n    \n    if (slt32(write_i_9194, n_8851)) {\n        int32_t write_index_9074 = ((__global\n                                     int32_t *) mem_9282)[write_i_9194];\n        int32_t write_value_9075 = ((__global\n                                     int32_t *) mem_9285)[write_i_9194];\n        \n        if (sle32(0, write_index_9074) && slt32(write_index_9074, n_8851)) {\n            ((__global int32_t *) mem_9290)[write_index_9074] =\n                write_value_9075;\n        }\n    }\n    \n  error_0:\n    return;\n    #undef segmap_group_sizze_9199\n}\n__kernel void segred_nonseg_9127(__gl",
                   "obal int *global_failure, __local volatile\n                                 int64_t *red_arr_mem_9420_backing_aligned_0,\n                                 __local volatile\n                                 int64_t *sync_arr_mem_9418_backing_aligned_1,\n                                 int32_t n_8851, int32_t num_groups_9120,\n                                 __global unsigned char *xs_mem_9228, __global\n                                 unsigned char *xs_mem_9229, __global\n                                 unsigned char *mem_9242, __global\n                                 unsigned char *mem_9246, __global\n                                 unsigned char *mem_9251, __global\n                                 unsigned char *mem_9256, __global\n                                 unsigned char *counter_mem_9408, __global\n                                 unsigned char *group_res_arr_mem_9410,\n                                 int32_t num_threads_9412)\n{\n    #define segred_group_sizze_9118 (mainzisegred_group_sizze_9117)\n    \n    const int block_dim0 = 0;\n    const int block_dim1 = 1;\n    const int block_dim2 = 2;\n    __local volatile char *restrict red_arr_mem_9420_backing_1 =\n                          (__local volatile\n                           char *) red_arr_mem_9420_backing_aligned_0;\n    __local volatile char *restrict sync_arr_mem_9418_backing_0 =\n                          (__local volatile\n                           char *) sync_arr_mem_9418_backing_aligned_1;\n    \n    if (*global_failure >= 0)\n        return;\n    \n    int32_t global_tid_9413;\n    int32_t local_tid_9414;\n    int32_t group_sizze_9417;\n    int32_t wave_sizze_9416;\n    int32_t group_tid_9415;\n    \n    global_tid_9413 = get_global_id(0);\n    local_tid_9414 = get_local_id(0);\n    group_sizze_9417 = get_local_size(0);\n    wave_sizze_9416 = LOCKSTEP_WIDTH;\n    group_tid_9415 = get_group_id(0);\n    \n    int32_t phys_tid_9127 = global_tid_9413;\n    __local char *sync_arr_mem_9418;\n    \n    sync_arr_mem_9418 = (__local ",
                   "char *) sync_arr_mem_9418_backing_0;\n    \n    __local char *red_arr_mem_9420;\n    \n    red_arr_mem_9420 = (__local char *) red_arr_mem_9420_backing_1;\n    \n    int32_t dummy_9125 = 0;\n    int32_t gtid_9126;\n    \n    gtid_9126 = 0;\n    \n    int32_t x_acc_9422;\n    int32_t chunk_sizze_9423 = smin32(squot32(sub32(add32(n_8851,\n                                                          mul32(segred_group_sizze_9118,\n                                                                num_groups_9120)),\n                                                    1),\n                                              mul32(segred_group_sizze_9118,\n                                                    num_groups_9120)),\n                                      squot32(sub32(add32(sub32(n_8851,\n                                                                phys_tid_9127),\n                                                          num_threads_9412), 1),\n                                              num_threads_9412));\n    int32_t x_8900;\n    int32_t x_8901;\n    \n    // neutral-initialise the accumulators\n    {\n        x_acc_9422 = 0;\n    }\n    for (int32_t i_9427 = 0; i_9427 < chunk_sizze_9423; i_9427++) {\n        gtid_9126 = add32(phys_tid_9127, mul32(num_threads_9412, i_9427));\n        // apply map function\n        {\n            int32_t copy_p_8903 = ((__global int32_t *) xs_mem_9229)[gtid_9126];\n            int32_t x_8904 = ((__global int32_t *) mem_9242)[gtid_9126];\n            int32_t copy_p_8905 = ((__global int32_t *) xs_mem_9228)[gtid_9126];\n            \n            // save map-out results\n            {\n                ((__global int32_t *) mem_9251)[add32(mul32(dummy_9125, n_8851),\n                                                      gtid_9126)] = copy_p_8903;\n                ((__global int32_t *) mem_9256)[add32(mul32(dummy_9125, n_8851),\n                                                      gtid_9126)] = copy_p_8905;\n            }\n            // load accumulator\n            {\n          ",
                   "      x_8900 = x_acc_9422;\n            }\n            // load new values\n            {\n                x_8901 = x_8904;\n            }\n            // apply reduction operator\n            {\n                int32_t res_8902 = add32(x_8900, x_8901);\n                \n                // store in accumulator\n                {\n                    x_acc_9422 = res_8902;\n                }\n            }\n        }\n    }\n    // to reduce current chunk, first store our result in memory\n    {\n        x_8900 = x_acc_9422;\n        ((__local int32_t *) red_arr_mem_9420)[local_tid_9414] = x_8900;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    \n    int32_t offset_9428;\n    int32_t skip_waves_9429;\n    int32_t x_9424;\n    int32_t x_9425;\n    \n    offset_9428 = 0;\n    // participating threads read initial accumulator\n    {\n        if (slt32(local_tid_9414, segred_group_sizze_9118)) {\n            x_9424 = ((__local\n                       int32_t *) red_arr_mem_9420)[add32(local_tid_9414,\n                                                          offset_9428)];\n        }\n    }\n    offset_9428 = 1;\n    while (slt32(offset_9428, wave_sizze_9416)) {\n        if (slt32(add32(local_tid_9414, offset_9428),\n                  segred_group_sizze_9118) && (sub32(local_tid_9414,\n                                                     mul32(squot32(local_tid_9414,\n                                                                   wave_sizze_9416),\n                                                           wave_sizze_9416)) &\n                                               sub32(mul32(2, offset_9428),\n                                                     1)) == 0) {\n            // read array element\n            {\n                x_9425 = ((volatile __local\n                           int32_t *) red_arr_mem_9420)[add32(local_tid_9414,\n                                                              offset_9428)];\n            }\n            // apply reduction operation\n            {\n                int32_t res_9426 = ad",
                   "d32(x_9424, x_9425);\n                \n                x_9424 = res_9426;\n            }\n            // write result of operation\n            {\n                ((volatile __local\n                  int32_t *) red_arr_mem_9420)[local_tid_9414] = x_9424;\n            }\n        }\n        offset_9428 *= 2;\n    }\n    skip_waves_9429 = 1;\n    while (slt32(skip_waves_9429, squot32(sub32(add32(segred_group_sizze_9118,\n                                                      wave_sizze_9416), 1),\n                                          wave_sizze_9416))) {\n        barrier(CLK_LOCAL_MEM_FENCE);\n        offset_9428 = mul32(skip_waves_9429, wave_sizze_9416);\n        if (slt32(add32(local_tid_9414, offset_9428),\n                  segred_group_sizze_9118) && (sub32(local_tid_9414,\n                                                     mul32(squot32(local_tid_9414,\n                                                                   wave_sizze_9416),\n                                                           wave_sizze_9416)) ==\n                                               0 && (squot32(local_tid_9414,\n                                                             wave_sizze_9416) &\n                                                     sub32(mul32(2,\n                                                                 skip_waves_9429),\n                                                           1)) == 0)) {\n            // read array element\n            {\n                x_9425 = ((__local\n                           int32_t *) red_arr_mem_9420)[add32(local_tid_9414,\n                                                              offset_9428)];\n            }\n            // apply reduction operation\n            {\n                int32_t res_9426 = add32(x_9424, x_9425);\n                \n                x_9424 = res_9426;\n            }\n            // write result of operation\n            {\n                ((__local int32_t *) red_arr_mem_9420)[local_tid_9414] = x_9424;\n            }\n        }\n        ",
                   "skip_waves_9429 *= 2;\n    }\n    barrier(CLK_LOCAL_MEM_FENCE);\n    // first thread saves the result in accumulator\n    {\n        if (local_tid_9414 == 0) {\n            x_acc_9422 = x_9424;\n        }\n    }\n    \n    int32_t old_counter_9430;\n    \n    // first thread in group saves group result to global memory\n    {\n        if (local_tid_9414 == 0) {\n            ((__global int32_t *) group_res_arr_mem_9410)[mul32(group_tid_9415,\n                                                                segred_group_sizze_9118)] =\n                x_acc_9422;\n            mem_fence_global();\n            old_counter_9430 = atomic_add_i32_global(&((volatile __global\n                                                        int *) counter_mem_9408)[0],\n                                                     (int) 1);\n            ((__local bool *) sync_arr_mem_9418)[0] = old_counter_9430 ==\n                sub32(num_groups_9120, 1);\n        }\n    }\n    barrier(CLK_GLOBAL_MEM_FENCE);\n    \n    bool is_last_group_9431 = ((__local bool *) sync_arr_mem_9418)[0];\n    \n    if (is_last_group_9431) {\n        if (local_tid_9414 == 0) {\n            old_counter_9430 = atomic_add_i32_global(&((volatile __global\n                                                        int *) counter_mem_9408)[0],\n                                                     (int) sub32(0,\n                                                                 num_groups_9120));\n        }\n        // read in the per-group-results\n        {\n            if (slt32(local_tid_9414, num_groups_9120)) {\n                x_8900 = ((__global\n                           int32_t *) group_res_arr_mem_9410)[mul32(local_tid_9414,\n                                                                    segred_group_sizze_9118)];\n            } else {\n                x_8900 = 0;\n            }\n            ((__local int32_t *) red_arr_mem_9420)[local_tid_9414] = x_8900;\n        }\n        barrier(CLK_LOCAL_MEM_FENCE);\n        // reduce the per-group results\n        {",
                   "\n            int32_t offset_9432;\n            int32_t skip_waves_9433;\n            int32_t x_9424;\n            int32_t x_9425;\n            \n            offset_9432 = 0;\n            // participating threads read initial accumulator\n            {\n                if (slt32(local_tid_9414, segred_group_sizze_9118)) {\n                    x_9424 = ((__local\n                               int32_t *) red_arr_mem_9420)[add32(local_tid_9414,\n                                                                  offset_9432)];\n                }\n            }\n            offset_9432 = 1;\n            while (slt32(offset_9432, wave_sizze_9416)) {\n                if (slt32(add32(local_tid_9414, offset_9432),\n                          segred_group_sizze_9118) && (sub32(local_tid_9414,\n                                                             mul32(squot32(local_tid_9414,\n                                                                           wave_sizze_9416),\n                                                                   wave_sizze_9416)) &\n                                                       sub32(mul32(2,\n                                                                   offset_9432),\n                                                             1)) == 0) {\n                    // read array element\n                    {\n                        x_9425 = ((volatile __local\n                                   int32_t *) red_arr_mem_9420)[add32(local_tid_9414,\n                                                                      offset_9432)];\n                    }\n                    // apply reduction operation\n                    {\n                        int32_t res_9426 = add32(x_9424, x_9425);\n                        \n                        x_9424 = res_9426;\n                    }\n                    // write result of operation\n                    {\n                        ((volatile __local\n                          int32_t *) red_arr_mem_9420)[local_tid_9414] = x_9424;",
                   "\n                    }\n                }\n                offset_9432 *= 2;\n            }\n            skip_waves_9433 = 1;\n            while (slt32(skip_waves_9433,\n                         squot32(sub32(add32(segred_group_sizze_9118,\n                                             wave_sizze_9416), 1),\n                                 wave_sizze_9416))) {\n                barrier(CLK_LOCAL_MEM_FENCE);\n                offset_9432 = mul32(skip_waves_9433, wave_sizze_9416);\n                if (slt32(add32(local_tid_9414, offset_9432),\n                          segred_group_sizze_9118) && (sub32(local_tid_9414,\n                                                             mul32(squot32(local_tid_9414,\n                                                                           wave_sizze_9416),\n                                                                   wave_sizze_9416)) ==\n                                                       0 &&\n                                                       (squot32(local_tid_9414,\n                                                                wave_sizze_9416) &\n                                                        sub32(mul32(2,\n                                                                    skip_waves_9433),\n                                                              1)) == 0)) {\n                    // read array element\n                    {\n                        x_9425 = ((__local\n                                   int32_t *) red_arr_mem_9420)[add32(local_tid_9414,\n                                                                      offset_9432)];\n                    }\n                    // apply reduction operation\n                    {\n                        int32_t res_9426 = add32(x_9424, x_9425);\n                        \n                        x_9424 = res_9426;\n                    }\n                    // write result of operation\n                    {\n                        ((__local int32_t *) red_arr_mem_9420)[l",
                   "ocal_tid_9414] =\n                            x_9424;\n                    }\n                }\n                skip_waves_9433 *= 2;\n            }\n            // and back to memory with the final result\n            {\n                if (local_tid_9414 == 0) {\n                    ((__global int32_t *) mem_9246)[0] = x_9424;\n                }\n            }\n        }\n    }\n    \n  error_1:\n    return;\n    #undef segred_group_sizze_9118\n}\n",
                   NULL};
static const char *size_names[] = {"main.group_size_9324",
                                   "main.segmap_group_size_9095",
                                   "main.segmap_group_size_9132",
                                   "main.segmap_group_size_9144",
                                   "main.segmap_group_size_9198",
                                   "main.segred_group_size_9117",
                                   "main.segred_num_groups_9119",
                                   "main.segscan_group_size_9108",
                                   "main.segscan_group_size_9157",
                                   "main.segscan_group_size_9166",
                                   "main.segscan_num_groups_9110",
                                   "main.segscan_num_groups_9159",
                                   "main.segscan_num_groups_9168"};
static const char *size_vars[] = {"mainzigroup_sizze_9324",
                                  "mainzisegmap_group_sizze_9095",
                                  "mainzisegmap_group_sizze_9132",
                                  "mainzisegmap_group_sizze_9144",
                                  "mainzisegmap_group_sizze_9198",
                                  "mainzisegred_group_sizze_9117",
                                  "mainzisegred_num_groups_9119",
                                  "mainzisegscan_group_sizze_9108",
                                  "mainzisegscan_group_sizze_9157",
                                  "mainzisegscan_group_sizze_9166",
                                  "mainzisegscan_num_groups_9110",
                                  "mainzisegscan_num_groups_9159",
                                  "mainzisegscan_num_groups_9168"};
static const char *size_classes[] = {"group_size", "group_size", "group_size",
                                     "group_size", "group_size", "group_size",
                                     "num_groups", "group_size", "group_size",
                                     "group_size", "num_groups", "num_groups",
                                     "num_groups"};
int futhark_get_num_sizes(void)
{
    return 13;
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
    size_t mainzigroup_sizze_9324;
    size_t mainzisegmap_group_sizze_9095;
    size_t mainzisegmap_group_sizze_9132;
    size_t mainzisegmap_group_sizze_9144;
    size_t mainzisegmap_group_sizze_9198;
    size_t mainzisegred_group_sizze_9117;
    size_t mainzisegred_num_groups_9119;
    size_t mainzisegscan_group_sizze_9108;
    size_t mainzisegscan_group_sizze_9157;
    size_t mainzisegscan_group_sizze_9166;
    size_t mainzisegscan_num_groups_9110;
    size_t mainzisegscan_num_groups_9159;
    size_t mainzisegscan_num_groups_9168;
} ;
struct futhark_context_config {
    struct opencl_config opencl;
    size_t sizes[13];
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
    cfg->sizes[5] = 0;
    cfg->sizes[6] = 0;
    cfg->sizes[7] = 0;
    cfg->sizes[8] = 0;
    cfg->sizes[9] = 0;
    cfg->sizes[10] = 0;
    cfg->sizes[11] = 0;
    cfg->sizes[12] = 0;
    opencl_config_init(&cfg->opencl, 13, size_names, size_vars, cfg->sizes,
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
    for (int i = 0; i < 13; i++) {
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
    struct memblock_device counter_mem_9408;
    int total_runs;
    long total_runtime;
    cl_kernel iota_9321;
    cl_kernel scan_stage1_9114;
    cl_kernel scan_stage1_9163;
    cl_kernel scan_stage1_9172;
    cl_kernel scan_stage2_9114;
    cl_kernel scan_stage2_9163;
    cl_kernel scan_stage2_9172;
    cl_kernel scan_stage3_9114;
    cl_kernel scan_stage3_9163;
    cl_kernel scan_stage3_9172;
    cl_kernel segmap_9092;
    cl_kernel segmap_9129;
    cl_kernel segmap_9141;
    cl_kernel segmap_9195;
    cl_kernel segred_nonseg_9127;
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
    int64_t iota_9321_total_runtime;
    int iota_9321_runs;
    int64_t scan_stage1_9114_total_runtime;
    int scan_stage1_9114_runs;
    int64_t scan_stage1_9163_total_runtime;
    int scan_stage1_9163_runs;
    int64_t scan_stage1_9172_total_runtime;
    int scan_stage1_9172_runs;
    int64_t scan_stage2_9114_total_runtime;
    int scan_stage2_9114_runs;
    int64_t scan_stage2_9163_total_runtime;
    int scan_stage2_9163_runs;
    int64_t scan_stage2_9172_total_runtime;
    int scan_stage2_9172_runs;
    int64_t scan_stage3_9114_total_runtime;
    int scan_stage3_9114_runs;
    int64_t scan_stage3_9163_total_runtime;
    int scan_stage3_9163_runs;
    int64_t scan_stage3_9172_total_runtime;
    int scan_stage3_9172_runs;
    int64_t segmap_9092_total_runtime;
    int segmap_9092_runs;
    int64_t segmap_9129_total_runtime;
    int segmap_9129_runs;
    int64_t segmap_9141_total_runtime;
    int segmap_9141_runs;
    int64_t segmap_9195_total_runtime;
    int segmap_9195_runs;
    int64_t segred_nonseg_9127_total_runtime;
    int segred_nonseg_9127_runs;
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
    ctx->iota_9321_total_runtime = 0;
    ctx->iota_9321_runs = 0;
    ctx->scan_stage1_9114_total_runtime = 0;
    ctx->scan_stage1_9114_runs = 0;
    ctx->scan_stage1_9163_total_runtime = 0;
    ctx->scan_stage1_9163_runs = 0;
    ctx->scan_stage1_9172_total_runtime = 0;
    ctx->scan_stage1_9172_runs = 0;
    ctx->scan_stage2_9114_total_runtime = 0;
    ctx->scan_stage2_9114_runs = 0;
    ctx->scan_stage2_9163_total_runtime = 0;
    ctx->scan_stage2_9163_runs = 0;
    ctx->scan_stage2_9172_total_runtime = 0;
    ctx->scan_stage2_9172_runs = 0;
    ctx->scan_stage3_9114_total_runtime = 0;
    ctx->scan_stage3_9114_runs = 0;
    ctx->scan_stage3_9163_total_runtime = 0;
    ctx->scan_stage3_9163_runs = 0;
    ctx->scan_stage3_9172_total_runtime = 0;
    ctx->scan_stage3_9172_runs = 0;
    ctx->segmap_9092_total_runtime = 0;
    ctx->segmap_9092_runs = 0;
    ctx->segmap_9129_total_runtime = 0;
    ctx->segmap_9129_runs = 0;
    ctx->segmap_9141_total_runtime = 0;
    ctx->segmap_9141_runs = 0;
    ctx->segmap_9195_total_runtime = 0;
    ctx->segmap_9195_runs = 0;
    ctx->segred_nonseg_9127_total_runtime = 0;
    ctx->segred_nonseg_9127_runs = 0;
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
        ctx->iota_9321 = clCreateKernel(prog, "iota_9321", &error);
        OPENCL_SUCCEED_FATAL(error);
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "iota_9321");
    }
    {
        ctx->scan_stage1_9114 = clCreateKernel(prog, "scan_stage1_9114",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage1_9114, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage1_9114");
    }
    {
        ctx->scan_stage1_9163 = clCreateKernel(prog, "scan_stage1_9163",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage1_9163, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage1_9163");
    }
    {
        ctx->scan_stage1_9172 = clCreateKernel(prog, "scan_stage1_9172",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage1_9172, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage1_9172");
    }
    {
        ctx->scan_stage2_9114 = clCreateKernel(prog, "scan_stage2_9114",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage2_9114, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage2_9114");
    }
    {
        ctx->scan_stage2_9163 = clCreateKernel(prog, "scan_stage2_9163",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage2_9163, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage2_9163");
    }
    {
        ctx->scan_stage2_9172 = clCreateKernel(prog, "scan_stage2_9172",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage2_9172, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage2_9172");
    }
    {
        ctx->scan_stage3_9114 = clCreateKernel(prog, "scan_stage3_9114",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage3_9114, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage3_9114");
    }
    {
        ctx->scan_stage3_9163 = clCreateKernel(prog, "scan_stage3_9163",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage3_9163, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage3_9163");
    }
    {
        ctx->scan_stage3_9172 = clCreateKernel(prog, "scan_stage3_9172",
                                               &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->scan_stage3_9172, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "scan_stage3_9172");
    }
    {
        ctx->segmap_9092 = clCreateKernel(prog, "segmap_9092", &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->segmap_9092, 0, sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "segmap_9092");
    }
    {
        ctx->segmap_9129 = clCreateKernel(prog, "segmap_9129", &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->segmap_9129, 0, sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "segmap_9129");
    }
    {
        ctx->segmap_9141 = clCreateKernel(prog, "segmap_9141", &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->segmap_9141, 0, sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "segmap_9141");
    }
    {
        ctx->segmap_9195 = clCreateKernel(prog, "segmap_9195", &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->segmap_9195, 0, sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "segmap_9195");
    }
    {
        ctx->segred_nonseg_9127 = clCreateKernel(prog, "segred_nonseg_9127",
                                                 &error);
        OPENCL_SUCCEED_FATAL(error);
        OPENCL_SUCCEED_FATAL(clSetKernelArg(ctx->segred_nonseg_9127, 0,
                                            sizeof(cl_mem),
                                            &ctx->global_failure));
        if (ctx->debugging)
            fprintf(stderr, "Created kernel %s.\n", "segred_nonseg_9127");
    }
    {
        cl_int success;
        
        ctx->counter_mem_9408.references = NULL;
        ctx->counter_mem_9408.size = 0;
        ctx->counter_mem_9408.mem = clCreateBuffer(ctx->opencl.ctx,
                                                   CL_MEM_READ_WRITE, (10 >
                                                                       0 ? 10 : 1) *
                                                   sizeof(int32_t), NULL,
                                                   &success);
        OPENCL_SUCCEED_OR_RETURN(success);
        if (10 > 0)
            OPENCL_SUCCEED_OR_RETURN(clEnqueueWriteBuffer(ctx->opencl.queue,
                                                          ctx->counter_mem_9408.mem,
                                                          CL_TRUE, 0, 10 *
                                                          sizeof(int32_t),
                                                          counter_mem_realtype_9669,
                                                          0, NULL, NULL));
    }
    ctx->sizes.mainzigroup_sizze_9324 = cfg->sizes[0];
    ctx->sizes.mainzisegmap_group_sizze_9095 = cfg->sizes[1];
    ctx->sizes.mainzisegmap_group_sizze_9132 = cfg->sizes[2];
    ctx->sizes.mainzisegmap_group_sizze_9144 = cfg->sizes[3];
    ctx->sizes.mainzisegmap_group_sizze_9198 = cfg->sizes[4];
    ctx->sizes.mainzisegred_group_sizze_9117 = cfg->sizes[5];
    ctx->sizes.mainzisegred_num_groups_9119 = cfg->sizes[6];
    ctx->sizes.mainzisegscan_group_sizze_9108 = cfg->sizes[7];
    ctx->sizes.mainzisegscan_group_sizze_9157 = cfg->sizes[8];
    ctx->sizes.mainzisegscan_group_sizze_9166 = cfg->sizes[9];
    ctx->sizes.mainzisegscan_num_groups_9110 = cfg->sizes[10];
    ctx->sizes.mainzisegscan_num_groups_9159 = cfg->sizes[11];
    ctx->sizes.mainzisegscan_num_groups_9168 = cfg->sizes[12];
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
                       struct memblock_device *out_mem_p_9642,
                       int32_t *out_out_arrsizze_9643,
                       struct memblock_device k_mem_9223,
                       struct memblock_device d_mem_9224, int32_t n_8851,
                       int32_t n_8852);
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
                "iota_9321            ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->iota_9321_runs, (long) ctx->iota_9321_total_runtime /
                (ctx->iota_9321_runs != 0 ? ctx->iota_9321_runs : 1),
                (long) ctx->iota_9321_total_runtime);
        ctx->total_runtime += ctx->iota_9321_total_runtime;
        ctx->total_runs += ctx->iota_9321_runs;
        fprintf(stderr,
                "scan_stage1_9114     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage1_9114_runs,
                (long) ctx->scan_stage1_9114_total_runtime /
                (ctx->scan_stage1_9114_runs !=
                 0 ? ctx->scan_stage1_9114_runs : 1),
                (long) ctx->scan_stage1_9114_total_runtime);
        ctx->total_runtime += ctx->scan_stage1_9114_total_runtime;
        ctx->total_runs += ctx->scan_stage1_9114_runs;
        fprintf(stderr,
                "scan_stage1_9163     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage1_9163_runs,
                (long) ctx->scan_stage1_9163_total_runtime /
                (ctx->scan_stage1_9163_runs !=
                 0 ? ctx->scan_stage1_9163_runs : 1),
                (long) ctx->scan_stage1_9163_total_runtime);
        ctx->total_runtime += ctx->scan_stage1_9163_total_runtime;
        ctx->total_runs += ctx->scan_stage1_9163_runs;
        fprintf(stderr,
                "scan_stage1_9172     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage1_9172_runs,
                (long) ctx->scan_stage1_9172_total_runtime /
                (ctx->scan_stage1_9172_runs !=
                 0 ? ctx->scan_stage1_9172_runs : 1),
                (long) ctx->scan_stage1_9172_total_runtime);
        ctx->total_runtime += ctx->scan_stage1_9172_total_runtime;
        ctx->total_runs += ctx->scan_stage1_9172_runs;
        fprintf(stderr,
                "scan_stage2_9114     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage2_9114_runs,
                (long) ctx->scan_stage2_9114_total_runtime /
                (ctx->scan_stage2_9114_runs !=
                 0 ? ctx->scan_stage2_9114_runs : 1),
                (long) ctx->scan_stage2_9114_total_runtime);
        ctx->total_runtime += ctx->scan_stage2_9114_total_runtime;
        ctx->total_runs += ctx->scan_stage2_9114_runs;
        fprintf(stderr,
                "scan_stage2_9163     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage2_9163_runs,
                (long) ctx->scan_stage2_9163_total_runtime /
                (ctx->scan_stage2_9163_runs !=
                 0 ? ctx->scan_stage2_9163_runs : 1),
                (long) ctx->scan_stage2_9163_total_runtime);
        ctx->total_runtime += ctx->scan_stage2_9163_total_runtime;
        ctx->total_runs += ctx->scan_stage2_9163_runs;
        fprintf(stderr,
                "scan_stage2_9172     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage2_9172_runs,
                (long) ctx->scan_stage2_9172_total_runtime /
                (ctx->scan_stage2_9172_runs !=
                 0 ? ctx->scan_stage2_9172_runs : 1),
                (long) ctx->scan_stage2_9172_total_runtime);
        ctx->total_runtime += ctx->scan_stage2_9172_total_runtime;
        ctx->total_runs += ctx->scan_stage2_9172_runs;
        fprintf(stderr,
                "scan_stage3_9114     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage3_9114_runs,
                (long) ctx->scan_stage3_9114_total_runtime /
                (ctx->scan_stage3_9114_runs !=
                 0 ? ctx->scan_stage3_9114_runs : 1),
                (long) ctx->scan_stage3_9114_total_runtime);
        ctx->total_runtime += ctx->scan_stage3_9114_total_runtime;
        ctx->total_runs += ctx->scan_stage3_9114_runs;
        fprintf(stderr,
                "scan_stage3_9163     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage3_9163_runs,
                (long) ctx->scan_stage3_9163_total_runtime /
                (ctx->scan_stage3_9163_runs !=
                 0 ? ctx->scan_stage3_9163_runs : 1),
                (long) ctx->scan_stage3_9163_total_runtime);
        ctx->total_runtime += ctx->scan_stage3_9163_total_runtime;
        ctx->total_runs += ctx->scan_stage3_9163_runs;
        fprintf(stderr,
                "scan_stage3_9172     ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->scan_stage3_9172_runs,
                (long) ctx->scan_stage3_9172_total_runtime /
                (ctx->scan_stage3_9172_runs !=
                 0 ? ctx->scan_stage3_9172_runs : 1),
                (long) ctx->scan_stage3_9172_total_runtime);
        ctx->total_runtime += ctx->scan_stage3_9172_total_runtime;
        ctx->total_runs += ctx->scan_stage3_9172_runs;
        fprintf(stderr,
                "segmap_9092          ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->segmap_9092_runs, (long) ctx->segmap_9092_total_runtime /
                (ctx->segmap_9092_runs != 0 ? ctx->segmap_9092_runs : 1),
                (long) ctx->segmap_9092_total_runtime);
        ctx->total_runtime += ctx->segmap_9092_total_runtime;
        ctx->total_runs += ctx->segmap_9092_runs;
        fprintf(stderr,
                "segmap_9129          ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->segmap_9129_runs, (long) ctx->segmap_9129_total_runtime /
                (ctx->segmap_9129_runs != 0 ? ctx->segmap_9129_runs : 1),
                (long) ctx->segmap_9129_total_runtime);
        ctx->total_runtime += ctx->segmap_9129_total_runtime;
        ctx->total_runs += ctx->segmap_9129_runs;
        fprintf(stderr,
                "segmap_9141          ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->segmap_9141_runs, (long) ctx->segmap_9141_total_runtime /
                (ctx->segmap_9141_runs != 0 ? ctx->segmap_9141_runs : 1),
                (long) ctx->segmap_9141_total_runtime);
        ctx->total_runtime += ctx->segmap_9141_total_runtime;
        ctx->total_runs += ctx->segmap_9141_runs;
        fprintf(stderr,
                "segmap_9195          ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->segmap_9195_runs, (long) ctx->segmap_9195_total_runtime /
                (ctx->segmap_9195_runs != 0 ? ctx->segmap_9195_runs : 1),
                (long) ctx->segmap_9195_total_runtime);
        ctx->total_runtime += ctx->segmap_9195_total_runtime;
        ctx->total_runs += ctx->segmap_9195_runs;
        fprintf(stderr,
                "segred_nonseg_9127   ran %5d times; avg: %8ldus; total: %8ldus\n",
                ctx->segred_nonseg_9127_runs,
                (long) ctx->segred_nonseg_9127_total_runtime /
                (ctx->segred_nonseg_9127_runs !=
                 0 ? ctx->segred_nonseg_9127_runs : 1),
                (long) ctx->segred_nonseg_9127_total_runtime);
        ctx->total_runtime += ctx->segred_nonseg_9127_total_runtime;
        ctx->total_runs += ctx->segred_nonseg_9127_runs;
        if (ctx->profiling)
            fprintf(stderr, "%d operations with cumulative runtime: %6ldus\n",
                    ctx->total_runs, ctx->total_runtime);
    }
}
static int futrts_main(struct futhark_context *ctx,
                       struct memblock_device *out_mem_p_9642,
                       int32_t *out_out_arrsizze_9643,
                       struct memblock_device k_mem_9223,
                       struct memblock_device d_mem_9224, int32_t n_8851,
                       int32_t n_8852)
{
    struct memblock_device out_mem_9319;
    
    out_mem_9319.references = NULL;
    
    int32_t out_arrsizze_9320;
    bool dim_match_8855 = n_8851 == n_8852;
    bool empty_or_match_cert_8856;
    
    if (!dim_match_8855) {
        ctx->error = msgprintf("Error: %s\n\nBacktrace:\n%s",
                               "function arguments of wrong shape",
                               "-> #0  reduce_by_index.fut:45:1-61\n");
        if (memblock_unref_device(ctx, &out_mem_9319, "out_mem_9319") != 0)
            return 1;
        return 1;
    }
    
    int64_t binop_x_9226 = sext_i32_i64(n_8851);
    int64_t bytes_9225 = mul64(4, binop_x_9226);
    struct memblock_device mem_9227;
    
    mem_9227.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9227, bytes_9225, "mem_9227"))
        return 1;
    
    int32_t group_sizze_9324;
    
    group_sizze_9324 = ctx->sizes.mainzigroup_sizze_9324;
    
    int32_t num_groups_9325;
    
    num_groups_9325 = squot32(sub32(add32(n_8851,
                                          sext_i32_i32(group_sizze_9324)), 1),
                              sext_i32_i32(group_sizze_9324));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->iota_9321, 0, sizeof(n_8851),
                                            &n_8851));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->iota_9321, 1,
                                            sizeof(mem_9227.mem),
                                            &mem_9227.mem));
    if (1 * (num_groups_9325 * group_sizze_9324) != 0) {
        const size_t global_work_sizze_9644[1] = {num_groups_9325 *
                     group_sizze_9324};
        const size_t local_work_sizze_9648[1] = {group_sizze_9324};
        int64_t time_start_9645 = 0, time_end_9646 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "iota_9321");
            fprintf(stderr, "%zu", global_work_sizze_9644[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_9648[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) 0);
            time_start_9645 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->iota_9321, 1, NULL,
                                                        global_work_sizze_9644,
                                                        local_work_sizze_9648,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->iota_9321_runs,
                                                                                                  &ctx->iota_9321_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_9646 = get_wall_time();
            
            long time_diff_9647 = time_end_9646 - time_start_9645;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "iota_9321",
                    time_diff_9647);
        }
    }
    
    int32_t segmap_group_sizze_9096;
    
    segmap_group_sizze_9096 = ctx->sizes.mainzisegmap_group_sizze_9095;
    
    int64_t segmap_group_sizze_9097 = sext_i32_i64(segmap_group_sizze_9096);
    int64_t y_9098 = sub64(segmap_group_sizze_9097, 1);
    int64_t x_9099 = add64(y_9098, binop_x_9226);
    int64_t x_9207 = squot64(x_9099, segmap_group_sizze_9097);
    int32_t segmap_usable_groups_9102 = sext_i64_i32(x_9207);
    int32_t segscan_group_sizze_9109;
    
    segscan_group_sizze_9109 = ctx->sizes.mainzisegscan_group_sizze_9108;
    
    int32_t num_groups_9111;
    int32_t max_num_groups_9326;
    
    max_num_groups_9326 = ctx->sizes.mainzisegscan_num_groups_9110;
    num_groups_9111 = sext_i64_i32(smax64(1,
                                          smin64(squot64(sub64(add64(binop_x_9226,
                                                                     sext_i32_i64(segscan_group_sizze_9109)),
                                                               1),
                                                         sext_i32_i64(segscan_group_sizze_9109)),
                                                 sext_i32_i64(max_num_groups_9326))));
    
    int32_t segred_group_sizze_9118;
    
    segred_group_sizze_9118 = ctx->sizes.mainzisegred_group_sizze_9117;
    
    int32_t num_groups_9120;
    int32_t max_num_groups_9327;
    
    max_num_groups_9327 = ctx->sizes.mainzisegred_num_groups_9119;
    num_groups_9120 = sext_i64_i32(smax64(1,
                                          smin64(squot64(sub64(add64(binop_x_9226,
                                                                     sext_i32_i64(segred_group_sizze_9118)),
                                                               1),
                                                         sext_i32_i64(segred_group_sizze_9118)),
                                                 sext_i32_i64(max_num_groups_9327))));
    
    int32_t segmap_group_sizze_9133;
    
    segmap_group_sizze_9133 = ctx->sizes.mainzisegmap_group_sizze_9132;
    
    int64_t segmap_group_sizze_9134 = sext_i32_i64(segmap_group_sizze_9133);
    int64_t y_9135 = sub64(segmap_group_sizze_9134, 1);
    int64_t x_9136 = add64(y_9135, binop_x_9226);
    int64_t x_9209 = squot64(x_9136, segmap_group_sizze_9134);
    int32_t segmap_usable_groups_9139 = sext_i64_i32(x_9209);
    struct memblock_device mem_9232;
    
    mem_9232.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9232, bytes_9225, "mem_9232"))
        return 1;
    
    struct memblock_device mem_9236;
    
    mem_9236.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9236, bytes_9225, "mem_9236"))
        return 1;
    
    struct memblock_device mem_9239;
    
    mem_9239.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9239, bytes_9225, "mem_9239"))
        return 1;
    
    struct memblock_device mem_9242;
    
    mem_9242.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9242, bytes_9225, "mem_9242"))
        return 1;
    
    struct memblock_device mem_9246;
    
    mem_9246.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9246, 4, "mem_9246"))
        return 1;
    
    struct memblock_device mem_9251;
    
    mem_9251.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9251, bytes_9225, "mem_9251"))
        return 1;
    
    struct memblock_device mem_9256;
    
    mem_9256.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9256, bytes_9225, "mem_9256"))
        return 1;
    
    struct memblock_device res_mem_9265;
    
    res_mem_9265.references = NULL;
    
    struct memblock_device res_mem_9266;
    
    res_mem_9266.references = NULL;
    
    struct memblock_device xs_mem_9228;
    
    xs_mem_9228.references = NULL;
    
    struct memblock_device xs_mem_9229;
    
    xs_mem_9229.references = NULL;
    if (memblock_set_device(ctx, &xs_mem_9228, &k_mem_9223, "k_mem_9223") != 0)
        return 1;
    if (memblock_set_device(ctx, &xs_mem_9229, &mem_9227, "mem_9227") != 0)
        return 1;
    for (int32_t i_8865 = 0; i_8865 < 32; i_8865++) {
        if (ctx->debugging)
            fprintf(stderr, "%s\n", "\n# SegMap");
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9092, 1,
                                                sizeof(n_8851), &n_8851));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9092, 2,
                                                sizeof(i_8865), &i_8865));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9092, 3,
                                                sizeof(xs_mem_9228.mem),
                                                &xs_mem_9228.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9092, 4,
                                                sizeof(mem_9232.mem),
                                                &mem_9232.mem));
        if (1 * (segmap_usable_groups_9102 * segmap_group_sizze_9096) != 0) {
            const size_t global_work_sizze_9649[1] =
                         {segmap_usable_groups_9102 * segmap_group_sizze_9096};
            const size_t local_work_sizze_9653[1] = {segmap_group_sizze_9096};
            int64_t time_start_9650 = 0, time_end_9651 = 0;
            
            if (ctx->debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "segmap_9092");
                fprintf(stderr, "%zu", global_work_sizze_9649[0]);
                fprintf(stderr, "] and local work size [");
                fprintf(stderr, "%zu", local_work_sizze_9653[0]);
                fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                        (int) 0);
                time_start_9650 = get_wall_time();
            }
            OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                            ctx->segmap_9092, 1,
                                                            NULL,
                                                            global_work_sizze_9649,
                                                            local_work_sizze_9653,
                                                            0, NULL,
                                                            ctx->profiling_paused ||
                                                            !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                      &ctx->segmap_9092_runs,
                                                                                                      &ctx->segmap_9092_total_runtime)));
            if (ctx->debugging) {
                OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
                time_end_9651 = get_wall_time();
                
                long time_diff_9652 = time_end_9651 - time_start_9650;
                
                fprintf(stderr, "kernel %s runtime: %ldus\n", "segmap_9092",
                        time_diff_9652);
            }
        }
        if (ctx->debugging)
            fprintf(stderr, "%s\n", "\n# SegScan");
        
        int32_t num_threads_9337 = mul32(num_groups_9111,
                                         segscan_group_sizze_9109);
        
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9114, 1,
                                                mul32((int32_t) sizeof(int32_t),
                                                      segscan_group_sizze_9109),
                                                NULL));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9114, 2,
                                                mul32((int32_t) sizeof(int32_t),
                                                      segscan_group_sizze_9109),
                                                NULL));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9114, 3,
                                                sizeof(n_8851), &n_8851));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9114, 4,
                                                sizeof(mem_9232.mem),
                                                &mem_9232.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9114, 5,
                                                sizeof(mem_9236.mem),
                                                &mem_9236.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9114, 6,
                                                sizeof(mem_9239.mem),
                                                &mem_9239.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9114, 7,
                                                sizeof(mem_9242.mem),
                                                &mem_9242.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9114, 8,
                                                sizeof(num_threads_9337),
                                                &num_threads_9337));
        if (1 * (num_groups_9111 * segscan_group_sizze_9109) != 0) {
            const size_t global_work_sizze_9654[1] = {num_groups_9111 *
                         segscan_group_sizze_9109};
            const size_t local_work_sizze_9658[1] = {segscan_group_sizze_9109};
            int64_t time_start_9655 = 0, time_end_9656 = 0;
            
            if (ctx->debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan_stage1_9114");
                fprintf(stderr, "%zu", global_work_sizze_9654[0]);
                fprintf(stderr, "] and local work size [");
                fprintf(stderr, "%zu", local_work_sizze_9658[0]);
                fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                        (int) (0 + mul32((int32_t) sizeof(int32_t),
                                         segscan_group_sizze_9109) +
                               mul32((int32_t) sizeof(int32_t),
                                     segscan_group_sizze_9109)));
                time_start_9655 = get_wall_time();
            }
            OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                            ctx->scan_stage1_9114,
                                                            1, NULL,
                                                            global_work_sizze_9654,
                                                            local_work_sizze_9658,
                                                            0, NULL,
                                                            ctx->profiling_paused ||
                                                            !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                      &ctx->scan_stage1_9114_runs,
                                                                                                      &ctx->scan_stage1_9114_total_runtime)));
            if (ctx->debugging) {
                OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
                time_end_9656 = get_wall_time();
                
                long time_diff_9657 = time_end_9656 - time_start_9655;
                
                fprintf(stderr, "kernel %s runtime: %ldus\n",
                        "scan_stage1_9114", time_diff_9657);
            }
        }
        if (ctx->debugging)
            fprintf(stderr, "%s: %llu%c", "elems_per_group",
                    (long long) mul32(segscan_group_sizze_9109,
                                      squot32(sub32(add32(n_8851,
                                                          num_threads_9337), 1),
                                              num_threads_9337)), '\n');
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9114, 1,
                                                mul32((int32_t) sizeof(int32_t),
                                                      num_groups_9111), NULL));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9114, 2,
                                                mul32((int32_t) sizeof(int32_t),
                                                      num_groups_9111), NULL));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9114, 3,
                                                sizeof(n_8851), &n_8851));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9114, 4,
                                                sizeof(num_groups_9111),
                                                &num_groups_9111));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9114, 5,
                                                sizeof(mem_9236.mem),
                                                &mem_9236.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9114, 6,
                                                sizeof(mem_9239.mem),
                                                &mem_9239.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9114, 7,
                                                sizeof(num_threads_9337),
                                                &num_threads_9337));
        if (1 * (1 * num_groups_9111) != 0) {
            const size_t global_work_sizze_9659[1] = {1 * num_groups_9111};
            const size_t local_work_sizze_9663[1] = {num_groups_9111};
            int64_t time_start_9660 = 0, time_end_9661 = 0;
            
            if (ctx->debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan_stage2_9114");
                fprintf(stderr, "%zu", global_work_sizze_9659[0]);
                fprintf(stderr, "] and local work size [");
                fprintf(stderr, "%zu", local_work_sizze_9663[0]);
                fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                        (int) (0 + mul32((int32_t) sizeof(int32_t),
                                         num_groups_9111) +
                               mul32((int32_t) sizeof(int32_t),
                                     num_groups_9111)));
                time_start_9660 = get_wall_time();
            }
            OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                            ctx->scan_stage2_9114,
                                                            1, NULL,
                                                            global_work_sizze_9659,
                                                            local_work_sizze_9663,
                                                            0, NULL,
                                                            ctx->profiling_paused ||
                                                            !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                      &ctx->scan_stage2_9114_runs,
                                                                                                      &ctx->scan_stage2_9114_total_runtime)));
            if (ctx->debugging) {
                OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
                time_end_9661 = get_wall_time();
                
                long time_diff_9662 = time_end_9661 - time_start_9660;
                
                fprintf(stderr, "kernel %s runtime: %ldus\n",
                        "scan_stage2_9114", time_diff_9662);
            }
        }
        
        int32_t required_groups_9396 = squot32(sub32(add32(n_8851,
                                                           segscan_group_sizze_9109),
                                                     1),
                                               segscan_group_sizze_9109);
        
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9114, 1,
                                                sizeof(n_8851), &n_8851));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9114, 2,
                                                sizeof(num_groups_9111),
                                                &num_groups_9111));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9114, 3,
                                                sizeof(mem_9236.mem),
                                                &mem_9236.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9114, 4,
                                                sizeof(mem_9239.mem),
                                                &mem_9239.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9114, 5,
                                                sizeof(num_threads_9337),
                                                &num_threads_9337));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9114, 6,
                                                sizeof(required_groups_9396),
                                                &required_groups_9396));
        if (1 * (num_groups_9111 * segscan_group_sizze_9109) != 0) {
            const size_t global_work_sizze_9664[1] = {num_groups_9111 *
                         segscan_group_sizze_9109};
            const size_t local_work_sizze_9668[1] = {segscan_group_sizze_9109};
            int64_t time_start_9665 = 0, time_end_9666 = 0;
            
            if (ctx->debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "scan_stage3_9114");
                fprintf(stderr, "%zu", global_work_sizze_9664[0]);
                fprintf(stderr, "] and local work size [");
                fprintf(stderr, "%zu", local_work_sizze_9668[0]);
                fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                        (int) 0);
                time_start_9665 = get_wall_time();
            }
            OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                            ctx->scan_stage3_9114,
                                                            1, NULL,
                                                            global_work_sizze_9664,
                                                            local_work_sizze_9668,
                                                            0, NULL,
                                                            ctx->profiling_paused ||
                                                            !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                      &ctx->scan_stage3_9114_runs,
                                                                                                      &ctx->scan_stage3_9114_total_runtime)));
            if (ctx->debugging) {
                OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
                time_end_9666 = get_wall_time();
                
                long time_diff_9667 = time_end_9666 - time_start_9665;
                
                fprintf(stderr, "kernel %s runtime: %ldus\n",
                        "scan_stage3_9114", time_diff_9667);
            }
        }
        
        struct memblock_device counter_mem_9408 = ctx->counter_mem_9408;
        struct memblock_device group_res_arr_mem_9410;
        
        group_res_arr_mem_9410.references = NULL;
        if (memblock_alloc_device(ctx, &group_res_arr_mem_9410,
                                  mul32((int32_t) sizeof(int32_t),
                                        mul32(segred_group_sizze_9118,
                                              num_groups_9120)),
                                  "group_res_arr_mem_9410"))
            return 1;
        
        int32_t num_threads_9412 = mul32(num_groups_9120,
                                         segred_group_sizze_9118);
        
        if (ctx->debugging)
            fprintf(stderr, "%s\n", "\n# SegRed");
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 1,
                                                mul32((int32_t) sizeof(int32_t),
                                                      segred_group_sizze_9118),
                                                NULL));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 2,
                                                (int32_t) sizeof(bool), NULL));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 3,
                                                sizeof(n_8851), &n_8851));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 4,
                                                sizeof(num_groups_9120),
                                                &num_groups_9120));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 5,
                                                sizeof(xs_mem_9228.mem),
                                                &xs_mem_9228.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 6,
                                                sizeof(xs_mem_9229.mem),
                                                &xs_mem_9229.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 7,
                                                sizeof(mem_9242.mem),
                                                &mem_9242.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 8,
                                                sizeof(mem_9246.mem),
                                                &mem_9246.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 9,
                                                sizeof(mem_9251.mem),
                                                &mem_9251.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 10,
                                                sizeof(mem_9256.mem),
                                                &mem_9256.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 11,
                                                sizeof(counter_mem_9408.mem),
                                                &counter_mem_9408.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 12,
                                                sizeof(group_res_arr_mem_9410.mem),
                                                &group_res_arr_mem_9410.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segred_nonseg_9127, 13,
                                                sizeof(num_threads_9412),
                                                &num_threads_9412));
        if (1 * (num_groups_9120 * segred_group_sizze_9118) != 0) {
            const size_t global_work_sizze_9670[1] = {num_groups_9120 *
                         segred_group_sizze_9118};
            const size_t local_work_sizze_9674[1] = {segred_group_sizze_9118};
            int64_t time_start_9671 = 0, time_end_9672 = 0;
            
            if (ctx->debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "segred_nonseg_9127");
                fprintf(stderr, "%zu", global_work_sizze_9670[0]);
                fprintf(stderr, "] and local work size [");
                fprintf(stderr, "%zu", local_work_sizze_9674[0]);
                fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                        (int) (0 + mul32((int32_t) sizeof(int32_t),
                                         segred_group_sizze_9118) +
                               (int32_t) sizeof(bool)));
                time_start_9671 = get_wall_time();
            }
            OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                            ctx->segred_nonseg_9127,
                                                            1, NULL,
                                                            global_work_sizze_9670,
                                                            local_work_sizze_9674,
                                                            0, NULL,
                                                            ctx->profiling_paused ||
                                                            !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                      &ctx->segred_nonseg_9127_runs,
                                                                                                      &ctx->segred_nonseg_9127_total_runtime)));
            if (ctx->debugging) {
                OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
                time_end_9672 = get_wall_time();
                
                long time_diff_9673 = time_end_9672 - time_start_9671;
                
                fprintf(stderr, "kernel %s runtime: %ldus\n",
                        "segred_nonseg_9127", time_diff_9673);
            }
        }
        
        int32_t read_res_9675;
        
        OPENCL_SUCCEED_OR_RETURN(clEnqueueReadBuffer(ctx->opencl.queue,
                                                     mem_9246.mem,
                                                     ctx->failure_is_an_option ? CL_FALSE : CL_TRUE,
                                                     0 * sizeof(int32_t),
                                                     sizeof(int32_t),
                                                     &read_res_9675, 0, NULL,
                                                     ctx->profiling_paused ||
                                                     !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                               &ctx->copy_scalar_from_dev_runs,
                                                                                               &ctx->copy_scalar_from_dev_total_runtime)));
        if (ctx->failure_is_an_option && futhark_context_sync(ctx) != 0)
            return 1;
        
        int32_t res_8897 = read_res_9675;
        
        if (ctx->debugging)
            fprintf(stderr, "%s\n", "\n# SegMap");
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9129, 1,
                                                sizeof(n_8851), &n_8851));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9129, 2,
                                                sizeof(res_8897), &res_8897));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9129, 3,
                                                sizeof(xs_mem_9228.mem),
                                                &xs_mem_9228.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9129, 4,
                                                sizeof(xs_mem_9229.mem),
                                                &xs_mem_9229.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9129, 5,
                                                sizeof(mem_9232.mem),
                                                &mem_9232.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9129, 6,
                                                sizeof(mem_9236.mem),
                                                &mem_9236.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9129, 7,
                                                sizeof(mem_9239.mem),
                                                &mem_9239.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9129, 8,
                                                sizeof(mem_9242.mem),
                                                &mem_9242.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9129, 9,
                                                sizeof(mem_9251.mem),
                                                &mem_9251.mem));
        OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9129, 10,
                                                sizeof(mem_9256.mem),
                                                &mem_9256.mem));
        if (1 * (segmap_usable_groups_9139 * segmap_group_sizze_9133) != 0) {
            const size_t global_work_sizze_9676[1] =
                         {segmap_usable_groups_9139 * segmap_group_sizze_9133};
            const size_t local_work_sizze_9680[1] = {segmap_group_sizze_9133};
            int64_t time_start_9677 = 0, time_end_9678 = 0;
            
            if (ctx->debugging) {
                fprintf(stderr, "Launching %s with global work size [",
                        "segmap_9129");
                fprintf(stderr, "%zu", global_work_sizze_9676[0]);
                fprintf(stderr, "] and local work size [");
                fprintf(stderr, "%zu", local_work_sizze_9680[0]);
                fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                        (int) 0);
                time_start_9677 = get_wall_time();
            }
            OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                            ctx->segmap_9129, 1,
                                                            NULL,
                                                            global_work_sizze_9676,
                                                            local_work_sizze_9680,
                                                            0, NULL,
                                                            ctx->profiling_paused ||
                                                            !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                      &ctx->segmap_9129_runs,
                                                                                                      &ctx->segmap_9129_total_runtime)));
            if (ctx->debugging) {
                OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
                time_end_9678 = get_wall_time();
                
                long time_diff_9679 = time_end_9678 - time_start_9677;
                
                fprintf(stderr, "kernel %s runtime: %ldus\n", "segmap_9129",
                        time_diff_9679);
            }
        }
        
        struct memblock_device mem_9259;
        
        mem_9259.references = NULL;
        if (memblock_alloc_device(ctx, &mem_9259, bytes_9225, "mem_9259"))
            return 1;
        if (mul64(sext_i32_i64(n_8851), (int32_t) sizeof(int32_t)) > 0) {
            OPENCL_SUCCEED_OR_RETURN(clEnqueueCopyBuffer(ctx->opencl.queue,
                                                         mem_9256.mem,
                                                         mem_9259.mem,
                                                         mul32(mul32(0, n_8851),
                                                               4), 0,
                                                         mul64(sext_i32_i64(n_8851),
                                                               (int32_t) sizeof(int32_t)),
                                                         0, NULL,
                                                         ctx->profiling_paused ||
                                                         !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                   &ctx->copy_dev_to_dev_runs,
                                                                                                   &ctx->copy_dev_to_dev_total_runtime)));
            if (ctx->debugging)
                OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
        }
        
        struct memblock_device mem_9263;
        
        mem_9263.references = NULL;
        if (memblock_alloc_device(ctx, &mem_9263, bytes_9225, "mem_9263"))
            return 1;
        if (mul64(sext_i32_i64(n_8851), (int32_t) sizeof(int32_t)) > 0) {
            OPENCL_SUCCEED_OR_RETURN(clEnqueueCopyBuffer(ctx->opencl.queue,
                                                         mem_9251.mem,
                                                         mem_9263.mem,
                                                         mul32(mul32(0, n_8851),
                                                               4), 0,
                                                         mul64(sext_i32_i64(n_8851),
                                                               (int32_t) sizeof(int32_t)),
                                                         0, NULL,
                                                         ctx->profiling_paused ||
                                                         !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                   &ctx->copy_dev_to_dev_runs,
                                                                                                   &ctx->copy_dev_to_dev_total_runtime)));
            if (ctx->debugging)
                OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
        }
        
        struct memblock_device xs_mem_tmp_9328;
        
        xs_mem_tmp_9328.references = NULL;
        if (memblock_set_device(ctx, &xs_mem_tmp_9328, &mem_9259, "mem_9259") !=
            0)
            return 1;
        
        struct memblock_device xs_mem_tmp_9329;
        
        xs_mem_tmp_9329.references = NULL;
        if (memblock_set_device(ctx, &xs_mem_tmp_9329, &mem_9263, "mem_9263") !=
            0)
            return 1;
        if (memblock_set_device(ctx, &xs_mem_9228, &xs_mem_tmp_9328,
                                "xs_mem_tmp_9328") != 0)
            return 1;
        if (memblock_set_device(ctx, &xs_mem_9229, &xs_mem_tmp_9329,
                                "xs_mem_tmp_9329") != 0)
            return 1;
        if (memblock_unref_device(ctx, &xs_mem_tmp_9329, "xs_mem_tmp_9329") !=
            0)
            return 1;
        if (memblock_unref_device(ctx, &xs_mem_tmp_9328, "xs_mem_tmp_9328") !=
            0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9263, "mem_9263") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9259, "mem_9259") != 0)
            return 1;
        if (memblock_unref_device(ctx, &group_res_arr_mem_9410,
                                  "group_res_arr_mem_9410") != 0)
            return 1;
    }
    if (memblock_set_device(ctx, &res_mem_9265, &xs_mem_9228, "xs_mem_9228") !=
        0)
        return 1;
    if (memblock_set_device(ctx, &res_mem_9266, &xs_mem_9229, "xs_mem_9229") !=
        0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9227, "mem_9227") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9232, "mem_9232") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9236, "mem_9236") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9239, "mem_9239") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9242, "mem_9242") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9246, "mem_9246") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9251, "mem_9251") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9256, "mem_9256") != 0)
        return 1;
    
    int32_t segmap_group_sizze_9145;
    
    segmap_group_sizze_9145 = ctx->sizes.mainzisegmap_group_sizze_9144;
    
    int64_t segmap_group_sizze_9146 = sext_i32_i64(segmap_group_sizze_9145);
    int64_t y_9147 = sub64(segmap_group_sizze_9146, 1);
    int64_t x_9148 = add64(y_9147, binop_x_9226);
    int64_t segmap_usable_groups_64_9150 = squot64(x_9148,
                                                   segmap_group_sizze_9146);
    int32_t segmap_usable_groups_9151 =
            sext_i64_i32(segmap_usable_groups_64_9150);
    struct memblock_device mem_9269;
    
    mem_9269.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9269, bytes_9225, "mem_9269"))
        return 1;
    
    struct memblock_device mem_9272;
    
    mem_9272.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9272, bytes_9225, "mem_9272"))
        return 1;
    if (ctx->debugging)
        fprintf(stderr, "%s\n", "\n# SegMap");
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9141, 1, sizeof(n_8851),
                                            &n_8851));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9141, 2,
                                            sizeof(k_mem_9223.mem),
                                            &k_mem_9223.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9141, 3,
                                            sizeof(d_mem_9224.mem),
                                            &d_mem_9224.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9141, 4,
                                            sizeof(res_mem_9266.mem),
                                            &res_mem_9266.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9141, 5,
                                            sizeof(mem_9269.mem),
                                            &mem_9269.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9141, 6,
                                            sizeof(mem_9272.mem),
                                            &mem_9272.mem));
    if (1 * (segmap_usable_groups_9151 * segmap_group_sizze_9145) != 0) {
        const size_t global_work_sizze_9681[1] = {segmap_usable_groups_9151 *
                     segmap_group_sizze_9145};
        const size_t local_work_sizze_9685[1] = {segmap_group_sizze_9145};
        int64_t time_start_9682 = 0, time_end_9683 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "segmap_9141");
            fprintf(stderr, "%zu", global_work_sizze_9681[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_9685[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) 0);
            time_start_9682 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->segmap_9141, 1,
                                                        NULL,
                                                        global_work_sizze_9681,
                                                        local_work_sizze_9685,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->segmap_9141_runs,
                                                                                                  &ctx->segmap_9141_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_9683 = get_wall_time();
            
            long time_diff_9684 = time_end_9683 - time_start_9682;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "segmap_9141",
                    time_diff_9684);
        }
    }
    if (memblock_unref_device(ctx, &res_mem_9266, "res_mem_9266") != 0)
        return 1;
    
    int32_t rotate_arg_8927 = sub32(n_8851, 1);
    int32_t segscan_group_sizze_9158;
    
    segscan_group_sizze_9158 = ctx->sizes.mainzisegscan_group_sizze_9157;
    
    int32_t num_groups_9160;
    int32_t max_num_groups_9444;
    
    max_num_groups_9444 = ctx->sizes.mainzisegscan_num_groups_9159;
    num_groups_9160 = sext_i64_i32(smax64(1,
                                          smin64(squot64(sub64(add64(binop_x_9226,
                                                                     sext_i32_i64(segscan_group_sizze_9158)),
                                                               1),
                                                         sext_i32_i64(segscan_group_sizze_9158)),
                                                 sext_i32_i64(max_num_groups_9444))));
    
    struct memblock_device mem_9276;
    
    mem_9276.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9276, bytes_9225, "mem_9276"))
        return 1;
    
    struct memblock_device mem_9278;
    
    mem_9278.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9278, binop_x_9226, "mem_9278"))
        return 1;
    if (ctx->debugging)
        fprintf(stderr, "%s\n", "\n# SegScan");
    
    int32_t num_threads_9445 = mul32(num_groups_9160, segscan_group_sizze_9158);
    
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9163, 1,
                                            mul32((int32_t) sizeof(bool),
                                                  segscan_group_sizze_9158),
                                            NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9163, 2,
                                            mul32((int32_t) sizeof(int32_t),
                                                  segscan_group_sizze_9158),
                                            NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9163, 3,
                                            sizeof(n_8851), &n_8851));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9163, 4,
                                            sizeof(rotate_arg_8927),
                                            &rotate_arg_8927));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9163, 5,
                                            sizeof(mem_9269.mem),
                                            &mem_9269.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9163, 6,
                                            sizeof(mem_9272.mem),
                                            &mem_9272.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9163, 7,
                                            sizeof(mem_9276.mem),
                                            &mem_9276.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9163, 8,
                                            sizeof(mem_9278.mem),
                                            &mem_9278.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9163, 9,
                                            sizeof(num_threads_9445),
                                            &num_threads_9445));
    if (1 * (num_groups_9160 * segscan_group_sizze_9158) != 0) {
        const size_t global_work_sizze_9686[1] = {num_groups_9160 *
                     segscan_group_sizze_9158};
        const size_t local_work_sizze_9690[1] = {segscan_group_sizze_9158};
        int64_t time_start_9687 = 0, time_end_9688 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage1_9163");
            fprintf(stderr, "%zu", global_work_sizze_9686[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_9690[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) (0 + mul32((int32_t) sizeof(bool),
                                     segscan_group_sizze_9158) +
                           mul32((int32_t) sizeof(int32_t),
                                 segscan_group_sizze_9158)));
            time_start_9687 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage1_9163,
                                                        1, NULL,
                                                        global_work_sizze_9686,
                                                        local_work_sizze_9690,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage1_9163_runs,
                                                                                                  &ctx->scan_stage1_9163_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_9688 = get_wall_time();
            
            long time_diff_9689 = time_end_9688 - time_start_9687;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage1_9163",
                    time_diff_9689);
        }
    }
    if (ctx->debugging)
        fprintf(stderr, "%s: %llu%c", "elems_per_group",
                (long long) mul32(segscan_group_sizze_9158,
                                  squot32(sub32(add32(n_8851, num_threads_9445),
                                                1), num_threads_9445)), '\n');
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9163, 1,
                                            mul32((int32_t) sizeof(bool),
                                                  num_groups_9160), NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9163, 2,
                                            mul32((int32_t) sizeof(int32_t),
                                                  num_groups_9160), NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9163, 3,
                                            sizeof(n_8851), &n_8851));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9163, 4,
                                            sizeof(num_groups_9160),
                                            &num_groups_9160));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9163, 5,
                                            sizeof(mem_9276.mem),
                                            &mem_9276.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9163, 6,
                                            sizeof(mem_9278.mem),
                                            &mem_9278.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9163, 7,
                                            sizeof(num_threads_9445),
                                            &num_threads_9445));
    if (1 * (1 * num_groups_9160) != 0) {
        const size_t global_work_sizze_9691[1] = {1 * num_groups_9160};
        const size_t local_work_sizze_9695[1] = {num_groups_9160};
        int64_t time_start_9692 = 0, time_end_9693 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage2_9163");
            fprintf(stderr, "%zu", global_work_sizze_9691[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_9695[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) (0 + mul32((int32_t) sizeof(bool), num_groups_9160) +
                           mul32((int32_t) sizeof(int32_t), num_groups_9160)));
            time_start_9692 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage2_9163,
                                                        1, NULL,
                                                        global_work_sizze_9691,
                                                        local_work_sizze_9695,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage2_9163_runs,
                                                                                                  &ctx->scan_stage2_9163_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_9693 = get_wall_time();
            
            long time_diff_9694 = time_end_9693 - time_start_9692;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage2_9163",
                    time_diff_9694);
        }
    }
    
    int32_t required_groups_9519 = squot32(sub32(add32(n_8851,
                                                       segscan_group_sizze_9158),
                                                 1), segscan_group_sizze_9158);
    
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9163, 1,
                                            sizeof(n_8851), &n_8851));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9163, 2,
                                            sizeof(num_groups_9160),
                                            &num_groups_9160));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9163, 3,
                                            sizeof(mem_9276.mem),
                                            &mem_9276.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9163, 4,
                                            sizeof(mem_9278.mem),
                                            &mem_9278.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9163, 5,
                                            sizeof(num_threads_9445),
                                            &num_threads_9445));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9163, 6,
                                            sizeof(required_groups_9519),
                                            &required_groups_9519));
    if (1 * (num_groups_9160 * segscan_group_sizze_9158) != 0) {
        const size_t global_work_sizze_9696[1] = {num_groups_9160 *
                     segscan_group_sizze_9158};
        const size_t local_work_sizze_9700[1] = {segscan_group_sizze_9158};
        int64_t time_start_9697 = 0, time_end_9698 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage3_9163");
            fprintf(stderr, "%zu", global_work_sizze_9696[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_9700[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) 0);
            time_start_9697 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage3_9163,
                                                        1, NULL,
                                                        global_work_sizze_9696,
                                                        local_work_sizze_9700,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage3_9163_runs,
                                                                                                  &ctx->scan_stage3_9163_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_9698 = get_wall_time();
            
            long time_diff_9699 = time_end_9698 - time_start_9697;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage3_9163",
                    time_diff_9699);
        }
    }
    if (memblock_unref_device(ctx, &mem_9278, "mem_9278") != 0)
        return 1;
    
    int32_t segscan_group_sizze_9167;
    
    segscan_group_sizze_9167 = ctx->sizes.mainzisegscan_group_sizze_9166;
    
    int32_t num_groups_9169;
    int32_t max_num_groups_9531;
    
    max_num_groups_9531 = ctx->sizes.mainzisegscan_num_groups_9168;
    num_groups_9169 = sext_i64_i32(smax64(1,
                                          smin64(squot64(sub64(add64(binop_x_9226,
                                                                     sext_i32_i64(segscan_group_sizze_9167)),
                                                               1),
                                                         sext_i32_i64(segscan_group_sizze_9167)),
                                                 sext_i32_i64(max_num_groups_9531))));
    
    struct memblock_device mem_9282;
    
    mem_9282.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9282, bytes_9225, "mem_9282"))
        return 1;
    
    struct memblock_device mem_9285;
    
    mem_9285.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9285, bytes_9225, "mem_9285"))
        return 1;
    
    struct memblock_device mem_9287;
    
    mem_9287.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9287, binop_x_9226, "mem_9287"))
        return 1;
    
    struct memblock_device mem_9290;
    
    mem_9290.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9290, bytes_9225, "mem_9290"))
        return 1;
    if (ctx->debugging)
        fprintf(stderr, "%s\n", "\n# SegScan");
    
    int32_t num_threads_9532 = mul32(num_groups_9169, segscan_group_sizze_9167);
    
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 1,
                                            mul32((int32_t) sizeof(bool),
                                                  segscan_group_sizze_9167),
                                            NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 2,
                                            mul32((int32_t) sizeof(int32_t),
                                                  segscan_group_sizze_9167),
                                            NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 3,
                                            mul32((int32_t) sizeof(int32_t),
                                                  segscan_group_sizze_9167),
                                            NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 4,
                                            sizeof(n_8851), &n_8851));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 5,
                                            sizeof(rotate_arg_8927),
                                            &rotate_arg_8927));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 6,
                                            sizeof(mem_9269.mem),
                                            &mem_9269.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 7,
                                            sizeof(mem_9272.mem),
                                            &mem_9272.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 8,
                                            sizeof(mem_9276.mem),
                                            &mem_9276.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 9,
                                            sizeof(mem_9282.mem),
                                            &mem_9282.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 10,
                                            sizeof(mem_9285.mem),
                                            &mem_9285.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 11,
                                            sizeof(mem_9287.mem),
                                            &mem_9287.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 12,
                                            sizeof(mem_9290.mem),
                                            &mem_9290.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage1_9172, 13,
                                            sizeof(num_threads_9532),
                                            &num_threads_9532));
    if (1 * (num_groups_9169 * segscan_group_sizze_9167) != 0) {
        const size_t global_work_sizze_9701[1] = {num_groups_9169 *
                     segscan_group_sizze_9167};
        const size_t local_work_sizze_9705[1] = {segscan_group_sizze_9167};
        int64_t time_start_9702 = 0, time_end_9703 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage1_9172");
            fprintf(stderr, "%zu", global_work_sizze_9701[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_9705[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) (0 + mul32((int32_t) sizeof(bool),
                                     segscan_group_sizze_9167) +
                           mul32((int32_t) sizeof(int32_t),
                                 segscan_group_sizze_9167) +
                           mul32((int32_t) sizeof(int32_t),
                                 segscan_group_sizze_9167)));
            time_start_9702 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage1_9172,
                                                        1, NULL,
                                                        global_work_sizze_9701,
                                                        local_work_sizze_9705,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage1_9172_runs,
                                                                                                  &ctx->scan_stage1_9172_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_9703 = get_wall_time();
            
            long time_diff_9704 = time_end_9703 - time_start_9702;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage1_9172",
                    time_diff_9704);
        }
    }
    if (ctx->debugging)
        fprintf(stderr, "%s: %llu%c", "elems_per_group",
                (long long) mul32(segscan_group_sizze_9167,
                                  squot32(sub32(add32(n_8851, num_threads_9532),
                                                1), num_threads_9532)), '\n');
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9172, 1,
                                            mul32((int32_t) sizeof(bool),
                                                  num_groups_9169), NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9172, 2,
                                            mul32((int32_t) sizeof(int32_t),
                                                  num_groups_9169), NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9172, 3,
                                            mul32((int32_t) sizeof(int32_t),
                                                  num_groups_9169), NULL));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9172, 4,
                                            sizeof(n_8851), &n_8851));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9172, 5,
                                            sizeof(num_groups_9169),
                                            &num_groups_9169));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9172, 6,
                                            sizeof(mem_9282.mem),
                                            &mem_9282.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9172, 7,
                                            sizeof(mem_9285.mem),
                                            &mem_9285.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9172, 8,
                                            sizeof(mem_9287.mem),
                                            &mem_9287.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage2_9172, 9,
                                            sizeof(num_threads_9532),
                                            &num_threads_9532));
    if (1 * (1 * num_groups_9169) != 0) {
        const size_t global_work_sizze_9706[1] = {1 * num_groups_9169};
        const size_t local_work_sizze_9710[1] = {num_groups_9169};
        int64_t time_start_9707 = 0, time_end_9708 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage2_9172");
            fprintf(stderr, "%zu", global_work_sizze_9706[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_9710[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) (0 + mul32((int32_t) sizeof(bool), num_groups_9169) +
                           mul32((int32_t) sizeof(int32_t), num_groups_9169) +
                           mul32((int32_t) sizeof(int32_t), num_groups_9169)));
            time_start_9707 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage2_9172,
                                                        1, NULL,
                                                        global_work_sizze_9706,
                                                        local_work_sizze_9710,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage2_9172_runs,
                                                                                                  &ctx->scan_stage2_9172_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_9708 = get_wall_time();
            
            long time_diff_9709 = time_end_9708 - time_start_9707;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage2_9172",
                    time_diff_9709);
        }
    }
    
    int32_t required_groups_9625 = squot32(sub32(add32(n_8851,
                                                       segscan_group_sizze_9167),
                                                 1), segscan_group_sizze_9167);
    
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9172, 1,
                                            sizeof(n_8851), &n_8851));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9172, 2,
                                            sizeof(num_groups_9169),
                                            &num_groups_9169));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9172, 3,
                                            sizeof(mem_9282.mem),
                                            &mem_9282.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9172, 4,
                                            sizeof(mem_9285.mem),
                                            &mem_9285.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9172, 5,
                                            sizeof(mem_9287.mem),
                                            &mem_9287.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9172, 6,
                                            sizeof(num_threads_9532),
                                            &num_threads_9532));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->scan_stage3_9172, 7,
                                            sizeof(required_groups_9625),
                                            &required_groups_9625));
    if (1 * (num_groups_9169 * segscan_group_sizze_9167) != 0) {
        const size_t global_work_sizze_9711[1] = {num_groups_9169 *
                     segscan_group_sizze_9167};
        const size_t local_work_sizze_9715[1] = {segscan_group_sizze_9167};
        int64_t time_start_9712 = 0, time_end_9713 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "scan_stage3_9172");
            fprintf(stderr, "%zu", global_work_sizze_9711[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_9715[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) 0);
            time_start_9712 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->scan_stage3_9172,
                                                        1, NULL,
                                                        global_work_sizze_9711,
                                                        local_work_sizze_9715,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->scan_stage3_9172_runs,
                                                                                                  &ctx->scan_stage3_9172_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_9713 = get_wall_time();
            
            long time_diff_9714 = time_end_9713 - time_start_9712;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "scan_stage3_9172",
                    time_diff_9714);
        }
    }
    if (memblock_unref_device(ctx, &mem_9269, "mem_9269") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9272, "mem_9272") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9276, "mem_9276") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9287, "mem_9287") != 0)
        return 1;
    
    int32_t segmap_group_sizze_9199;
    
    segmap_group_sizze_9199 = ctx->sizes.mainzisegmap_group_sizze_9198;
    
    int64_t segmap_group_sizze_9200 = sext_i32_i64(segmap_group_sizze_9199);
    int64_t y_9201 = sub64(segmap_group_sizze_9200, 1);
    int64_t x_9202 = add64(y_9201, binop_x_9226);
    int64_t segmap_usable_groups_64_9204 = squot64(x_9202,
                                                   segmap_group_sizze_9200);
    int32_t segmap_usable_groups_9205 =
            sext_i64_i32(segmap_usable_groups_64_9204);
    
    if (ctx->debugging)
        fprintf(stderr, "%s\n", "\n# SegMap");
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9195, 1, sizeof(n_8851),
                                            &n_8851));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9195, 2,
                                            sizeof(mem_9282.mem),
                                            &mem_9282.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9195, 3,
                                            sizeof(mem_9285.mem),
                                            &mem_9285.mem));
    OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->segmap_9195, 4,
                                            sizeof(mem_9290.mem),
                                            &mem_9290.mem));
    if (1 * (segmap_usable_groups_9205 * segmap_group_sizze_9199) != 0) {
        const size_t global_work_sizze_9716[1] = {segmap_usable_groups_9205 *
                     segmap_group_sizze_9199};
        const size_t local_work_sizze_9720[1] = {segmap_group_sizze_9199};
        int64_t time_start_9717 = 0, time_end_9718 = 0;
        
        if (ctx->debugging) {
            fprintf(stderr, "Launching %s with global work size [",
                    "segmap_9195");
            fprintf(stderr, "%zu", global_work_sizze_9716[0]);
            fprintf(stderr, "] and local work size [");
            fprintf(stderr, "%zu", local_work_sizze_9720[0]);
            fprintf(stderr, "]; local memory parameters sum to %d bytes.\n",
                    (int) 0);
            time_start_9717 = get_wall_time();
        }
        OPENCL_SUCCEED_OR_RETURN(clEnqueueNDRangeKernel(ctx->opencl.queue,
                                                        ctx->segmap_9195, 1,
                                                        NULL,
                                                        global_work_sizze_9716,
                                                        local_work_sizze_9720,
                                                        0, NULL,
                                                        ctx->profiling_paused ||
                                                        !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                                  &ctx->segmap_9195_runs,
                                                                                                  &ctx->segmap_9195_total_runtime)));
        if (ctx->debugging) {
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
            time_end_9718 = get_wall_time();
            
            long time_diff_9719 = time_end_9718 - time_start_9717;
            
            fprintf(stderr, "kernel %s runtime: %ldus\n", "segmap_9195",
                    time_diff_9719);
        }
    }
    if (memblock_unref_device(ctx, &mem_9285, "mem_9285") != 0)
        return 1;
    
    bool x_9076 = sle32(0, rotate_arg_8927);
    bool index_certs_9079;
    
    if (!x_9076) {
        ctx->error = msgprintf("Error: %s%d%s%d%s\n\nBacktrace:\n%s", "Index [",
                               rotate_arg_8927,
                               "] out of bounds for array of shape [", n_8851,
                               "].",
                               "-> #0  reduce_by_index.fut:33:44-70\n   #1  reduce_by_index.fut:42:8-35\n   #2  reduce_by_index.fut:45:37-61\n   #3  reduce_by_index.fut:45:1-61\n");
        if (memblock_unref_device(ctx, &mem_9290, "mem_9290") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9287, "mem_9287") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9285, "mem_9285") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9282, "mem_9282") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9278, "mem_9278") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9276, "mem_9276") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9272, "mem_9272") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9269, "mem_9269") != 0)
            return 1;
        if (memblock_unref_device(ctx, &xs_mem_9229, "xs_mem_9229") != 0)
            return 1;
        if (memblock_unref_device(ctx, &xs_mem_9228, "xs_mem_9228") != 0)
            return 1;
        if (memblock_unref_device(ctx, &res_mem_9266, "res_mem_9266") != 0)
            return 1;
        if (memblock_unref_device(ctx, &res_mem_9265, "res_mem_9265") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9256, "mem_9256") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9251, "mem_9251") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9246, "mem_9246") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9242, "mem_9242") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9239, "mem_9239") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9236, "mem_9236") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9232, "mem_9232") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9227, "mem_9227") != 0)
            return 1;
        if (memblock_unref_device(ctx, &out_mem_9319, "out_mem_9319") != 0)
            return 1;
        return 1;
    }
    
    int32_t read_res_9721;
    
    OPENCL_SUCCEED_OR_RETURN(clEnqueueReadBuffer(ctx->opencl.queue,
                                                 mem_9282.mem,
                                                 ctx->failure_is_an_option ? CL_FALSE : CL_TRUE,
                                                 rotate_arg_8927 *
                                                 sizeof(int32_t),
                                                 sizeof(int32_t),
                                                 &read_res_9721, 0, NULL,
                                                 ctx->profiling_paused ||
                                                 !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                           &ctx->copy_scalar_from_dev_runs,
                                                                                           &ctx->copy_scalar_from_dev_total_runtime)));
    if (ctx->failure_is_an_option && futhark_context_sync(ctx) != 0)
        return 1;
    
    int32_t x_9080 = read_res_9721;
    
    if (memblock_unref_device(ctx, &mem_9282, "mem_9282") != 0)
        return 1;
    
    int32_t j_9081 = add32(1, x_9080);
    bool empty_slice_9082 = j_9081 == 0;
    bool zzero_leq_i_p_m_t_s_9083 = sle32(0, x_9080);
    bool i_p_m_t_s_leq_w_9084 = slt32(x_9080, n_8851);
    bool i_lte_j_9085 = sle32(0, j_9081);
    bool y_9086 = zzero_leq_i_p_m_t_s_9083 && i_p_m_t_s_leq_w_9084;
    bool y_9087 = i_lte_j_9085 && y_9086;
    bool ok_or_empty_9088 = empty_slice_9082 || y_9087;
    bool index_certs_9089;
    
    if (!ok_or_empty_9088) {
        ctx->error = msgprintf("Error: %s%d%s%d%s%d%s\n\nBacktrace:\n%s",
                               "Index [", 0, ":", j_9081,
                               "] out of bounds for array of shape [", n_8851,
                               "].",
                               "-> #0  reduce_by_index.fut:33:8-74\n   #1  reduce_by_index.fut:42:8-35\n   #2  reduce_by_index.fut:45:37-61\n   #3  reduce_by_index.fut:45:1-61\n");
        if (memblock_unref_device(ctx, &mem_9290, "mem_9290") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9287, "mem_9287") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9285, "mem_9285") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9282, "mem_9282") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9278, "mem_9278") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9276, "mem_9276") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9272, "mem_9272") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9269, "mem_9269") != 0)
            return 1;
        if (memblock_unref_device(ctx, &xs_mem_9229, "xs_mem_9229") != 0)
            return 1;
        if (memblock_unref_device(ctx, &xs_mem_9228, "xs_mem_9228") != 0)
            return 1;
        if (memblock_unref_device(ctx, &res_mem_9266, "res_mem_9266") != 0)
            return 1;
        if (memblock_unref_device(ctx, &res_mem_9265, "res_mem_9265") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9256, "mem_9256") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9251, "mem_9251") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9246, "mem_9246") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9242, "mem_9242") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9239, "mem_9239") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9236, "mem_9236") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9232, "mem_9232") != 0)
            return 1;
        if (memblock_unref_device(ctx, &mem_9227, "mem_9227") != 0)
            return 1;
        if (memblock_unref_device(ctx, &out_mem_9319, "out_mem_9319") != 0)
            return 1;
        return 1;
    }
    
    int64_t binop_x_9292 = sext_i32_i64(j_9081);
    int64_t bytes_9291 = mul64(4, binop_x_9292);
    struct memblock_device mem_9293;
    
    mem_9293.references = NULL;
    if (memblock_alloc_device(ctx, &mem_9293, bytes_9291, "mem_9293"))
        return 1;
    if (mul64(sext_i32_i64(j_9081), (int32_t) sizeof(int32_t)) > 0) {
        OPENCL_SUCCEED_OR_RETURN(clEnqueueCopyBuffer(ctx->opencl.queue,
                                                     mem_9290.mem, mem_9293.mem,
                                                     0, 0,
                                                     mul64(sext_i32_i64(j_9081),
                                                           (int32_t) sizeof(int32_t)),
                                                     0, NULL,
                                                     ctx->profiling_paused ||
                                                     !ctx->profiling ? NULL : opencl_get_event(&ctx->opencl,
                                                                                               &ctx->copy_dev_to_dev_runs,
                                                                                               &ctx->copy_dev_to_dev_total_runtime)));
        if (ctx->debugging)
            OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
    }
    if (memblock_unref_device(ctx, &mem_9290, "mem_9290") != 0)
        return 1;
    out_arrsizze_9320 = j_9081;
    if (memblock_set_device(ctx, &out_mem_9319, &mem_9293, "mem_9293") != 0)
        return 1;
    (*out_mem_p_9642).references = NULL;
    if (memblock_set_device(ctx, &*out_mem_p_9642, &out_mem_9319,
                            "out_mem_9319") != 0)
        return 1;
    *out_out_arrsizze_9643 = out_arrsizze_9320;
    if (memblock_unref_device(ctx, &mem_9293, "mem_9293") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9290, "mem_9290") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9287, "mem_9287") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9285, "mem_9285") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9282, "mem_9282") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9278, "mem_9278") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9276, "mem_9276") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9272, "mem_9272") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9269, "mem_9269") != 0)
        return 1;
    if (memblock_unref_device(ctx, &xs_mem_9229, "xs_mem_9229") != 0)
        return 1;
    if (memblock_unref_device(ctx, &xs_mem_9228, "xs_mem_9228") != 0)
        return 1;
    if (memblock_unref_device(ctx, &res_mem_9266, "res_mem_9266") != 0)
        return 1;
    if (memblock_unref_device(ctx, &res_mem_9265, "res_mem_9265") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9256, "mem_9256") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9251, "mem_9251") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9246, "mem_9246") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9242, "mem_9242") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9239, "mem_9239") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9236, "mem_9236") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9232, "mem_9232") != 0)
        return 1;
    if (memblock_unref_device(ctx, &mem_9227, "mem_9227") != 0)
        return 1;
    if (memblock_unref_device(ctx, &out_mem_9319, "out_mem_9319") != 0)
        return 1;
    return 0;
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
                       struct futhark_i32_1d *in1)
{
    struct memblock_device k_mem_9223;
    
    k_mem_9223.references = NULL;
    
    struct memblock_device d_mem_9224;
    
    d_mem_9224.references = NULL;
    
    int32_t n_8851;
    int32_t n_8852;
    struct memblock_device out_mem_9319;
    
    out_mem_9319.references = NULL;
    
    int32_t out_arrsizze_9320;
    
    lock_lock(&ctx->lock);
    k_mem_9223 = in0->mem;
    n_8851 = in0->shape[0];
    d_mem_9224 = in1->mem;
    n_8852 = in1->shape[0];
    
    int ret = futrts_main(ctx, &out_mem_9319, &out_arrsizze_9320, k_mem_9223,
                          d_mem_9224, n_8851, n_8852);
    
    if (ret == 0) {
        assert((*out0 =
                (struct futhark_i32_1d *) malloc(sizeof(struct futhark_i32_1d))) !=
            NULL);
        (*out0)->mem = out_mem_9319;
        (*out0)->shape[0] = out_arrsizze_9320;
    }
    lock_unlock(&ctx->lock);
    return ret;
}
