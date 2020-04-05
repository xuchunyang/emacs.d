#include <emacs-module.h>

int plugin_is_GPL_compatible;

#include <libguile.h>

struct box
{
  void *x;
  void *y;
};

static void *
guile_float_to_bytes_1 (void *data)
{
  struct box *b = (struct box *) data;
  double f = *((double *) b->x);
  uint8_t *p = (uint8_t *) b->y;
  SCM bv = scm_c_make_bytevector (4);
  scm_bytevector_ieee_single_set_x (bv,
                                    scm_from_int (0),
                                    scm_from_double (f),
                                    scm_endianness_big);
  
  for (int i = 0; i < 4; i++)
    p[i] = scm_c_bytevector_ref (bv, i);
  return NULL;
}

static void
guile_float_to_bytes (double f, uint8_t *bytes)
{
  /* XXX try two element array */
  struct box b = { &f, bytes };
  scm_with_guile (guile_float_to_bytes_1, &b);
}

static emacs_value
Ffloat_to_bytes
(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  double f = env->extract_float (env, args[0]);
  uint8_t bytes[4] = { 0 };
  guile_float_to_bytes (f, bytes);
  {
    emacs_value Qlist = env->intern (env, "list");
    emacs_value args[] =
      {
        env->make_integer (env, bytes[0]),
        env->make_integer (env, bytes[1]),
        env->make_integer (env, bytes[2]),
        env->make_integer (env, bytes[3]),
      };
    int argc =  sizeof (args) / sizeof (emacs_value);
    return env->funcall (env, Qlist, argc, args);
  }
}

/* Lisp utilities for easier readability (simple wrappers).  */

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qdefalias = env->intern (env, "defalias");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qdefalias, 2, args);
}

/* Module init function.  */
int
emacs_module_init (struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment (runtime);

#define DEFUN(lsym, csym, amin, amax, doc, data)                        \
  bind_function (env, lsym,                                             \
		 env->make_function (env, amin, amax, csym, doc, data))

  DEFUN ("guile-float-to-bytes", Ffloat_to_bytes, 1, 1, NULL, NULL);
#undef DEFUN

  provide (env, "guile-module");
  return 0;
}

/* Local Variables: */
/* compile-command: "cc -Wall -shared -fpic `pkg-config --cflags --libs guile-2.2` guile-module.c -o guile-module.so" */
/* End: */
