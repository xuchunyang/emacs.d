/* GNU Emacs module of TagLib */

#include <string.h>
#include <stdlib.h>
#include <tag_c.h>
#include <emacs-module.h>

int plugin_is_GPL_compatible;

static emacs_value
Ftaglib_core_return_t (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                       void *data)
{

  return env->intern (env, "t");
}

/* TODO: Create alist to represent various slots in a Tag */
static emacs_value
Ftaglib_core_tag_title (emacs_env *env, ptrdiff_t nargs, emacs_value args[],
                        void *data)
{
  emacs_value lisp_str = args[0];
  ptrdiff_t size = 0;
  char * buf = NULL;

  env->copy_string_contents (env, lisp_str, buf, &size);
  buf = malloc (size);
  env->copy_string_contents (env, lisp_str, buf, &size);

  TagLib_File * file = taglib_file_new (buf);
  TagLib_Tag * tag = taglib_file_tag (file);
  char * title = taglib_tag_title (tag);
  emacs_value rtv = env->make_string (env, title, strlen (title) - 1);

  taglib_file_free (file);
  return rtv;
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
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

/* Module init function.  */
int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);

#define DEFUN(lsym, csym, amin, amax, doc, data)        \
  bind_function (env, lsym, \
                 env->make_function (env, amin, amax, csym, doc, data))

  DEFUN ("taglib-core-return-t", Ftaglib_core_return_t, 0, 0,
         "Always return t (for testing).", NULL);
  DEFUN ("taglib-core-tag-title", Ftaglib_core_tag_title, 1, 1,
         "Get Title from file.", NULL);

#undef DEFUN

  provide (env, "taglib-core");
  return 0;
}
