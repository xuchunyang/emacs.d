#include <emacs-module.h>
int plugin_is_GPL_compatible;

#include <pthread.h>
#include <unistd.h>
#include <string.h>

int counter = 0;

static void *
perform_work (void *args)
{
  sleep (1);
  counter++;
  return 0;
}

static void
error (emacs_env *env, char *msg)
{
  env->non_local_exit_signal (env, env->intern (env, "error"),
			      env->make_string (env, msg, strlen (msg)));
}

static emacs_value
Ftest (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  pthread_t thread;
  int result_code;
  
  result_code = pthread_create (&thread, NULL, perform_work, NULL);
  if (result_code)
    error (env, "pthread_create failed");
  result_code = pthread_join (thread, NULL);
  if (result_code)
    error (env, "pthread_join failed");

  return env->make_integer (env, counter);
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

  DEFUN ("pthread-module-test", Ftest, 0, 0, NULL, NULL);
#undef DEFUN

  provide (env, "pthread-module");
  return 0;
}

/* Local Variables: */
/* compile-command: "cc -lpthread -Wall -shared -fpic pthread-module.c -o pthread-module.so" */
/* End: */
