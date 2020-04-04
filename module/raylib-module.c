#include <emacs-module.h>
int plugin_is_GPL_compatible;

#include <raylib.h>
/* XXX When I close the window, Emacs is no longer functional */

/*
 * When I close the window, Emacs is no longer functional
 * ~/src/emacs/src/emacs -Q -nw -l raylib-module.so -f raylib-module-basic-window
 *
 * ~/src/emacs/src/emacs -Q -nw -l raylib-module.so --eval "(thread-join (make-thread #'raylib-module-basic-window))"
 * emacs[63535:1014636] *** Terminating app due to uncaught exception
 * 'NSInternalInconsistencyException', reason: '+[NSUndoManager(NSInternal)
 * _endTopLevelGroupings] is only safe to invoke on the main thread.'
 */
static emacs_value
Fbasic_window (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  InitWindow (600, 400, "基本窗口");
  SetTargetFPS (60);
  while (!WindowShouldClose ())
    {
      BeginDrawing ();

      ClearBackground (RAYWHITE);
      DrawText ("Hello, Raylib!", 100, 100, 20, RED);

      EndDrawing ();
    }
  CloseWindow ();
  return env->intern (env, "nil");
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

  DEFUN ("raylib-module-basic-window", Fbasic_window, 0, 0, NULL, NULL);
#undef DEFUN

  provide (env, "raylib-module");
  return 0;
}

/* Local Variables: */
/* compile-command: "cc -lraylib -Wall -shared -fpic raylib-module.c -o raylib-module.so" */
/* End: */
