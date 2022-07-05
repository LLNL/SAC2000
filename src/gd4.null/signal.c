/*******************************************************************************
** PURPOSE:  Signal routine which is compatible with the SunView Notifier.
*            This routine replaces the system signal routine.
*
** INPUT ARGUMENTS:
*    See documentation for system signal routine.
*
** SUBROUTINES CALLED:
*    notify_set_signal_func
******************************************************************************/

#ifdef SUNOS4.0      /* Code for SunOS versions 4.0 and later. */

#include <sys/types.h>
#include <signal.h>
#include <sunwindow/notify.h>
#include <errno.h>

void (* signal (sig, func)) ()
  int sig, (*func) ();
{

  if ((sig < 1 || sig > NSIG) || (sig == SIGKILL || sig == SIGSTOP)) {
    errno = EINVAL;
    return;
  }

  if (sig == SIGCONT && func == SIG_IGN) {
    errno = EINVAL;
    return;
  }

  notify_set_signal_func(sig, func, sig, NOTIFY_ASYNC);
  return;

#else      /* Code for SunOS versions 3.5 and earlier. */

#include <signal.h>
#include <sunwindow/notify.h>
#include <errno.h>

int (* signal (sig, func)) ()
  int sig, (*func) ();
{

  if ((sig < 1 || sig > NSIG) || (sig == SIGKILL || sig == SIGSTOP)) {
    errno = EINVAL;
    return(BADSIG);
  }

  if (sig == SIGCONT && func == SIG_IGN) {
    errno = EINVAL;
    return(BADSIG);
  }

  return ((int(*) ()) notify_set_signal_func(sig, func, sig, NOTIFY_ASYNC));

#endif

}
/*******************************************************************************
** MODIFICATION HISTORY:
*    881026:  Added changes needed for SunOS versions 4.0 and later.
*    870707:  Original version.
*******************************************************************************/
