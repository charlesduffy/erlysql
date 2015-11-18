#ifndef __DBGLOG_
#define __DBGLOG_

#ifdef XDEBUG
#define debug(format, ...) fprintf (stderr, "[DEBUG] %s|%d: " format "\n",  __FILE__, __LINE__, ##__VA_ARGS__ )
#else
#define debug(format, ...)
#endif


/* terminate header guards */

#endif
