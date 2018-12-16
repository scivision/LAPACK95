#include <sys/time.h>
#include <unistd.h>

double rtc_() {
   struct timeval t;
   
   gettimeofday(&t, NULL);
   
   return (t.tv_sec + (double) t.tv_usec /1000000.0);
}
