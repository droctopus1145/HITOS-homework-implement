/*
 *  linux/lib/iam.c
 *
 *  (C) 2025  dr0ctopus1145
 */

#define __LIBRARY__
#include <unistd.h>

_syscall1(int,iam,const char *,name)