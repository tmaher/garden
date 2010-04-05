/* $Id$ */
#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <arpa/inet.h>
#include <resolv.h>
#include <string.h>
#include <errno.h>
#include <sys/fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "yakov_b32.h"

int main(int argc, char **argv){
  char msg[32] = "Meep ar bfikl bradzo\n";

  char b32_data[128];
  char msg_after[128];
  int msg_after_len = 0;

  printf(msg);

  yakov_b32_encode(msg, strlen(msg), b32_data, sizeof(b32_data));

  printf("%s\n", b32_data);

  yakov_b32_decode(b32_data, strlen(b32_data), msg_after, sizeof(msg_after),
                   &msg_after_len);

  printf(msg_after);

  return 0;
}

