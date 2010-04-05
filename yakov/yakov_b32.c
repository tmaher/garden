/* $Id$ */

#include "yakov_b32.h"

void yakov_b32_encode_block(char *src, char *dst){
  int i=0, j=0;
  char * alphabet = "0123456789abcdefghjkmnpqrstvwxyz";

  // 12345
  // abcdefgh

  // top 5 bits of i
  dst[j] = alphabet[(u_int8_t) (src[i] >> 3)];

  // bottom 3 of i, top 2 of i+1
  // dst[j + 1] = alphabet[(u_int8_t) (((src[i] << 5) >> 3) | (src[i + 1] >> 6))];
  dst[j + 1] = alphabet[ ((u_int8_t) (((u_int8_t) (src[i] << 5)) >> 3)) |
                         ((u_int8_t) (src[i+1] >> 6))];

  // bits 3-7 of i+1
  // dst[j + 2] = alphabet[(u_int8_t) ((src[i + 1] << 2) >> 3)];
  dst[j + 2] = alphabet[ (u_int8_t) (((u_int8_t) (src[i+1] << 2)) >> 3)];

  // bottom 1 bit of i+1, top 4 bits of i+2
  //dst[j + 3] = alphabet[(u_int8_t) (((src[i + 1] << 7) >> 3) | (src[i+2] >> 4))];
  dst[j + 3] = alphabet[ ((u_int8_t) (((u_int8_t) (src[i+1] << 7)) >> 3)) |
                         ((u_int8_t) (src[i+2] >> 4)) ];

  // bottom 4 bits of i+2, top bit of i+3
  // dst[j + 4] = alphabet[(u_int8_t) (((src[i+2] << 4) >> 3) | (src[i+3] >> 7))];
  dst[j + 4] = alphabet[ ((u_int8_t) (((u_int8_t) (src[i+2] << 4)) >> 3)) |
                         ((u_int8_t) (src[i+3] >> 7)) ];

  // bits 2-6 of i+3
  // dst[j + 5] = alphabet[(u_int8_t) ((src[i+3] << 1) >> 3)];
  dst[j + 5] = alphabet[ (u_int8_t) (((u_int8_t) (src[i+3] << 1)) >> 3) ];

  // bottom 2 bits of i+3, top 3 bits of i+4
  // dst[j + 6] = alphabet[(u_int8_t) (((src[i+3] << 6) >> 3) | (src[i+4] >> 5))];
  dst[j + 6] = alphabet[ ((u_int8_t) (((u_int8_t) (src[i+3] << 6)) >> 3)) |
                         ((u_int8_t) (src[i+4] >> 5)) ];

  // bottom 5 bits of i+4
  // dst[j + 7] = alphabet[(u_int8_t) ((src[i+4] << 3) >> 3)];
  dst[j + 7] = alphabet[ (u_int8_t) (((u_int8_t) (src[i+4] << 3)) >> 3) ];

  return;
}

char *yakov_b32_encode(char *src, int src_len, char *dst, int dst_len){

  char remainder[5] = {0, 0, 0, 0, 0};

  if(dst_len < ((src_len * 8 / 5) + 2)) return NULL;

  /* Base32 is a 5-bit encoding scheme.  A "block" in this context is 5
     bytes of input.  That's 40 bits, which is exactly 8 bytes of output,
     which means we don't need to pad the input with any zeroes. */

  int i=0, j=0;
  for(i = 0; i <= (src_len - 5); i += 5){
    yakov_b32_encode_block(&(src[i]), &(dst[j]));
    j += 8;
  }

  if(i == src_len){
    /* Yay, the input fit in an integer number of blocks */
    dst[j] = '0';
    return dst;
  }

  /* Boo, the input ended in a partial block.  Pad it with zeroes to
     finish out the block and encode anyway */
  memcpy(remainder, &(src[i]), (src_len - i));
  yakov_b32_encode_block(remainder, &(dst[j]));

  /* Now we need to indicate how much padding we used.  To do that,
     we append one of the characters we omitted from the alphabet. */
  switch(src_len - i){
  case 1:
    dst[j+2] = 'l'; /* 2 bits */
    dst[j+3] = 0;
    dst[j+2] = 0;
    break;
  case 2:
    dst[j+4] = 'u'; /* 4 bits */
    dst[j+5] = 0;
    dst[j+4] = 0;
    break;
  case 3:
    dst[j+5] = 'i'; /* 1 bit */
    dst[j+6] = 0;
    dst[j+5] = 0;
    break;
  case 4:
    dst[j+7] = 'o'; /* 3 bits */
    dst[j+8] = 0;
    dst[j+7] = 0;
    break;
  }
  
  
  return dst;
}

void yakov_b32_decode_block(u_int8_t *src, u_int8_t *dst){
  int i=0, j=0;

  // all 5 bits of i, top 3 bits of i+1
  dst[j] = ((u_int8_t) (src[i] << 3)) | ((u_int8_t) (src[i+1] >> 2));

  // bottom 2 bits of i+1, all bits of i+2, top bit of i+3
  dst[j+1] = ((u_int8_t) (src[i+1] << 6)) |
    ((u_int8_t) (src[i+2] << 1)) |
    ((u_int8_t) (src[i+3] >> 4));

  // bottom 4 bits of i+3, top 4 bits of i+4
  dst[j+2] = ((u_int8_t) (src[i+3] << 4)) |
    ((u_int8_t) (src[i+4] >> 1));

  // bottom bit of i+4, all 5 bits of i+5, top 2 bits of i+6
  dst[j+3] = ((u_int8_t) (src[i+4] << 7)) |
    ((u_int8_t) (src[i+5] << 2)) |
    ((u_int8_t) (src[i+6] >> 3));

  // bottom 3 bits of i+6, all 5 bits of i+7
  dst[j+4] = ((u_int8_t) (src[i+6] << 5)) |
    ((u_int8_t) (src[i+7]));

  return;
}

char *yakov_b32_decode(char *encap, int encap_len,
                       char *payload, int payload_buf_len,
                       int *payload_return_len){
  int i=0, j=0, n=0;
  u_int8_t normal_buf[8];

  /* Lookup table to convert from Crockford 32 alphabet to the original
     (non-byte-aligned unaligned) data.  yakov_b32_decode_block()
     removes the padding bits and repacks the data bits into the
     original encapsulated data. */
  u_int8_t a2u[] = {
    0 ,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 00-0f
    0 ,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 10-0f
    0 ,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, // 20-0f
    0 ,  1,  2,  3,  4,  5,  6,  7,  8,  9,  0,  0,  0,  0,  0,  0, // 30-3f
    0 , 10, 11, 12, 13, 14, 15, 16, 17,  1, 18, 19,  1, 20, 21,  0, // 40-4f
    22, 23, 24, 25, 26,  0, 27, 28, 29, 30, 31,  0,  0,  0,  0,  0, // 50-5f
    0 , 10, 11, 12, 13, 14, 15, 16, 17,  1, 18, 19,  1, 20, 21,  0, // 60-7f
    22, 23, 24, 25, 26,  0, 27, 28, 29, 30, 31,  0,  0,  0,  0,  0  // 70-7f
  };

  for(j=0; j <= encap_len; j += 1){
    if(encap[j] >= 0x7F) return NULL;  // invalid characters!
    if(encap[j] == 0x2D) continue;     // 0x2D is "-", skip it
    normal_buf[n] = a2u[(u_int8_t) encap[j]];
    n += 1;
    if(n == 8){
      yakov_b32_decode_block(normal_buf, (u_int8_t *) &(payload[i]));
      i += 5;
      n = 0;
      memset(normal_buf, 0, 8);
    }
  }

  /* yay, we wended on a block boundary. Just return */
  if(n == 0){
    *payload_return_len = i;
    return payload;
  }

  /* Check how much of a remainder there is, synthesize a full block,
     decode it, and trunacte the return data. */
  memset(&(normal_buf[n]), 0, 8 - n);
  yakov_b32_decode_block(normal_buf, (u_int8_t *) &(payload[i]));

  switch(n) {
  case 2:  // one byte
    *payload_return_len = i + 1;
    break;
  case 4: // two bytes
    *payload_return_len = i + 2;
    break;
  case 5: // three bytes
    *payload_return_len = i + 3;
    break;
  case 7: // four bytes
    *payload_return_len = i + 4;
    break;
  default:  /* ERROR!  ERROR!  STERILIZE!  STER-I-LIZE! */
    *payload_return_len = 0;
    return NULL;
  }

  return payload;
}

