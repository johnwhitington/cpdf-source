/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <sys/types.h>
#include <sys/time.h>

CAMLprim value unix_gettimeofday(value unit)
{
  struct timeval tp;
  gettimeofday(&tp, NULL);
  return caml_copy_double((double) tp.tv_sec + (double) tp.tv_usec / 1e6);
}
