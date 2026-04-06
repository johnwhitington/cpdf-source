//Interface to Alan Murta's General Polygon Clipper Library
#include "gpc.h"
#include <stdio.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <assert.h>
#include <caml/signals.h>

static void freepolygon(value);

//Called by OCaml's garbage collector when a polygon needs freeing
void freepolygon(value poly) {
  gpc_free_polygon(*(gpc_polygon **) Data_custom_val(poly));
}

//The structure for the custom operations for our custom block which will hold a gpc polygon pointer
static struct custom_operations polygon_ops = {"Poly/v0.1",
                                        freepolygon,
                                        custom_compare_default,
                                        custom_hash_default,
                                        custom_serialize_default,
                                        custom_deserialize_default
                                       };

//Build a gpc polygon from an OCaml one 
CAMLprim value gpcml_buildpolygon (value mlpoly) {
  CAMLparam1 (mlpoly);
  CAMLlocal1 (cst);
  gpc_polygon *p = (gpc_polygon *) malloc(sizeof(gpc_polygon));
  cst = caml_alloc_custom(&polygon_ops, sizeof(gpc_polygon*), 0, 1);
  *((gpc_polygon **) Data_custom_val(cst)) = p;
  if (p) {
    int x, y;
    p->num_contours = Int_val(Field(mlpoly, 0));
    p->contour = (gpc_vertex_list *) malloc(sizeof(gpc_vertex_list) * p->num_contours);
    p->hole = (int *) malloc(sizeof(int) * p->num_contours);
    for(x = 0; x < p->num_contours; x++) {
      (p->hole)[x] = Int_val(Field(Field(mlpoly, 1), x));
      (p->contour)[x].num_vertices = Int_val(Field(Field(Field(mlpoly, 2), x), 0));
      (p->contour)[x].vertex = (gpc_vertex *) malloc(sizeof(gpc_vertex) * ((p->contour)[x].num_vertices));
      for(y = 0; y < (p->contour)[x].num_vertices; y++) {
        ((p->contour)[x].vertex)[y].x = Double_field(Field(Field(Field(Field(mlpoly, 2), x), 1), y), 0);
        ((p->contour)[x].vertex)[y].y = Double_field(Field(Field(Field(Field(mlpoly, 2), x), 1), y), 1);
      };
    };
  };
  CAMLreturn((value) cst);
}

//Build an OCaml polygon from a gpc one
CAMLprim value gpcml_getpolygon(value poly) {
  CAMLparam1(poly);
  CAMLlocal1(mlp);
  CAMLlocal5(holes, contours, contour, vertices, vertex);
  int x, y;
  gpc_polygon *p = *(gpc_polygon **) Data_custom_val(poly);
  mlp = caml_alloc_tuple(3);
  Store_field(mlp, 0, Val_int(p->num_contours));
  if (p->num_contours == 0)
  {
    holes = Atom(0);
    contours = Atom(0);
  }
  else
  {
    holes = caml_alloc(p->num_contours, 0);
    contours = caml_alloc(p->num_contours, 0);
    for(x = 0; x < p->num_contours; x++) {
      Store_field(holes, x, Val_int(p->hole[x]));
      contour = caml_alloc_tuple(2);
      Store_field(contour, 0, Val_int(p->contour[x].num_vertices));
      if (p->contour[x].num_vertices == 0) 
      {
        vertices = Atom(0);
        Store_field(contour, 1, vertices);
      }
      else
      {
        vertices = caml_alloc(p->contour[x].num_vertices, 0);
        Store_field(contour, 1, vertices);
        for(y = 0; y < p->contour[x].num_vertices; y++) {
          vertex = caml_alloc(4, Double_array_tag);
          Store_double_field(vertex, 0, (p->contour[x].vertex)[y].x);
          Store_double_field(vertex, 1, (p->contour[x].vertex)[y].y);
          Store_field(vertices, y, vertex);
        }
      }
      Store_field(contours, x, contour);
    }
  }
  Store_field(mlp, 1, holes);
  Store_field(mlp, 2, contours);
  CAMLreturn(mlp);
}

//Print a polygon to standard output
CAMLprim value gpcml_printpolygon (value valpolygon) {
  CAMLparam1 (valpolygon);
  gpc_polygon* p = *(gpc_polygon**) Data_custom_val(valpolygon);
  int x, y;
  printf("\nContours: %i\n", p->num_contours);
  for(x = 0; x < p->num_contours; x++) {
    printf("  Contour %i has %i vertices and hole %i:\n", x, p->contour[x].num_vertices, p->hole[x]);
    for(y = 0; y < p->contour[x].num_vertices; y++) {
      printf("    Vertex %i: (x = %f, y = %f)\n", y, (p->contour[x].vertex)[y].x, (p->contour[x].vertex)[y].y);
    }
  }
  fflush(stdout);
  CAMLreturn(Val_unit);
}

//Clip one polygon against another, returning the result
CAMLprim value gpcml_clip(value p, value q, value op) {
 CAMLparam3(p, q, op);
 CAMLlocal1(r);
 gpc_polygon *rp = (gpc_polygon *) malloc(sizeof(gpc_polygon));
 gpc_op gpc_clipop;
 int clip_op = Int_val(op);
 gpc_polygon *real_p;
 gpc_polygon *real_q;
 assert(Is_block(p) && Tag_val(p) == Custom_tag);
 assert(Is_block(q) && Tag_val(q) == Custom_tag);
 real_p = *(gpc_polygon**) Data_custom_val(p);
 real_q = *(gpc_polygon**) Data_custom_val(q);
 if (clip_op == 0) gpc_clipop = GPC_DIFF;
 if (clip_op == 1) gpc_clipop = GPC_INT;
 if (clip_op == 2) gpc_clipop = GPC_XOR;
 if (clip_op == 3) gpc_clipop = GPC_UNION;

 caml_enter_blocking_section();
 gpc_polygon_clip(gpc_clipop, real_p, real_q, rp);
 caml_leave_blocking_section();

 r = caml_alloc_custom(&polygon_ops, sizeof(gpc_polygon*), 0, 1);
 *((gpc_polygon **) Data_custom_val(r)) = rp;
 CAMLreturn(r);
}
