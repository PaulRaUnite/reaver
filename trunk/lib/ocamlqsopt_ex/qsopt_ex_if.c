#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <gmp.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <QSopt_ex.h>

struct lp {
  char *prob_name;
  int number_of_variables, number_of_constraints, number_of_values;
  int *cmatcnt, *cmatbeg, *cmatind;
  char *sense;
  mpq_t *obj, *rhs, *lower, *upper, *cmatval;
  char **colnames;
  char **rownames;
  QSbasis* basis;
};

#define NEGINFTY -1
#define FINITE 0
#define INFTY 1

struct sol {
  int kind;
  mpq_t value;
  mpq_t* x;
  int x_size;
};

value camlidl_qsopt_ex_get_value_from_sol(value v_sol) {
  CAMLparam1(v_sol);
      
  struct sol * sol = (struct sol *) v_sol;
  char * _str_value; 
  value str_value;
  switch (sol->kind) {
    case NEGINFTY: 
      str_value = caml_copy_string("--");
      break;
    case INFTY:
      str_value = caml_copy_string("++");
      break;
    case FINITE: 
      _str_value = mpq_get_str(NULL, 10, sol->value);
      str_value = caml_copy_string(_str_value);
      free(_str_value);
      break;
    default:
      caml_failwith("");
      break;
  }

  CAMLreturn(str_value);
}

value camlidl_qsopt_ex_get_x_from_sol(value v_sol, value v_i) {
  CAMLparam2(v_sol, v_i);
      
  struct sol * sol = (struct sol *) v_sol;
  int i = Int_val(v_i);

  char * _str_value = mpq_get_str(NULL, 10, sol->x[i]);
  value str_value = caml_copy_string(_str_value);
  free(_str_value);
  CAMLreturn(str_value);
}

value camlidl_qsopt_ex_free_sol(value v_sol) {
  CAMLparam1(v_sol);
  struct sol * sol = (struct sol *) v_sol;
  mpq_clear(sol->value);
  if (sol->kind == FINITE) {
    int i;
    for (i = 0; i < sol->x_size; i++) {
      mpq_clear(sol->x[i]);
    }
  }
  free(sol->x);
  free(sol);
  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_hello(value u) {
	CAMLparam1(u);
	printf("Hallo\n");
	CAMLreturn(Val_unit);
}

#define ARRAY_INIT(a,n,t) a = (t *) malloc(n * sizeof(t)); if(a == NULL) caml_failwith("caml_csdp: out of memory!")
#define PTR_INIT(p,t) p = (t *) malloc(sizeof(t)); if(p == NULL) caml_failwith("caml_csdp: out of memory!")
#define CLEANUP caml_failwith("CLEANUP")
#define COMP_LE 0
#define COMP_EQ 1

value camlidl_qsopt_ex_qsopt_ex_init (value v_presicion) {
  CAMLparam1(v_presicion);
        
  int precision;
  precision = Int_val(v_presicion);
  QSexactStart();        
  QSexact_set_precision (precision);

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_create_lp(
    value v_prob_name,
    value v_number_of_variables, 
    value v_number_of_constraints,
    value v_number_of_values) 
{
	CAMLparam4(
            v_prob_name,
            v_number_of_variables, 
            v_number_of_constraints,
            v_number_of_values);

        int i;

        char *prob_name;
        prob_name = strdup(String_val(v_prob_name));
	int number_of_variables, number_of_constraints, number_of_values;
	number_of_variables = Int_val(v_number_of_variables);
	number_of_constraints = Int_val(v_number_of_constraints);
        number_of_values = Int_val(v_number_of_values);

	struct lp * lp;
        PTR_INIT(lp,struct lp);

        lp->prob_name = prob_name;
        lp->number_of_variables = number_of_variables;
        lp->number_of_constraints = number_of_constraints;
        lp->number_of_values = number_of_values;

        ARRAY_INIT(lp->cmatcnt, number_of_variables, int);
        ARRAY_INIT(lp->cmatbeg, number_of_variables, int);
        ARRAY_INIT(lp->cmatind, number_of_values, int); 
        ARRAY_INIT(lp->sense, number_of_constraints, char);
        ARRAY_INIT(lp->obj, number_of_variables, mpq_t);
        for (i = 0; i < number_of_variables; i++) 
          mpq_init(lp->obj[i]);
        ARRAY_INIT(lp->rhs, number_of_constraints, mpq_t);
        for (i = 0; i < number_of_constraints; i++) 
          mpq_init(lp->rhs[i]);
        ARRAY_INIT(lp->lower, number_of_variables, mpq_t);
        for (i = 0; i < number_of_variables; i++) {
          mpq_init(lp->lower[i]);
          mpq_set(lp->lower[i], mpq_ILL_MINDOUBLE);
        }
        ARRAY_INIT(lp->upper, number_of_variables, mpq_t);
        for (i = 0; i < number_of_variables; i++) {
          mpq_init(lp->upper[i]);
          mpq_set(lp->upper[i], mpq_ILL_MAXDOUBLE);
        }
        ARRAY_INIT(lp->cmatval, number_of_values, mpq_t);
        for (i = 0; i < number_of_values; i++)
          mpq_init(lp->cmatval[i]);
        ARRAY_INIT(lp->colnames, number_of_variables, char *);
        for (i = 0; i < number_of_variables; i++) {
          lp->colnames[i] = (char*) malloc(10 * sizeof(char));
          sprintf(lp->colnames[i], "x_%d", i);
        }
        ARRAY_INIT(lp->rownames, number_of_constraints, char *);
        for (i = 0; i < number_of_constraints; i++) {
          lp->rownames[i] = (char*) malloc(10 * sizeof(char));
          sprintf(lp->rownames[i], "c_%d", i);
        }

        lp->basis = EGsMalloc(QSbasis, 1);
        memset(lp->basis, 0, sizeof(QSbasis));
	
        CAMLreturn((value) lp);	
}

value camlidl_qsopt_ex_set_cmatcnt(value v_lp, value v_i, value v_cnt) {
  CAMLparam3(v_lp, v_i, v_cnt);

  int i, cnt;
  i = Int_val(v_i);
  cnt = Int_val(v_cnt);
  struct lp * lp;
  lp = (struct lp *) v_lp;
  
  lp->cmatcnt[i] = cnt;

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_set_cmatbeg(value v_lp, value v_i, value v_beg) {
  CAMLparam3(v_lp, v_i, v_beg);

  int i, beg;
  i = Int_val(v_i);
  beg = Int_val(v_beg);
  struct lp * lp;
  lp = (struct lp *) v_lp;
  
  lp->cmatbeg[i] = beg;

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_set_cmatind(value v_lp, value v_i, value v_ind) {
  CAMLparam3(v_lp, v_i, v_ind);

  int i, ind;
  i = Int_val(v_i);
  ind = Int_val(v_ind);
  struct lp * lp;
  lp = (struct lp *) v_lp;
  
  lp->cmatind[i] = ind;

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_set_sense(value v_lp, value v_i, value v_sense) {
  CAMLparam3(v_lp, v_i, v_sense);

  int i;
  int sense;
  i = Int_val(v_i);
  sense = Int_val(v_sense);
  struct lp * lp;
  lp = (struct lp *) v_lp;
 
  if (sense == COMP_LE) 
    lp->sense[i] = 'L';
  else if (sense == COMP_EQ)
    lp->sense[i] = 'E';

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_set_colname(value v_lp, value v_i, value v_n) {
  CAMLparam3(v_lp, v_i, v_n);
  
  char *n = String_val(v_n);
  struct lp * lp;
  lp = (struct lp *) v_lp;
  int i;
  i = Int_val(v_i);
  free(lp->colnames[i]);
  lp->colnames[i] = strdup(n);

  CAMLreturn(Val_unit);
}

void set_mpq_t_value(mpq_t *q, value v_n) {
  char *str = String_val(v_n);
  int ret_val = mpq_set_str(*q, str, 10);
  mpq_canonicalize(*q);
  if (ret_val) printf("######################## Problem");
  return; 
}

value camlidl_qsopt_ex_set_obj(value v_lp, value v_i, value v_n) {
  CAMLparam3(v_lp, v_i, v_n);

  struct lp * lp;
  lp = (struct lp *) v_lp;
  int i;
  i = Int_val(v_i);
 
  set_mpq_t_value(&lp->obj[i], v_n);

  /*  char * test = mpq_get_str(NULL,10,lp->obj[i]);
      printf("Test : %s", test); */

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_set_rhs(value v_lp, value v_i, value v_n) {
  CAMLparam3(v_lp, v_i, v_n);

  struct lp * lp;
  lp = (struct lp *) v_lp;
  int i;
  i = Int_val(v_i);
 
  set_mpq_t_value(&lp->rhs[i], v_n);

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_set_lower(value v_lp, value v_i, value v_n) {
  CAMLparam3(v_lp, v_i, v_n);

  struct lp * lp;
  lp = (struct lp *) v_lp;
  int i;
  i = Int_val(v_i);
 
  set_mpq_t_value(&lp->lower[i], v_n);

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_set_upper(value v_lp, value v_i, value v_n) {
  CAMLparam3(v_lp, v_i, v_n);

  struct lp * lp;
  lp = (struct lp *) v_lp;
  int i;
  i = Int_val(v_i);
 
  set_mpq_t_value(&lp->upper[i], v_n);

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_set_cmatval(value v_lp, value v_i, value v_n) {
  CAMLparam3(v_lp, v_i, v_n);

  struct lp * lp;
  lp = (struct lp *) v_lp;
  int i;
  i = Int_val(v_i);
 
  set_mpq_t_value(&lp->cmatval[i], v_n);

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_free_lp(value v_lp) {
  CAMLparam1(v_lp);

  int i;

  struct lp *lp;
  lp = (struct lp *) v_lp;

  free(lp->prob_name);
  free(lp->cmatcnt);
  free(lp->cmatbeg);
  free(lp->cmatind);
  free(lp->sense);
  for (i = 0; i < lp->number_of_variables; i++) 
    mpq_clear(lp->obj[i]);
  free(lp->obj);
  for (i = 0; i < lp->number_of_constraints; i++) 
    mpq_clear(lp->rhs[i]);
  free(lp->rhs);
  for (i = 0; i < lp->number_of_variables; i++) 
    mpq_clear(lp->lower[i]);
  free(lp->lower);
  for (i = 0; i < lp->number_of_variables; i++) 
    mpq_clear(lp->upper[i]);
  free(lp->upper);
  for (i = 0; i < lp->number_of_values; i++)
    mpq_clear(lp->cmatval[i]);
  free(lp->cmatval);
  for (i = 0; i < lp->number_of_variables; i++) 
    free(lp->colnames[i]);
  free(lp->colnames);
  for (i = 0; i < lp->number_of_constraints; i++) {
    free(lp->rownames[i]);
  }
  free(lp);

  CAMLreturn(Val_unit);
}

value camlidl_qsopt_ex_solve(value v_lp) {
  CAMLparam1(v_lp);

  int rval = 0, status = 0;
  
  struct sol * sol;
  sol = (struct sol *) malloc(sizeof(struct sol));
  mpq_init(sol->value);

  /* mpq_t obj_value;
     mpq_init(obj_value); */

  struct lp *lp;
  lp = (struct lp *) v_lp;
 
  mpq_QSprob p;
  p = mpq_QSload_prob 
    (lp->prob_name, 
     lp->number_of_variables, 
     lp->number_of_constraints, 
     lp->cmatcnt, 
     lp->cmatbeg, 
     lp->cmatind, 
     lp->cmatval,
     QS_MAX, 
     lp->obj, 
     lp->rhs, 
     lp->sense, 
     lp->lower, 
     lp->upper, 
     lp->colnames,
     lp->rownames);

  /* mpq_QSwrite_prob_file(p, stdout, "LP");
   rval = QSexact_solver (p, NULL, NULL, NULL, DUAL_SIMPLEX, &status);
   rval = QSexact_solver (p, NULL, NULL, NULL, PRIMAL_SIMPLEX, &status); */
  rval = QSexact_solver (p, NULL, NULL, lp->basis, PRIMAL_SIMPLEX, &status);
  /* rval = QSexact_solver (p, NULL, NULL, NULL, PRIMAL_OR_DUAL, &status); */

  if (rval) {
    fprintf (stderr, "QSexact_solver failed\n");
    CLEANUP;
  }

  /*  CPXsolninfo  */
  /* rval = mpq_QSget_status (p, &status); */
  switch (status) {
    case QS_LP_OPTIMAL:
      sol->kind = FINITE;
      
      rval = mpq_QSget_objval (p, sol->value);
      if (rval) {
        fprintf (stderr, "Could not get obj value, error code %d\n", rval);
      } 

      /* Auslesen der optimalen Loesung */
      int i; 

      sol->x = (mpq_t *) malloc (lp->number_of_variables * sizeof (mpq_t));
      sol->x_size = lp->number_of_variables;
      for (i = 0; i < lp->number_of_variables; i++) 
        mpq_init (sol->x[i]);

      rval = mpq_QSget_x_array (p, sol->x);
      if (rval) {
          fprintf (stderr, "Could not get x-vector, error code %d\n", rval);
      } 

      break;
    case QS_LP_INFEASIBLE:
      sol->kind = NEGINFTY;
      break;
    case QS_LP_UNBOUNDED:
      sol->kind = INFTY;
      break;
    case QS_LP_UNSOLVED:
      printf ("The optimizer could not solve the LP\n");
      break;
    case QS_LP_MODIFIED:
      printf ("The LP was modified since last optimization call\n");
      break;
    default:
      printf ("Unknown solution status: %d\n", status);
      break;
  }

  mpq_QSfree_prob(p);
  CAMLreturn((value) sol);	
}

