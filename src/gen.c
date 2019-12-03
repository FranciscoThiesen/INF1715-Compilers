#include "ast.h"
#include "symbols.h"
#include "typing.h"
#include "aux.h"
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

static void gen_temporary_code(int expnum) {
    printf("t%d", expnum);
}

static void gen_local_temporary_code(int expnum) {
    printf("%%");
    gen_temporary_code(expnum);
}

static void gen_global_temporary_code(int expnum) {
    printf("@");
    gen_temporary_code(expnum);
}

static int get_new_label(State *global_state) {
    return ++(global_state->label_count);
}

static void copylabel(char *buffer, int labelnum) {
    sprintf(buffer, "%%l%d", labelnum);
}

static void gen_local_vars(Def *def, State *global_state);

static int get_type_size(Type *t) {
    if (t->tag == SEQ)
        return 8;

    switch (t->single.type) {
        case CHAR:
        case INT:
            return 4;
        case FLOAT:
            return 8;
        case BOOL:
            return 1;
        default:
            return 0;
    }
}
static void gen_type(Type *type);

static void gen_printf_call(Type *t, char *tfmt, int sz, int expnum) {
    printf("call i32 (i8*, ...) @printf( i8* getelementptr ([%d x i8],"
            "[%d x i8]*  @fmt%s, i32 0, i32 0), ", sz, sz, tfmt);
    gen_type(t);
    printf(" ");
    gen_local_temporary_code(expnum);
    printf(")\n");
}

static Type *get_exp_type_internal(Exp *exp);

static Type *get_ref_type(RefVar *r) {
    switch (r->tag) {
        case REF_VAR:
            return r->refv.v->type;
        case REF_ARRAY:
            return get_exp_type_internal(r->refa.v)->seq.next;
        default:
            fprintf(stderr, "not implemented\n");
            exit(-1);
    }
}

static Type *get_exp_type_internal(Exp *exp) {
    switch(exp->tag) {
        case VAR:
            return get_ref_type(exp->var.def);
        case EXPCHAR:
            return exp->expchar.type;
        case EXPINT:
            return exp->expint.type;
        case EXPFLOAT:
            return exp->expfloat.type;
        case EXPBOOL:
            return exp->expbool.type;
        case SUM:
        case SUB:
        case MUL:
        case DIV:
        case OR:
        case NEQ:
        case EQ:
        case GEQ:
        case LEQ:
        case L:
        case G:
        case AND:
            return exp->binary.exptype;
        case NOT:
        case MINUS:
            return exp->unary.exptype;
        case NEW:
            return exp->new.type;
        case CALLEXP:
            return exp->call.funcdef->type;
        default:
            fprintf(stderr, "not implemented");
            exit(-1);
    }
}

static void gen_native_type(Native_types type) {
    switch( type ) {
        case CHAR:
        case INT:
            printf("i32");
            break;
        case FLOAT:
            printf("double");
            break;
        case BOOL:
            printf("i8");
            break;
        default:
            break;
    }
}

static void gen_type(Type *type) {
    if (type == NULL) {
        printf("void");
        return;
    }

    if (type->tag == SINGLE) {
        gen_native_type(type->single.type);
    } else {
        gen_type(type->seq.next);
        printf("*");
    }

}

static void gen_pointer_reference(Type *t, int temp) {
    gen_type(t);
    printf("* ");
    gen_local_temporary_code(temp);
    printf("\n");
}

static int get_new_temporary(State *global_state) {
    return ++(global_state->temp_count);
}

static void gen_load(Type *t, int address_temporary, State *global_state) {
    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = load ");
    gen_type(t);
    printf(", ");
    gen_pointer_reference(t, address_temporary);
}

static void gen_store(Type* t1, Type *t2, int address_temporary, int tempnum,
        bool is_exp) {
    printf("store ");
    gen_type(t1);
    printf(" ");
    if (is_exp)
        gen_local_temporary_code(tempnum);
    else
        printf("%%%d", tempnum);

    printf(", ");
    gen_type(t2);
    printf("* ");
    gen_local_temporary_code(address_temporary);
    printf("\n");
}


static int gen_exp(Exp *exp, State *global_state);

static void gen_label_ref(int labelnum) {
    printf("%%l%d", labelnum);
}

static void gen_label(int labelnum) {
    printf("l%d:\n", labelnum);
}


static void gen_conditional_jump(int lt, int lf, State *global_state) {
    char ltstr[10], lfstr[10];

    printf("br i1 ");
    gen_local_temporary_code(global_state->temp_count);
    copylabel(ltstr, lt);
    copylabel(lfstr, lf);
    printf(", label %s, label %s\n", ltstr, lfstr);
}

static void gen_ext_i1_to_i8(State *global_state) {
    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = ");
    printf("zext i1 ");
    gen_local_temporary_code(global_state->temp_count - 1);
    printf(" to i8\n");
}

static void gen_cond(Exp *exp, int lt, int lf, State *global_state) {
    int e1num;
    int l1;

    switch (exp->tag) {
        case AND:
            l1 = get_new_label(global_state);
            gen_cond(exp->binary.e1, l1, lf, global_state);
            gen_label(l1);
            gen_cond(exp->binary.e2, lt, lf, global_state);
            break;
        case OR:
            l1 = get_new_label(global_state);
            gen_cond(exp->binary.e1, lt, l1, global_state);
            gen_label(l1);
            gen_cond(exp->binary.e2, lt, lf, global_state);
            break;
        case NOT:
            gen_cond(exp->unary.exp, lf, lt, global_state);
            break;
        case SUM:
        case SUB:
        case MUL:
        case DIV:
            e1num = gen_exp(exp, global_state);
            Type *te1 = get_exp_type_internal(exp);

            gen_local_temporary_code(get_new_temporary(global_state));
            printf(" = ");

            if (te1->single.type == FLOAT)
                printf("fcmp oeq ");
            else
                printf("icmp eq ");

            gen_local_temporary_code(e1num);
            printf(", 0\n");
            break;
        default:
            e1num = gen_exp(exp, global_state);
            int truncated_id = get_new_temporary(global_state);
            gen_local_temporary_code(truncated_id);

            printf(" = trunc i8 ");
            gen_local_temporary_code(e1num);
            printf(" to i1\n");

            gen_conditional_jump(lt, lf, global_state);
            break;
    }

}

static int gen_int(int eint, State *global_state) {
    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = add i32 0, %d\n", eint);
    return global_state->temp_count;
}

static int gen_float(float efloat, State *global_state) {
    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = fadd double %e, %e\n", 0.0, efloat);
    return global_state->temp_count;
}

static int gen_bool(bool ebool, State *global_state) {
    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = add i8  0, %d\n", ebool);
    return global_state->temp_count;
}

static void gen_zero(Type *type) {
    if (type->tag == SEQ) {
        printf("null");
        return;
    }

    switch(type->single.type) {
        case CHAR:
        case BOOL:
        case INT:
            printf("0");
            break;
        case FLOAT:
            printf("%e", 0.0);
            break;
        default:
            fprintf(stderr, "not implemented\n");
            exit(-1);
    }
}

static int copy_var_address(Type *type, int tempnum, bool is_global,
        State *global_state) {
    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = getelementptr ");
    gen_type(type);
    printf(", ");
    gen_type(type);
    printf("* ");
    if(is_global)
        gen_global_temporary_code(tempnum);
    else
        gen_local_temporary_code(tempnum);
    printf(", i32 0\n");

    return global_state->temp_count;
}

static void gen_unconditional_jump(int ls) {
    char labelstr[10];

    copylabel(labelstr, ls);
    printf("br label %s\n", labelstr);
}

static int gen_var(RefVar *r, State *global_state) {
    Var *v = r->refv.v;

    return copy_var_address(v->type, v->tempnum, v->is_global, global_state);
}

static int gen_array(RefVar *r, State *global_state) {
    int expvarnum = gen_exp(r->refa.v, global_state);
    int expindexnum = gen_exp(r->refa.idx, global_state);
    Type *indexedtype = get_ref_type(r);

    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = getelementptr ");
    gen_type(indexedtype);
    printf(", ");
    gen_type(indexedtype);
    printf("* ");
    printf(" ");
    gen_local_temporary_code(expvarnum);
    printf(", i32 ");
    gen_local_temporary_code(expindexnum);
    printf("\n");

    return global_state->temp_count;
}

static int gen_ref(RefVar *r, State *global_state) {
    switch (r->tag) {
        case REF_VAR:
            return gen_var(r, global_state);
        case REF_ARRAY:
            return gen_array(r, global_state);
        default:
            fprintf(stderr, "not implemented\n");
            exit(-1);
    }
}

static int gen_exp_att(RefVar *r, Exp *e, State *global_state) {
    int refnum = gen_ref(r, global_state);
    int expnum = gen_exp(e, global_state);

    Type *reftype = get_ref_type(r);
    Type *exptype = get_exp_type_internal(e);

    gen_store(reftype, exptype, refnum, expnum, true);

    return global_state->temp_count;
}

static int gen_exp_var(Exp *exp, State *global_state) {
    int refnum = gen_ref(exp->var.def, global_state);
    Type *reftype = get_ref_type(exp->var.def);

    gen_load(reftype, refnum, global_state);

    return global_state->temp_count;
}

static int gen_exp_new(Exp *exp, Type *type, State *global_state) {
    int expnum = gen_exp(exp, global_state);
    int tsize = get_type_size(type->seq.next);

    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = sext i32 ");
    gen_local_temporary_code(expnum);
    printf(" to i64\n");
    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = mul i64 ");
    gen_local_temporary_code(global_state->temp_count - 1);
    printf(", %d\n", tsize);
    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = call i8* @malloc(i64 ");
    gen_local_temporary_code(global_state->temp_count - 1);
    printf(")\n");
    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = bitcast i8 * ");
    gen_local_temporary_code(global_state->temp_count - 1);
    printf(" to ");
    gen_type(type);
    printf("\n");

    return global_state->temp_count;
}

static void gen_ret() {
    printf("ret void\n");
}

static void gen_retexp(Exp *exp, State *global_state) {
    int numexp = gen_exp(exp, global_state);

    printf("ret ");
    gen_type(global_state->cur_func_type);
    printf(" ");
    gen_local_temporary_code(numexp);
    printf("\n");

}

static void gen_stat(Stat *stat, State *global_state);

static void gen_while(Cmd *cmd, State *global_state) {
    int l_cond = get_new_label(global_state);
    int l_body = get_new_label(global_state);
    int l_end = get_new_label(global_state);

    // pula para o inicio do while
    gen_unconditional_jump(l_cond);

    // checa condicao
    gen_label(l_cond);
    gen_cond(cmd->cmd_while.exp, l_body, l_end, global_state);

    // corpo do while
    gen_label(l_body);
    gen_stat(cmd->cmd_while.stat, global_state);
    gen_unconditional_jump(l_cond);

    // final do while
    gen_label(l_end);

}

static void gen_if(Cmd *cmd, State *global_state) {
    int lt = get_new_label(global_state);
    int lf = get_new_label(global_state);

    gen_cond(cmd->cmd_if.exp, lt, lf, global_state);
    gen_label(lt);
    gen_stat(cmd->cmd_if.stat, global_state);
    gen_unconditional_jump(lf);
    printf("\n");

    gen_label(lf);

}

static void gen_ifelse(Cmd *cmd, State *global_state) {
    int lt  = get_new_label(global_state);
    int le  = get_new_label(global_state);
    int lo  = get_new_label(global_state);

    gen_cond(cmd->cmd_if.exp, lt, le, global_state);
    gen_label(lt);
    gen_stat(cmd->cmd_ifelse.stat, global_state);
    gen_unconditional_jump(lo);
    printf("\n");

    gen_label(le);
    gen_stat(cmd->cmd_ifelse.stat2, global_state);
    gen_unconditional_jump(lo);
    printf("\n");

    gen_label(lo);

}
static int gen_binary_exp(Exp *e1, Exp *e2, Exp_type etype,
        State *global_state) {
    Type *te1 = get_exp_type_internal(e1);
    int e1num = gen_exp(e1, global_state);
    int e2num = gen_exp(e2, global_state);

    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = ");

    switch(etype) {
        case SUM:
            if (te1->single.type == FLOAT)
                printf("fadd ");
            else
                printf("add nsw ");
            break;
        case SUB:
            if (te1->single.type == FLOAT)
                printf("fsub ");
            else
                printf("sub nsw ");

            break;
        case MUL:
            if (te1->single.type == FLOAT)
                printf("fmul ");
            else
                printf("mul nsw ");

            break;
        case DIV:
            if (te1->single.type == FLOAT)
                printf("fdiv ");
            else
                printf("sdiv ");
            break;
        case NEQ:
            if (te1->single.type == FLOAT)
                printf("fcmp one ");
            else
                printf("icmp ne ");
            break;
        case EQ:
            if (te1->single.type == FLOAT)
                printf("fcmp oeq ");
            else
                printf("icmp eq ");
            break;
        case G:
            if (te1->single.type == FLOAT)
                printf("fcmp ogt ");
            else
                printf("icmp sgt ");
            break;
        case L:
            if (te1->single.type == FLOAT)
                printf("fcmp olt ");
            else
                printf("icmp slt ");
            break;
        case GEQ:
            if (te1->single.type == FLOAT)
                printf("fcmp oge ");
            else
                printf("icmp sge ");
            break;
        case LEQ:
            if (te1->single.type == FLOAT)
                printf("fcmp ole ");
            else
                printf("icmp sle ");
            break;
        default:
            fprintf(stderr, "not implemented\n");
    }

    gen_type(te1);

    printf(" ");
    gen_local_temporary_code(e1num);
    printf(", ");
    gen_local_temporary_code(e2num);
    printf("\n");

    switch(etype) {
        case NEQ:
        case EQ:
        case G:
        case L:
        case GEQ:
        case LEQ:
            gen_ext_i1_to_i8(global_state);
            break;
        default:
            //noop
            break;
    }

    return global_state->temp_count;
}

static void gen_exp_args(Exp_list *arg_list, State *global_state) {
    for (int i = 0; arg_list != NULL; i++) {
        arg_list->expnum = gen_exp(arg_list->exp, global_state);
        arg_list = arg_list->next;
    }
}

#define GENARG(list_node, t)	\
    gen_type(t);				\
    printf(" ");				\
    gen_local_temporary_code(list_node->expnum);

static int gen_call(Exp *exp, State *global_state) {
    gen_exp_args(exp->call.explist, global_state);

    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = ");
    printf("call ");
    gen_type(exp->call.funcdef->type);
    printf(" @%s(", exp->call.name);

    Exp_list *curr_exp;
    for (curr_exp = exp->call.explist; curr_exp->next != NULL;
            curr_exp = curr_exp->next) {
        GENARG(curr_exp, get_exp_type_internal(curr_exp->exp));
        printf(", ");
    }

    GENARG(curr_exp, get_exp_type_internal(curr_exp->exp));
    printf(")\n");

    return global_state->temp_count;
}

static int gen_unary_type(Exp *e1, Exp_type etype, State *global_state) {
    int e1num = gen_exp(e1, global_state);
    Type *te1 = get_exp_type_internal(e1);

    gen_local_temporary_code(get_new_temporary(global_state));
    printf(" = ");
    switch(etype) {
        case NOT:
            printf("icmp ne i8 ");
            gen_local_temporary_code(e1num);
            printf(", 0\n");
            gen_local_temporary_code(get_new_temporary(global_state));
            printf(" = ");
            printf("xor i1 ");
            gen_local_temporary_code(global_state->temp_count - 1);
            printf(", true\n");
            gen_ext_i1_to_i8(global_state);
            break;
        case MINUS:
            if( te1->single.type == FLOAT )
                printf("f");

            printf("sub ");
            gen_type(te1);
            printf(" 0, ");
            gen_local_temporary_code(global_state->temp_count - 1);
            printf("\n");
            break;
        default:
            fprintf(stderr, "not implemented\n");
            exit(-1);
    }

    return global_state->temp_count;
}

static int gen_exp(Exp *exp, State *global_state) {
    if( exp == NULL ) {
        fprintf(stderr, "error\n");
        exit(-1);
    }

    int lt, lf, lo;
    switch( exp->tag ) {
        case DIV:
        case MUL:
        case SUB:
        case SUM:
        case NEQ:
        case EQ:
        case GEQ:
        case LEQ:
        case L:
        case G:
            return gen_binary_exp(exp->binary.e1, exp->binary.e2, exp->tag,
                    global_state);
        case AND:
        case OR:
            lt = get_new_label(global_state);
            lf = get_new_label(global_state);
            lo = get_new_label(global_state);

            gen_cond(exp, lt, lf, global_state);
            gen_label(lt);
            gen_unconditional_jump(lo);
            gen_label(lf);
            gen_unconditional_jump(lo);
            gen_label(lo);
            gen_local_temporary_code(get_new_temporary(global_state));
            printf(" = phi i8 [1, ");
            gen_label_ref(lt);
            printf("], [0, ");
            gen_label_ref(lf);
            printf("]\n");
            //%%l%d], [0, %%l%d]\n", lt, lf);
            return global_state->temp_count;
        case NOT:
        case MINUS:
            return gen_unary_type(exp->unary.exp, exp->tag, global_state);
        case EXPATT:
            return gen_exp_att(exp->att.v, exp->att.e, global_state);
        case EXPCHAR:
        case EXPINT:
            return gen_int(exp->expint.i, global_state);
        case EXPFLOAT:
            return gen_float(exp->expfloat.f, global_state);
        case EXPBOOL:
            return gen_bool(exp->expbool.b, global_state);
        case VAR:
            return gen_exp_var(exp, global_state);
        case CALLEXP:
            return gen_call(exp, global_state);
        case NEW:
            return gen_exp_new(exp->new.exp, exp->new.type, global_state);
        default:
            fprintf(stderr, "not implemented genexp: %d\n", exp->tag);
            exit(-1);
    }
}

static void gen_print(Exp *exp, State *global_state) {
    int expnum = gen_exp(exp, global_state);
    Type *exptype = get_exp_type_internal(exp);

    if (exptype->tag == SINGLE) {
        switch(exptype->single.type) {
            case CHAR:
            case INT:
                gen_printf_call(exptype, "int", 4, expnum);
                break;
            case FLOAT:
                gen_printf_call(exptype, "float", 6, expnum);
                break;
            case BOOL:
                gen_printf_call(exptype, "bool", 6, expnum);
                break;
            default:
                fprintf(stderr, "not implemented %d\n", exptype->single.type);
                exit(-1);
        }
    } else {
        gen_printf_call(exptype, "ptr", 4, expnum);
    }
}

static void gen_cmd(Cmd *cmd, State *global_state ) {
    if (cmd == NULL)
        return;

    switch ( cmd->tag ) {
        case PRINT:
            gen_print(cmd->print.exp, global_state);
            gen_cmd( cmd->print.next, global_state);
            break;
        case ATTCMD:
            gen_exp(cmd->att.att, global_state );
            gen_cmd(cmd->att.next, global_state);
            break;
        case RET:
            gen_ret();
            gen_cmd(cmd->cmd_ret.next, global_state);
            break;
        case RETEXP:
            gen_retexp(cmd->cmd_ret_exp.exp, global_state);
            gen_cmd(cmd->cmd_ret_exp.next, global_state);
            break;
        case IF:
            gen_if(cmd, global_state);
            gen_cmd(cmd->cmd_if.next, global_state);
            break;
        case IFELSE:
            gen_ifelse(cmd, global_state);
            gen_cmd(cmd->cmd_ifelse.next, global_state);
            break;
        case WHILE:
            gen_while(cmd, global_state);
            gen_cmd(cmd->cmd_while.next, global_state);
            break;
        case CALLCMD:
            gen_exp(cmd->call.call, global_state);
            gen_cmd(cmd->call.next, global_state);
            break;
        default:
            fprintf(stderr, "not implemented gen cmd: %d\n", cmd->tag);
            exit(-1);
    }

}

static void gen_stat(Stat *stat, State *global_state) {
    if (!stat) {
        return;
    }

    gen_local_vars(stat->vars, global_state);
    gen_cmd(stat->cmds, global_state);
}

static void gen_global_var(Def *var, State *global_state) {
    if (var == NULL)
        return;

    Var *v = var->var.def;

    if (v == NULL)
        return;

    v->tempnum = get_new_temporary(global_state);
    gen_global_temporary_code(v->tempnum);
    printf(" = global ");
    gen_type(v->type);
    printf(" ");
    gen_zero(v->type);
    printf("\n");
    v->is_global = true;
}

static void gen_local_var(Def *var, State *global_state) {
    Var *v = var->var.def;

    v->tempnum = get_new_temporary(global_state);
    gen_local_temporary_code(v->tempnum);
    printf(" = alloca ");
    gen_type(v->type);
    printf("\n");
}

static void gen_param(Def *param, State *global_state, int param_id) {
    Var *p = param->var.def;

    gen_local_var(param, global_state);
    gen_store(p->type, p->type, p->tempnum, param_id, false);
}

static void gen_params(Def *params, State *global_state) {
    if (params == NULL)
        return;

    int i;
    Def *curr_param;

    for (i = 0, curr_param = params; curr_param != NULL;
            curr_param = curr_param->var.next, i++) {
        gen_param(curr_param, global_state, i);
    }
}

static void gen_param_types(Def *params) {
    Def *curr_param;

    if (params == NULL)
        return;

    for (curr_param = params; curr_param->var.next != NULL;
            curr_param = curr_param->var.next) {
        gen_type(curr_param->var.def->type);
        printf(", ");
    }

    gen_type(curr_param->var.def->type);
}

static void gen_func(Def *dfunc, State *global_state) {
    Func *f;

    f = dfunc->func.def;
    if (f == NULL)
        return;

    global_state->cur_func_type = f->type;

    printf("define ");
    gen_type(f->type);
    printf(" @%s(", f->name);
    gen_param_types(f->param);
    printf(") {\n");

    gen_params(f->param, global_state);
    gen_stat(f->stat, global_state);

    printf("}\n");

}

static void gen_local_vars(Def *def, State *global_state) {
    for (Def *curr_def = def; curr_def != NULL;
            curr_def = curr_def->var.next) {
        gen_local_var(curr_def, global_state);
    }
}

static void gen_global_defs(Def *def, State *global_state) {
    for (Def *curr_def = def; curr_def != NULL;) {
        switch (curr_def->tag) {
            case DEFVAR:
                gen_global_var(curr_def, global_state);
                curr_def = curr_def->var.next;
                break;
            case DEFFUNC:
                gen_func(curr_def, global_state);
                curr_def = curr_def->func.next;
                break;
        }
    }
}

void gen_code(State *global_state) {
    printf("@fmtint = internal constant [4 x i8] c\"%%d\\0A\\00\"\n");
    printf("@fmtfloat = internal constant [6 x i8] c\"%%.7f\\0A\\00\"\n");
    printf("@fmtbool = internal constant [6 x i8] c\"%%hhx\\0A\\00\"\n");
    printf("@fmtchar = internal constant [4 x i8] c\"%%c\\0A\\00\"\n");
    printf("@fmtptr = internal constant [4 x i8] c\"%%p\\0A\\00\"\n");
    printf("declare i32 @printf(i8*, ...)\n");
    printf("declare noalias i8* @malloc(i64)\n\n");
    gen_global_defs(GLOBAL_TREE, global_state);
}
