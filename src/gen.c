#include "ast.h"
#include "symbols.h"
#include "typing.h"
#include "aux.h"
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

static void gen_temporary_code(int expnum) {
    printf("%%t%d", expnum);
}

static int get_new_label(State *global_state) {
    return ++(global_state->label_count);
}

static void copylabel(char *buffer, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vsprintf(buffer, fmt, args);
    va_end(args);
}

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
    gen_temporary_code(expnum);
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
    gen_temporary_code(temp);
    printf("\n");
}

static int get_new_temporary(State *global_state) {
    return ++(global_state->temp_count);
}

static void gen_load(Type *t, int address_temporary, State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = load ");
    gen_type(t);
    printf(", ");
    gen_pointer_reference(t, address_temporary);
}

static void gen_store(Type* t1, Type *t2, int address_temporary, int expnum) {
    printf("store ");
    gen_type(t1);
    printf(" ");
    gen_temporary_code(expnum);
    printf(", ");
    gen_type(t2);
    printf("* ");
    gen_temporary_code(address_temporary);
    printf("\n");
}

static int gen_exp(Exp *exp, State *global_state);
static void gen_defs(Def *def, State *global_state, bool is_global);

static int gen_int(int eint, State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = add i32 0, %d\n", eint);
    return global_state->temp_count;
}

static int gen_float(float efloat, State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = fadd double %e, %e\n", 0.0, efloat);
    return global_state->temp_count;
}

static int gen_bool(bool ebool, State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
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

static int copy_var_address(Type *type, char *name, bool is_global,
        State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = getelementptr ");
    gen_type(type);
    printf(", ");
    gen_type(type);
    printf("* ");
    if(is_global) printf("@");
    else printf("%%");
    printf("%s", name);
    printf(", i32 0\n");

    return global_state->temp_count;
}

static void gen_conditional_jump(char *lt, char *lf, State *global_state) {
    printf("br i1 ");
    gen_temporary_code(global_state->temp_count);
    printf(", label %s, label %s\n", lt, lf);
}

static int gen_cond(Exp *e, char *lt, char *lf, State *global_state) {
    int expnum = gen_exp(e, global_state);

    int truncated_id = get_new_temporary(global_state);
    gen_temporary_code(truncated_id);

    printf(" = trunc i8 ");
    gen_temporary_code(expnum);
    printf(" to i1\n");

    gen_temporary_code(get_new_temporary(global_state));
    printf(" = icmp eq i1 ");
    gen_temporary_code(truncated_id);
    printf(", 0\n");
    //icmp eq rc, 0
    gen_conditional_jump(lf, lt, global_state);
    return global_state->temp_count;
}

static void gen_ext_i1_to_i8(State *global_state) {
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = ");
    printf("zext i1 ");
    gen_temporary_code(global_state->temp_count - 1);
    printf(" to i8\n");
}

static void gen_label(int labelnum) {
    printf("l%d:\n", labelnum);
}

static void gen_unconditional_jump(char *labelstr) {
    printf("br label %s\n", labelstr);
}

// Vamos usar a partir de agora a seguinte notação
// lt = label_true, lf = label_false, le = label_end
static int gen_and(Exp *e) {
    int lt  = get_new_label(global_state);
    int lf  = get_new_label(global_state);
    int le  = get_new_label(global_state);
    int lst = get_new_label(global_state);
    char ltstr[10], lfstr[10], lestr[10], lststr[10];

    copylabel(ltstr, "%%l%d", lt );
    copylabel(lfstr, "%%l%d", lf );
    copylabel(lestr,  "%%l%d", le );
    copylabel(lststr, "%%l%d", lst);

    // testando o lado esquerdo do and
    // Se for verdade queremos ir pra ltstr,
    // se for falso
    gen_cond(e->binary.e1, ltstr, lfstr, global_state);

    gen_label(lt);
    gen_cond(e->binary.e2, lststr, lfstr, global_state);
    printf("\n");

    gen_label(lst);
    gen_unconditional_jump(lestr);
    printf("\n");

    gen_label(lf);
    gen_unconditional_jump(lestr);
    printf("\n");

    printf("l%d:\n", le);

    gen_temporary_code(get_new_temporary(global_state));
    printf(" = phi i1 [false, %s], [true, %s]\n", lfstr, lststr);

    gen_ext_i1_to_i8(global_state);
    printf("\n");

    return global_state->temp_count;
}

static int gen_var(Var *v, State *global_state) {
    return copy_var_address(v->type, v->name, v->is_global, global_state);
}

static int gen_array(RefVar *r, State *global_state) {
    int expvarnum = gen_exp(r->refa.v, global_state);
    int expindexnum = gen_exp(r->refa.idx, global_state);
    Type *indexedtype = get_ref_type(r);

    gen_temporary_code(get_new_temporary(global_state));
    printf(" = getelementptr ");
    gen_type(indexedtype);
    printf(", ");
    gen_type(indexedtype);
    printf("* ");
    printf(" ");
    gen_temporary_code(expvarnum);
    printf(", i32 ");
    gen_temporary_code(expindexnum);
    printf("\n");

    return global_state->temp_count;
}

static int gen_or(Exp *e) {
    int lt  = get_new_label(global_state);
    int lf  = get_new_label(global_state);
    int le  = get_new_label(global_state);
    int lsf = get_new_label(global_state);
    char ltstr[10], lfstr[10], lestr[10], lsfstr[10];

    copylabel(ltstr, "%%l%d", lt );
    copylabel(lfstr, "%%l%d", lf );
    copylabel(lestr, "%%l%d", le );
    copylabel(lsfstr, "%%l%d", lsf);

    // testando o lado esquerdo do and
    // Se for verdade queremos ir pra ltstr,
    // se for falso
    gen_cond(e->binary.e1, ltstr, lfstr, global_state);

    gen_label(lt);
    gen_unconditional_jump(lsfstr);
    printf("\n");

    gen_label(lf);
    gen_cond(e->binary.e2, lsfstr, lestr, global_state);
    printf("\n");

    gen_label(lsf);
    gen_unconditional_jump(lestr);
    printf("\n");

    gen_label(le);

    gen_temporary_code(get_new_temporary(global_state));
    printf(" = phi i1 [false, %s], [true, %s]\n", lfstr, lsfstr);

    gen_ext_i1_to_i8(global_state);
    printf("\n");

    return global_state->temp_count;
}

static int gen_ref(RefVar *r, State *global_state) {
    switch (r->tag) {
        case REF_VAR:
            return gen_var(r->refv.v, global_state);
            break;
        case REF_ARRAY:
            return gen_array(r, global_state);
            break;
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

    gen_store(reftype, exptype, refnum, expnum);

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
    int tsize = get_type_size(type);

    gen_temporary_code(get_new_temporary(global_state));
    printf(" = sext i32 ");
    gen_temporary_code(expnum);
    printf(" to i64\n");
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = mul i64 ");
    gen_temporary_code(global_state->temp_count - 1);
    printf(", %d\n", tsize);
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = call i8* @malloc(i64 ");
    gen_temporary_code(global_state->temp_count - 1);
    printf(")\n");
    gen_temporary_code(get_new_temporary(global_state));
    printf(" = bitcast i8 * ");
    gen_temporary_code(global_state->temp_count - 1);
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
    gen_temporary_code(numexp);
    printf("\n");

}

static void gen_stat(Stat *stat, State *global_state);

static void gen_while(Cmd *cmd, State *global_state) {
    int l_cond = get_new_label(global_state);
    int l_body = get_new_label(global_state);
    int l_end = get_new_label(global_state);

    char l_cond_str[10], l_body_str[10], l_end_str[10];

    copylabel(l_cond_str, "%%l%d", l_cond );
    copylabel(l_body_str, "%%l%d", l_body );
    copylabel(l_end_str , "%%l%d", l_end  );

    // pula para o inicio do while
    gen_unconditional_jump(l_cond_str);

    // checa condicao
    gen_label(l_cond);
    gen_cond(cmd->cmd_while.exp, l_body_str, l_end_str, global_state);

    // corpo do while
    gen_label(l_body);
    gen_stat(cmd->cmd_while.stat, global_state);
    gen_unconditional_jump(l_cond_str);

    // final do while
    gen_label(l_end);

}

static void gen_if(Cmd *cmd, State *global_state) {
    int lt  = get_new_label(global_state);
    int lf  = get_new_label(global_state);
    char ltstr[10], lfstr[10];

    copylabel(ltstr, "%%l%d", lt );
    copylabel(lfstr, "%%l%d", lf );

    gen_cond(cmd->cmd_if.exp, ltstr, lfstr, global_state);
    gen_label(lt);
    gen_stat(cmd->cmd_if.stat, global_state);
    gen_unconditional_jump(lfstr);
    printf("\n");

    printf("l%d:\n", lf);

}

static void gen_ifelse(Cmd *cmd, State *global_state) {
    int lt  = get_new_label(global_state);
    int le  = get_new_label(global_state);
    int lo  = get_new_label(global_state);
    char ltstr[10], lestr[10], lostr[10];

    copylabel(ltstr, "%%l%d", lt );
    copylabel(lestr, "%%l%d", le );
    copylabel(lostr, "%%l%d", lo );

    gen_cond(cmd->cmd_ifelse.exp, ltstr, lestr, global_state);
    gen_label(lt);
    gen_stat(cmd->cmd_ifelse.stat, global_state);
    gen_unconditional_jump(lostr);
    printf("\n");

    gen_label(le);
    gen_stat(cmd->cmd_ifelse.stat2, global_state);
    gen_unconditional_jump(lostr);
    printf("\n");

    gen_label(lo);

}

static int gen_binary_exp(Exp *e1, Exp *e2, Exp_type etype,
        State *global_state) {
    Type *te1 = get_exp_type_internal(e1);
    int e1num = gen_exp(e1, global_state);
    int e2num = gen_exp(e2, global_state);

    gen_temporary_code(get_new_temporary(global_state));
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
            exit(-1);
    }

    gen_type(te1);

    printf(" ");
    gen_temporary_code(e1num);
    printf(", ");
    gen_temporary_code(e2num);
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

static int gen_unary_type(Exp *e1, Exp_type etype, State *global_state) {
    int e1num = gen_exp(e1, global_state);
    Type *te1 = get_exp_type_internal(e1);

    gen_temporary_code(get_new_temporary(global_state));
    printf(" = ");
    switch(etype) {
        case NOT:
            printf("icmp ne i8 ");
            gen_temporary_code(e1num);
            printf(", 0\n");
            gen_temporary_code(get_new_temporary(global_state));
            printf(" = ");
            printf("xor i1 ");
            gen_temporary_code(global_state->temp_count - 1);
            printf(", true\n");
            gen_ext_i1_to_i8(global_state);
            break;
        case MINUS:
            if( te1->single.type == FLOAT )
                printf("f");

            printf("sub ");
            gen_type(te1);
            printf(" 0, ");
            gen_temporary_code(global_state->temp_count - 1);
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
        case OR:
            return gen_or(exp);
        case AND:
            return gen_and(exp);
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

static void gen_cmd( Cmd *cmd, State *global_state ) {
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
        default:
            fprintf(stderr, "not implemented gen cmd: %d\n", cmd->tag);
            exit(-1);
    }

}

static void gen_stat(Stat *stat, State *global_state) {
    if (!stat) {
        return;
    }

    gen_defs(stat->vars, global_state, false);
    gen_cmd(stat->cmds, global_state);
}

static void gen_global_var(Def *var) {
    Var *v = var->var.def;

    printf("@%s = global ", v->name);
    gen_type(v->type);
    printf(" ");
    gen_zero(v->type);
    printf("\n");
    v->is_global = true;
}

static void gen_local_var(Def *var) {
    Var *v = var->var.def;

    printf("%%%s = alloca ", v->name);
    gen_type(v->type);
    printf("\n");
}

static void gen_params(Def *params, State *global_state) {
    if( params != NULL ) {
        fprintf(stderr, "not implemented\n");
        exit(-1);
    }
}

static void gen_func(Def *dfunc, State *global_state) {
    Func *f;

    if (dfunc == NULL)
        return;

    f = dfunc->func.def;
    if (f == NULL)
        return;

    gen_params(f->param, global_state);

    global_state->cur_func_type = f->type;

    printf("define ");
    gen_type(f->type);
    printf(" @%s() {\n", f->name);

    if (f->stat)
        gen_stat(f->stat, global_state);

    printf("}\n");

}

static void gen_defs(Def *def, State *global_state, bool is_global) {
    if (!def)
        return;

    switch (def->tag) {
        case DEFVAR:
            if (is_global)
                gen_global_var(def);
            else
                gen_local_var(def);

            gen_defs(def->var.next, global_state, is_global);
            break;
        case DEFFUNC:
            gen_func(def, global_state);
            gen_defs(def->func.next, global_state, is_global);
            break;
    }
}

void gen_code(State *global_state) {
    printf("@fmtint = internal constant [4 x i8] c\"%%d\\0A\\00\"\n");
    printf("@fmtfloat = internal constant [6 x i8] c\"%%.7f\\0A\\00\"\n");
    printf("@fmtbool = internal constant [6 x i8] c\"%%hhx\\0A\\00\"\n");
    printf("@fmtchar = internal constant [4 x i8] c\"%%c\\0A\\00\"\n");
    printf("@fmtptr = internal constant [4 x i8] c\"%%p\\0A\\00\"\n");
    printf("declare i32 @printf(i8*, ...)\n");
    printf("declare noalias i8* @malloc(i64)\n");
    gen_defs(GLOBAL_TREE, global_state, true);
}
