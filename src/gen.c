#include "ast.h"
#include "symbols.h"
#include "typing.h"
#include "aux.h"
#include <stdio.h>
#include <stdbool.h>

#define GENLOAD(t, name) \
    printf("%%e%d = load ", ++(global_state->exp_count));   \
    gen_type(t, false);                                     \
    printf(", ");                                           \
    gen_type(t, false);                                     \
    printf("* @%s\n", name);

#define GENSTORE(t, name, expnum)   \
    printf("store ");               \
    gen_type(t, false);             \
    printf(" %%e%d, ", expnum);     \
    gen_type(t, false);             \
    printf("* @%s\n", name);

static void gen_native_type(Native_types type, bool is_seq) {
    if (is_seq)
        printf("*");

    switch( type ) {
        case INT:
        case CHAR:
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

static void gen_type(Type *type, bool is_seq) {
    if (type == NULL) {
        printf("void");
        return;
    }

    if (type->tag == SINGLE)
        gen_native_type(type->single.type, is_seq ? true : false);
    else
        gen_type(type->seq.next, true);

}

static int gen_exp(Exp *exp, State *global_state);

static int gen_int(int eint, State *global_state) {
    printf("%%e%d = add i32 0, %d\n", ++(global_state->exp_count), eint);
    return global_state->exp_count;
}

static int gen_float(float efloat, State *global_state) {
    printf("%%e%d = fadd double %e, %e\n", ++(global_state->exp_count), 0.0, efloat);
    return global_state->exp_count;
}

static void gen_zero(Type *type) {
    if (type->tag == SEQ) {
        fprintf(stderr, "not implemented\n");
        exit(-1);
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

static int gen_exp_att(Exp *e1, Exp *e2, State *global_state) {
    int expnum = gen_exp(e2, global_state);
    Var *v = e1->var.def;
    Type *te1 = v->type;

    if (!v->is_global) {
        fprintf(stderr, "not implemented\n");
        exit(-1);
    }

    GENSTORE(v->type, v->name, expnum);
    GENLOAD(te1, v->name);

    return global_state->exp_count;
}

static int gen_exp_varid(Exp *exp, State *global_state) {
    Var *v = exp->var.def;

    if (!v->is_global) {
        fprintf(stderr, "not implemented\n");
        exit(-1);
    }

    GENLOAD(v->type, v->name);

    return global_state->exp_count;
}


static int gen_exp(Exp *exp, State *global_state) {
    if( exp == NULL )
        return -1;

    switch( exp->tag ) {
        case EXPINT:
            return gen_int(exp->expint.i, global_state);
        case EXPFLOAT:
            return gen_float(exp->expfloat.f, global_state);
        case EXPATT:
            global_state->cur_line = exp->binary.line;
            return gen_exp_att(exp->binary.e1, exp->binary.e2, global_state);
        case VARID:
            return gen_exp_varid(exp, global_state);
        default:
            fprintf(stderr, "not implemented\n");
            exit(-1);
    }

}

static void gen_print(Exp *exp, State *global_state) {
    Type *exptype = get_exp_type(exp);
    int expnum = gen_exp(exp, global_state);

    if (exptype->tag == SINGLE) {
        switch(exptype->single.type) {
            case INT:
                printf("call i32 (i8*, ...) @printf( i8* getelementptr ([4 x i8], [4 x i8]*"
                        "@fmtint, i32 0, i32 0), i32 %%e%d)\n", expnum);
                break;
            case FLOAT:
                printf("call i32 (i8*, ...) @printf( i8* getelementptr ([6 x i8], [6 x i8]*"
                        "@fmtfloat, i32 0, i32 0), double %%e%d)\n", expnum);
                break;
            default:
                fprintf(stderr, "not implemented\n");
                exit(-1);
        }
    } else {
        fprintf(stderr, "not implemented\n");
        exit(-1);
    }
}

static void gen_cmd( Cmd *cmd, State *global_state ) {
    if (!cmd)
        return;

    switch ( cmd->tag ) {
        case PRINT:
            gen_print( cmd->print.exp, global_state);
            gen_cmd( cmd->print.next, global_state);
            break;
        case ATTCMD:
            gen_exp(cmd->att.att, global_state );
            gen_cmd(cmd->att.next, global_state);
            break;
        default:
            fprintf(stderr, "not implemented\n");
            exit(-1);
    }

}

static void gen_stat(Stat *stat, State *global_state) {
    if (!stat) {
        return;
    }

    gen_cmd( stat->cmds, global_state);
}

static void gen_var(Def *var, bool is_global) {
    if (var == NULL)
        return;

    if (is_global) {
        Var *v = var->var.def;

        printf("@%s = global ", v->name);
        gen_type(v->type, false);
        printf(" ");
        gen_zero(v->type);
        printf("\n");
        v->is_global = is_global;
    } else {
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

    global_state->cur_line = f->line;

    global_state->cur_func_type = f->type; 
    global_state->cur_func_name = f->name;

    printf("define ");
    gen_type(f->type, false);
    printf(" @%s() {\n", f->name);

    if (f->stat)
        gen_stat(f->stat, global_state);

    printf("ret i32 0\n");
    printf("}\n");

}

static void gen_defs(Def *def, State *global_state) {
    if (!def)
        return;

    switch (def->tag) {
        case DEFVAR:
            gen_var(def, true);
            gen_defs(def->var.next, global_state);
            break;
        case DEFFUNC:
            gen_func(def, global_state);
            gen_defs(def->func.next, global_state);
            break;
    }
}

void gen_code(State *global_state) {
    printf("@fmtint = internal constant [4 x i8] c\"%%d\\0A\\00\"\n");
    printf("@fmtfloat = internal constant [6 x i8] c\"%%.7f\\0A\\00\"\n");
    printf("declare i32 @printf(i8*, ...)\n");
    gen_defs(GLOBAL_TREE, global_state);
}
