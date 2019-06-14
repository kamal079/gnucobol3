/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 28 "parser.y" /* yacc.c:339  */

#include "config.h"

#include <stdlib.h>
#include <string.h>

#define	COB_IN_PARSER	1
#include "cobc.h"
#include "tree.h"

#ifndef	_STDLIB_H
#define	_STDLIB_H 1
#endif

#define YYSTYPE			cb_tree
#define yyerror(x)		cb_error_always ("%s", x)

#define emit_statement(x) \
do { \
  if (!skip_statements) { \
	CB_ADD_TO_CHAIN (x, current_program->exec_list); \
  } \
}  ONCE_COB

#define push_expr(type, node) \
  current_expr = cb_build_list (cb_int (type), node, current_expr)

/* Statement terminator definitions */
#define TERM_NONE		0
#define TERM_ACCEPT		1U
#define TERM_ADD		2U
#define TERM_CALL		3U
#define TERM_COMPUTE		4U
#define TERM_DELETE		5U
#define TERM_DISPLAY		6U
#define TERM_DIVIDE		7U
#define TERM_EVALUATE		8U
#define TERM_IF			9U
#define TERM_JSON		10U
#define TERM_MODIFY		11U
#define TERM_MULTIPLY		12U
#define TERM_PERFORM		13U
#define TERM_READ		14U
#define TERM_RECEIVE		15U
#define TERM_RETURN		16U
#define TERM_REWRITE		17U
#define TERM_SEARCH		18U
#define TERM_START		19U
#define TERM_STRING		20U
#define TERM_SUBTRACT		21U
#define TERM_UNSTRING		22U
#define TERM_WRITE		23U
#define TERM_XML		24U
#define TERM_MAX		25U	/* Always last entry, used for array size */

#define	TERMINATOR_WARNING(x,z)	terminator_warning (x, TERM_##z, #z)
#define	TERMINATOR_ERROR(x,z)	terminator_error (x, TERM_##z, #z)
#define	TERMINATOR_CLEAR(x,z)	terminator_clear (x, TERM_##z)

/* Defines for duplicate checks */
/* Note - We use <= 16 for common item definitions and */
/* > 16 for non-common item definitions e.g. REPORT and SCREEN */
#define	SYN_CLAUSE_1		(1U << 0)
#define	SYN_CLAUSE_2		(1U << 1)
#define	SYN_CLAUSE_3		(1U << 2)
#define	SYN_CLAUSE_4		(1U << 3)
#define	SYN_CLAUSE_5		(1U << 4)
#define	SYN_CLAUSE_6		(1U << 5)
#define	SYN_CLAUSE_7		(1U << 6)
#define	SYN_CLAUSE_8		(1U << 7)
#define	SYN_CLAUSE_9		(1U << 8)
#define	SYN_CLAUSE_10		(1U << 9)
#define	SYN_CLAUSE_11		(1U << 10)
#define	SYN_CLAUSE_12		(1U << 11)
#define	SYN_CLAUSE_13		(1U << 12)
#define	SYN_CLAUSE_14		(1U << 13)
#define	SYN_CLAUSE_15		(1U << 14)
#define	SYN_CLAUSE_16		(1U << 15)
#define	SYN_CLAUSE_17		(1U << 16)
#define	SYN_CLAUSE_18		(1U << 17)
#define	SYN_CLAUSE_19		(1U << 18)
#define	SYN_CLAUSE_20		(1U << 19)
#define	SYN_CLAUSE_21		(1U << 20)
#define	SYN_CLAUSE_22		(1U << 21)
#define	SYN_CLAUSE_23		(1U << 22)
#define	SYN_CLAUSE_24		(1U << 23)
#define	SYN_CLAUSE_25		(1U << 24)
#define	SYN_CLAUSE_26		(1U << 25)
#define	SYN_CLAUSE_27		(1U << 26)
#define	SYN_CLAUSE_28		(1U << 27)
#define	SYN_CLAUSE_29		(1U << 28)
#define	SYN_CLAUSE_30		(1U << 29)
#define	SYN_CLAUSE_31		(1U << 30)
#define	SYN_CLAUSE_32		(1U << 31)

#define	EVAL_DEPTH		32
#define	PROG_DEPTH		16

/* Global variables */

struct cb_program		*current_program = NULL;
struct cb_statement		*current_statement = NULL;
struct cb_label			*current_section = NULL;
struct cb_label			*current_paragraph = NULL;
struct cb_field		*external_defined_fields_ws;
struct cb_field		*external_defined_fields_global;
cb_tree				defined_prog_list = NULL;
int				cb_exp_line = 0;

cb_tree				cobc_printer_node = NULL;
int				functions_are_all = 0;
int				non_const_word = 0;
int				suppress_data_exceptions = 0;
unsigned int			cobc_repeat_last_token = 0;
unsigned int			cobc_in_id = 0;
unsigned int			cobc_in_procedure = 0;
unsigned int			cobc_in_repository = 0;
unsigned int			cobc_force_literal = 0;
unsigned int			cobc_cs_check = 0;
unsigned int			cobc_allow_program_name = 0;
unsigned int			cobc_in_xml_generate_body = 0;
unsigned int			cobc_in_xml_parse_body = 0;
unsigned int			cobc_in_json_generate_body = 0;

/* Local variables */

enum tallying_phrase {
	NO_PHRASE,
	FOR_PHRASE,
	CHARACTERS_PHRASE,
	ALL_LEADING_TRAILING_PHRASES,
	VALUE_REGION_PHRASE
};

enum key_clause_type {
	NO_KEY,
	RECORD_KEY,
	RELATIVE_KEY
};

static struct cb_statement	*main_statement;

static cb_tree			current_expr;
static struct cb_field		*current_field;
static struct cb_field		*control_field;
static struct cb_field		*description_field;
static struct cb_file		*current_file;
static struct cb_cd		*current_cd;
static struct cb_report		*current_report;
static struct cb_report		*report_instance;
static struct cb_key_component	*key_component_list;

static struct cb_file		*linage_file;
static cb_tree			next_label_list;

static const char			*stack_progid[PROG_DEPTH];

static enum cb_storage		current_storage;

static cb_tree			perform_stack;
static cb_tree			qualifier;
static cb_tree			keys_list;

static cb_tree			save_tree;
static cb_tree			start_tree;

static unsigned int		check_unreached;
static unsigned int		in_declaratives;
static unsigned int		in_debugging;
static unsigned int		current_linage;
static unsigned int		report_count;
static unsigned int		first_prog;
static unsigned int		setup_from_identification;
static unsigned int		use_global_ind;
static unsigned int		same_area;
static unsigned int		inspect_keyword;
static unsigned int		main_flag_set;
static int			next_label_id;
static int			eval_level;
static int			eval_inc;
static int			eval_inc2;
static int			depth;
static int			first_nested_program;
static int			call_mode;
static int			size_mode;
static cob_flags_t		set_attr_val_on;
static cob_flags_t		set_attr_val_off;
static cob_flags_t		check_duplicate;
static cob_flags_t		check_on_off_duplicate;
static cob_flags_t		check_pic_duplicate;
static cob_flags_t		check_line_col_duplicate;
static unsigned int		skip_statements;
static unsigned int		start_debug;
static unsigned int		save_debug;
static unsigned int		needs_field_debug;
static unsigned int		needs_debug_item;
static unsigned int		env_div_seen;
static cob_flags_t		header_check;
static unsigned int		call_nothing;
static enum tallying_phrase	previous_tallying_phrase;
static cb_tree			default_rounded_mode;
static enum key_clause_type	key_type;

static enum cb_display_type	display_type;
static int			is_first_display_item;
static cb_tree			advancing_value;
static cb_tree			upon_value;
static cb_tree			line_column;

static cb_tree			ml_suppress_list;
static cb_tree			xml_encoding;
static int			with_xml_dec;
static int			with_attrs;

static cb_tree			alphanumeric_collation;
static cb_tree			national_collation;

static enum cb_ml_suppress_category	ml_suppress_category;

static int			term_array[TERM_MAX];
static cb_tree			eval_check[EVAL_DEPTH][EVAL_DEPTH];

static const char		*backup_source_file = NULL;
static int			backup_source_line = 0;

/* Defines for header presence */

#define	COBC_HD_ENVIRONMENT_DIVISION	(1U << 0)
#define	COBC_HD_CONFIGURATION_SECTION	(1U << 1)
#define	COBC_HD_SPECIAL_NAMES		(1U << 2)
#define	COBC_HD_INPUT_OUTPUT_SECTION	(1U << 3)
#define	COBC_HD_FILE_CONTROL		(1U << 4)
#define	COBC_HD_I_O_CONTROL		(1U << 5)
#define	COBC_HD_DATA_DIVISION		(1U << 6)
#define	COBC_HD_FILE_SECTION		(1U << 7)
#define	COBC_HD_WORKING_STORAGE_SECTION	(1U << 8)
#define	COBC_HD_LOCAL_STORAGE_SECTION	(1U << 9)
#define	COBC_HD_LINKAGE_SECTION		(1U << 10)
#define	COBC_HD_COMMUNICATION_SECTION	(1U << 11)
#define	COBC_HD_REPORT_SECTION		(1U << 12)
#define	COBC_HD_SCREEN_SECTION		(1U << 13)
#define	COBC_HD_PROCEDURE_DIVISION	(1U << 14)
#define	COBC_HD_PROGRAM_ID		(1U << 15)
#define	COBC_HD_SOURCE_COMPUTER		(1U << 16)
#define	COBC_HD_OBJECT_COMPUTER		(1U << 17)
#define	COBC_HD_REPOSITORY		(1U << 18)

/* Static functions */

static void
begin_statement (const char *name, const unsigned int term)
{
	if (check_unreached) {
		cb_warning (cb_warn_unreachable, _("unreachable statement '%s'"), name);
	}
	current_paragraph->flag_statement = 1;
	current_statement = cb_build_statement (name);
	CB_TREE (current_statement)->source_file = cb_source_file;
	CB_TREE (current_statement)->source_line = cb_source_line;
	current_statement->flag_in_debug = in_debugging;
	emit_statement (CB_TREE (current_statement));
	if (term) {
		term_array[term]++;
	}
	main_statement = current_statement;
}

/* create a new statement with base attributes of current_statement
   and set this as new current_statement */
static void
begin_implicit_statement (void)
{
	struct cb_statement	*new_statement;
	new_statement = cb_build_statement (NULL);
	new_statement->common = current_statement->common;
	new_statement->name = current_statement->name;
	new_statement->flag_in_debug = !!in_debugging;
	new_statement->flag_implicit = 1;
	current_statement = new_statement;
	main_statement->body = cb_list_add (main_statement->body,
					    CB_TREE (current_statement));
}

# if 0 /* activate only for debugging purposes for attribs
	FIXME: Replace by DEBUG_LOG function */
static
void print_bits (cob_flags_t num)
{
	unsigned int 	size = sizeof (cob_flags_t);
	unsigned int	max_pow = 1 << (size * 8 - 1);
	int 		i = 0;

	for(; i < size * 8; ++i){
		/* Print last bit and shift left. */
		fprintf (stderr, "%u ", num & max_pow ? 1 : 0);
		num = num << 1;
	}
	fprintf (stderr, "\n");
}
#endif

/* functions for storing current position and
   assigning it to a cb_tree after its parsing is finished */
static COB_INLINE
void backup_current_pos (void)
{
	backup_source_file = cb_source_file;
	backup_source_line = cb_source_line;
}

#if 0 /* currently not used */
static COB_INLINE
void set_pos_from_backup (cb_tree x)
{
	x->source_file = backup_source_file;
	x->source_line = backup_source_line;
}
#endif

static void
emit_entry (const char *name, const int encode, cb_tree using_list, cb_tree convention)
{
	cb_tree		l;
	cb_tree		label;
	cb_tree		x;
	cb_tree		entry_conv;
	struct cb_field	*f, *ret_f;
	int			param_num;
	char		buff[COB_MINI_BUFF];

	snprintf (buff, (size_t)COB_MINI_MAX, "E$%s", name);
	label = cb_build_label (cb_build_reference (buff), NULL);
	if (encode) {
		CB_LABEL (label)->name = cb_encode_program_id (name);
		CB_LABEL (label)->orig_name = name;
	} else {
		CB_LABEL (label)->name = name;
		CB_LABEL (label)->orig_name = current_program->orig_program_id;
	}
	CB_LABEL (label)->flag_begin = 1;
	CB_LABEL (label)->flag_entry = 1;
	label->source_line = backup_source_line;
	emit_statement (label);

	if (current_program->flag_debugging) {
		emit_statement (cb_build_debug (cb_debug_contents,
						"START PROGRAM", NULL));
	}

	param_num = 1;
	for (l = using_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
			f = CB_FIELD (cb_ref (x));
			if (!current_program->flag_chained) {
				if (f->storage != CB_STORAGE_LINKAGE) {
					cb_error_x (x, _("'%s' is not in LINKAGE SECTION"), f->name);
				}
				if (f->flag_item_based || f->flag_external) {
					cb_error_x (x, _("'%s' cannot be BASED/EXTERNAL"), f->name);
				}
				f->flag_is_pdiv_parm = 1;
			} else {
				if (f->storage != CB_STORAGE_WORKING) {
					cb_error_x (x, _("'%s' is not in WORKING-STORAGE SECTION"), f->name);
				}
				f->flag_chained = 1;
				f->param_num = param_num;
				param_num++;
			}
			if (f->level != 01 && f->level != 77) {
				cb_error_x (x, _("'%s' not level 01 or 77"), f->name);
			}
			if (f->redefines) {
				cb_error_x (x, _ ("'%s' REDEFINES field not allowed here"), f->name);
			}
			/* add a "receiving" entry for the USING parameter */
			if (cb_listing_xref) {
				cobc_xref_link (&f->xref, CB_REFERENCE (x)->common.source_line, 1);
			}
		}
	}


	if (current_program->returning &&
		cb_ref (current_program->returning) != cb_error_node) {
		ret_f = CB_FIELD (cb_ref (current_program->returning));
		if (ret_f->redefines) {
			cb_error_x (current_program->returning,
				_("'%s' REDEFINES field not allowed here"), ret_f->name);
		}
	} else {
		ret_f = NULL;
	}

	/* Check returning item against using items when FUNCTION */
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION && ret_f) {
		for (l = using_list; l; l = CB_CHAIN (l)) {
			x = CB_VALUE (l);
			if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
				f = CB_FIELD (cb_ref (x));
				if (ret_f == f) {
					cb_error_x (x, _("'%s' USING item duplicates RETURNING item"), f->name);
				}
			}
		}
	}

	for (l = current_program->entry_list; l; l = CB_CHAIN (l)) {
		if (strcmp ((const char *)name,
			    (const char *)(CB_LABEL(CB_PURPOSE(l))->name)) == 0) {
			cb_error_x (CB_TREE (current_statement),
				    _("ENTRY '%s' duplicated"), name);
		}
	}

	if (convention) {
		entry_conv = convention;
	} else {
		entry_conv = current_program->entry_convention;
	}

	current_program->entry_list =
		cb_list_append (current_program->entry_list,
				CB_BUILD_PAIR (label, CB_BUILD_PAIR(entry_conv, using_list)));
}

static size_t
increment_depth (void)
{
	if (++depth >= PROG_DEPTH) {
		cb_error (_("maximum nested program depth exceeded (%d)"),
			  PROG_DEPTH);
		return 1;
	}
	return 0;
}

static void
terminator_warning (cb_tree stmt, const unsigned int termid,
		    const char *name)
{
	char		terminator[32];

	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	/* LCOV_EXCL_START */
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_warning", name);
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	snprintf (terminator, 32, "END-%s", name);
	if (is_reserved_word (terminator)) {
		cb_warning_x (cb_warn_terminator, CB_TREE (current_statement),
			_("%s statement not terminated by %s"), name, terminator);
	}

	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static void
terminator_error (cb_tree stmt, const unsigned int termid, const char *name)
{
	char		terminator[32];

	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	/* LCOV_EXCL_START */
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_error", name);
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	snprintf (terminator, 32, "END-%s", name);
	if (is_reserved_word (terminator)) {
		cb_error_x (CB_TREE (current_statement),
			_("%s statement not terminated by %s"), name, terminator);
	} else {
		cb_error_x (CB_TREE (current_statement),
			_("%s statement not terminated"), name);
	}

	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static void
terminator_clear (cb_tree stmt, const unsigned int termid)
{
	struct cb_perform	*p;
	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	/* LCOV_EXCL_START */
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_warning", current_statement->name);
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	if (termid == TERM_PERFORM
	 && perform_stack) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (p->perform_type == CB_PERFORM_UNTIL) {
			cb_terminate_cond ();
		}
	}
	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static int
literal_value (cb_tree x)
{
	if (x == cb_space) {
		return ' ';
	} else if (x == cb_zero) {
		return '0';
	} else if (x == cb_quote) {
		return cb_flag_apostrophe ? '\'' : '"';
	} else if (x == cb_null) {
		return 0;
	} else if (x == cb_low) {
		return 0;
	} else if (x == cb_high) {
		return 255;
	} else if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
		return cb_get_int (x);
	} else {
		return CB_LITERAL (x)->data[0];
	}
}

static void
setup_use_file (struct cb_file *fileptr)
{
	struct cb_file	*newptr;

	if (fileptr->organization == COB_ORG_SORT) {
		cb_error (_("USE statement invalid for SORT file"));
	}
	if (fileptr->flag_global) {
		newptr = cobc_parse_malloc (sizeof(struct cb_file));
		*newptr = *fileptr;
		newptr->handler = current_section;
		newptr->handler_prog = current_program;
		if (!use_global_ind) {
			current_program->local_file_list =
				cb_list_add (current_program->local_file_list,
					     CB_TREE (newptr));
		} else {
			current_program->global_file_list =
				cb_list_add (current_program->global_file_list,
					     CB_TREE (newptr));
		}
	} else {
		fileptr->handler = current_section;
	}
}

static void
emit_duplicate_clause_message (const char *clause)
{
	/* FIXME: replace by a new warning level that is set
	   to warn/error depending on cb_relaxed_syntax_checks */
	if (cb_relaxed_syntax_checks) {
		cb_warning (COBC_WARN_FILLER, _("duplicate %s clause"), clause);
	} else {
		cb_error (_("duplicate %s clause"), clause);
	}
}

static void
check_repeated (const char *clause, const cob_flags_t bitval, cob_flags_t *already_seen)
{
	if (*already_seen & bitval) {
		emit_duplicate_clause_message (clause);
	} else {
		*already_seen |= bitval;
	}
}

static void
error_if_no_page_lines_limit (const char *phrase)
{
	if (!current_report->lines && !current_report->t_lines) {
		cb_error (_("Cannot specify %s without number of lines on page"),
			  phrase);
	}
}

static void
setup_occurs (void)
{
	check_repeated ("OCCURS", SYN_CLAUSE_7, &check_pic_duplicate);
	if (current_field->indexes == COB_MAX_SUBSCRIPTS) {
		cb_error (_ ("maximum OCCURS depth exceeded (%d)"),
			COB_MAX_SUBSCRIPTS);
	} else {
		current_field->indexes++;
	}

	if (current_field->flag_unbounded) {
		if (current_field->storage != CB_STORAGE_LINKAGE) {
			cb_error_x (CB_TREE(current_field), _("'%s' is not in LINKAGE SECTION"),
				cb_name (CB_TREE(current_field)));
		}
	}

	if (current_field->flag_item_based) {
		cb_error (_ ("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else if (current_field->flag_external) {
		cb_error (_ ("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
	}
	current_field->flag_occurs = 1;
}

static void
setup_occurs_min_max (cb_tree occurs_min, cb_tree occurs_max)
{
	if (occurs_max) {
		current_field->occurs_min = cb_get_int (occurs_min);
		if (occurs_max != cb_int0) {
			current_field->occurs_max = cb_get_int (occurs_max);
			if (!current_field->depending) {
				if (cb_relaxed_syntax_checks) {
					cb_warning (COBC_WARN_FILLER, _ ("TO phrase without DEPENDING phrase"));
					cb_warning (COBC_WARN_FILLER, _ ("maximum number of occurrences assumed to be exact number"));
					current_field->occurs_min = 1; /* CHECKME: why using 1 ? */
				} else {
					cb_error (_ ("TO phrase without DEPENDING phrase"));
				}
			}
			if (current_field->occurs_max <= current_field->occurs_min) {
				cb_error (_ ("OCCURS TO must be greater than OCCURS FROM"));
			}
		} else {
			current_field->occurs_max = 0;
		}
	} else {
		current_field->occurs_min = 1; /* CHECKME: why using 1 ? */
		current_field->occurs_max = cb_get_int (occurs_min);
		if (current_field->depending) {
			cb_verify (cb_odo_without_to, _ ("OCCURS DEPENDING ON without TO phrase"));
		}
	}
}

static void
check_relaxed_syntax (const cob_flags_t lev)
{
	const char	*s;

	switch (lev) {
	case COBC_HD_ENVIRONMENT_DIVISION:
		s = "ENVIRONMENT DIVISION";
		break;
	case COBC_HD_CONFIGURATION_SECTION:
		s = "CONFIGURATION SECTION";
		break;
	case COBC_HD_SPECIAL_NAMES:
		s = "SPECIAL-NAMES";
		break;
	case COBC_HD_INPUT_OUTPUT_SECTION:
		s = "INPUT-OUTPUT SECTION";
		break;
	case COBC_HD_FILE_CONTROL:
		s = "FILE-CONTROL";
		break;
	case COBC_HD_I_O_CONTROL:
		s = "I-O-CONTROL";
		break;
	case COBC_HD_DATA_DIVISION:
		s = "DATA DIVISION";
		break;
	case COBC_HD_FILE_SECTION:
		s = "FILE SECTION";
		break;
	case COBC_HD_WORKING_STORAGE_SECTION:
		s = "WORKING-STORAGE SECTION";
		break;
	case COBC_HD_LOCAL_STORAGE_SECTION:
		s = "LOCAL-STORAGE SECTION";
		break;
	case COBC_HD_LINKAGE_SECTION:
		s = "LINKAGE SECTION";
		break;
	case COBC_HD_COMMUNICATION_SECTION:
		s = "COMMUNICATION SECTION";
		break;
	case COBC_HD_REPORT_SECTION:
		s = "REPORT SECTION";
		break;
	case COBC_HD_SCREEN_SECTION:
		s = "SCREEN SECTION";
		break;
	case COBC_HD_PROCEDURE_DIVISION:
		s = "PROCEDURE DIVISION";
		break;
	case COBC_HD_PROGRAM_ID:
		s = "PROGRAM-ID";
		break;
	/* LCOV_EXCL_START */
	default:
		s = _("unknown");
		break;
	/* LCOV_EXCL_STOP */
	}
	if (cb_relaxed_syntax_checks) {
		cb_warning (COBC_WARN_FILLER, _("%s header missing - assumed"), s);
	} else {
		cb_error (_("%s header missing"), s);
	}
}

/* check if headers are present - return 0 if fine, 1 if missing
   Lev1 must always be present and is checked
   Lev2/3/4, if non-zero (forced) may be present
*/
static int
check_headers_present (const cob_flags_t lev1, const cob_flags_t lev2,
		       const cob_flags_t lev3, const cob_flags_t lev4)
{
	int ret = 0;
	if (!(header_check & lev1)) {
		header_check |= lev1;
		check_relaxed_syntax (lev1);
		ret = 1;
	}
	if (lev2) {
		if (!(header_check & lev2)) {
			header_check |= lev2;
			check_relaxed_syntax (lev2);
			ret = 1;
		}
	}
	if (lev3) {
		if (!(header_check & lev3)) {
			header_check |= lev3;
			check_relaxed_syntax (lev3);
			ret = 1;
		}
	}
	if (lev4) {
		if (!(header_check & lev4)) {
			header_check |= lev4;
			check_relaxed_syntax (lev4);
			ret = 1;
		}
	}
	return ret;
}

/*
  TO-DO: Refactor header checks - have several header_checks: division_header,
  section_header, paragraph_header, sentence_type
*/
static void
set_conf_section_part (const cob_flags_t part)
{
	header_check &= ~COBC_HD_SOURCE_COMPUTER;
	header_check &= ~COBC_HD_OBJECT_COMPUTER;
	header_check &= ~COBC_HD_SPECIAL_NAMES;
	header_check &= ~COBC_HD_REPOSITORY;
	header_check |= part;
}

static const char *
get_conf_section_part_name (const cob_flags_t part)
{
	if (part == COBC_HD_SOURCE_COMPUTER) {
		return "SOURCE-COMPUTER";
	} else if (part == COBC_HD_OBJECT_COMPUTER) {
		return "OBJECT-COMPUTER";
	} else if (part == COBC_HD_SPECIAL_NAMES) {
		return "SPECIAL-NAMES";
	} else if (part == COBC_HD_REPOSITORY) {
		return "REPOSITORY";
	/* LCOV_EXCL_START */
	} else {
		/* This should never happen (and therefore doesn't get a translation) */
		cb_error ("unexpected configuration section part " CB_FMT_LLU, part);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

static int
get_conf_section_part_order (const cob_flags_t part)
{
	if (part == COBC_HD_SOURCE_COMPUTER) {
		return 1;
	} else if (part == COBC_HD_OBJECT_COMPUTER) {
		return 2;
	} else if (part == COBC_HD_SPECIAL_NAMES) {
		return 3;
	} else if (part == COBC_HD_REPOSITORY) {
		return 4;
	/* LCOV_EXCL_START */
	} else {
		/* This should never happen (and therefore doesn't get a translation) */
		cb_error ("unexpected configuration section part " CB_FMT_LLU, part);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

static void
check_conf_section_order (const cob_flags_t part)
{
	const cob_flags_t	prev_part
		= header_check & (COBC_HD_SOURCE_COMPUTER
				  | COBC_HD_OBJECT_COMPUTER
				  | COBC_HD_SPECIAL_NAMES
				  | COBC_HD_REPOSITORY);
#define MESSAGE_LEN 100
	char			message[MESSAGE_LEN] = { '\0' };

	if (prev_part == 0) {
		return;
	}

	if (prev_part == part) {
		cb_error (_("duplicate %s"), get_conf_section_part_name (part));
	} else if (get_conf_section_part_order (part) < get_conf_section_part_order (prev_part)) {
		snprintf (message, MESSAGE_LEN, _("%s incorrectly after %s"),
			  get_conf_section_part_name (part),
			  get_conf_section_part_name (prev_part));
		cb_verify (cb_incorrect_conf_sec_order, message);
	}
}

#undef MESSAGE_LEN

static void
build_words_for_nested_programs (void)
{
	cb_tree		x;
	cb_tree		y;

	/* Inherit special name mnemonics from parent */
	for (x = current_program->mnemonic_spec_list; x; x = CB_CHAIN (x)) {
		y = cb_build_reference (cb_name(CB_PURPOSE(x)));
		if (CB_SYSTEM_NAME_P (CB_VALUE(x))) {
			cb_define (y, CB_VALUE(x));
		} else {
			cb_build_constant (y, CB_VALUE(x));
		}
	}

	/* Inherit class names from parent */
	for (x = current_program->class_name_list; x; x = CB_CHAIN(x)) {
		y = cb_build_reference (cb_name(CB_VALUE(x)));
		cb_define (y, CB_VALUE(x));
	}
}

static void
clear_initial_values (void)
{
	perform_stack = NULL;
	current_statement = NULL;
	main_statement = NULL;
	qualifier = NULL;
	in_declaratives = 0;
	in_debugging = 0;
	use_global_ind = 0;
	check_duplicate = 0;
	check_pic_duplicate = 0;
	skip_statements = 0;
	start_debug = 0;
	save_debug = 0;
	needs_field_debug = 0;
	needs_debug_item = 0;
	env_div_seen = 0;
	header_check = 0;
	next_label_id = 0;
	current_linage = 0;
	set_attr_val_on = 0;
	set_attr_val_off = 0;
	report_count = 0;
	current_storage = CB_STORAGE_WORKING;
	eval_level = 0;
	eval_inc = 0;
	eval_inc2 = 0;
	inspect_keyword = 0;
	check_unreached = 0;
	cobc_in_id = 0;
	cobc_in_procedure = 0;
	cobc_in_repository = 0;
	cobc_force_literal = 0;
	cobc_in_xml_generate_body = 0;
	cobc_in_xml_parse_body = 0;
	cobc_in_json_generate_body = 0;
	non_const_word = 0;
	suppress_data_exceptions = 0;
	same_area = 1;
	memset ((void *)eval_check, 0, sizeof(eval_check));
	memset ((void *)term_array, 0, sizeof(term_array));
	linage_file = NULL;
	current_file = NULL;
	current_cd = NULL;
	current_report = NULL;
	report_instance = NULL;
	next_label_list = NULL;
	default_rounded_mode = cb_int (COB_STORE_ROUND);
}

/*
  We must check for redefinitions of program-names and external program names
  outside of the usual reference/word_list methods as it may have to be done in
  a case-sensitive way.
*/
static void
begin_scope_of_program_name (struct cb_program *program)
{
	const char	*prog_name = program->program_name;
	const char	*prog_id = program->orig_program_id;
	const char	*elt_name;
	const char	*elt_id;
	cb_tree		l;

	/* Error if a program with the same name has been defined. */
	for (l = defined_prog_list; l; l = CB_CHAIN (l)) {
		elt_name = ((struct cb_program *) CB_VALUE (l))->program_name;
		elt_id = ((struct cb_program *) CB_VALUE (l))->orig_program_id;
		if (cb_fold_call && strcasecmp (prog_name, elt_name) == 0) {
			cb_error_x ((cb_tree) program,
				    _("redefinition of program name '%s'"),
				    elt_name);
		} else if (strcmp (prog_id, elt_id) == 0) {
		        cb_error_x ((cb_tree) program,
				    _("redefinition of program ID '%s'"),
				    elt_id);
			return;
		}
	}

	/* Otherwise, add the program to the list. */
	defined_prog_list = cb_list_add (defined_prog_list,
					 (cb_tree) program);
}

static void
remove_program_name (struct cb_list *l, struct cb_list *prev)
{
	if (prev == NULL) {
		defined_prog_list = l->chain;
	} else {
		prev->chain = l->chain;
	}
	cobc_parse_free (l);
}

/* Remove the program from defined_prog_list, if necessary. */
static void
end_scope_of_program_name (struct cb_program *program, const unsigned char type)
{
	struct	cb_list	*prev = NULL;
	struct	cb_list *l = (struct cb_list *) defined_prog_list;

	/* create empty entry if the program has no PROCEDURE DIVISION, error for UDF */
	if (!program->entry_list) {
		if (type == COB_MODULE_TYPE_FUNCTION) {
			cb_error (_("FUNCTION '%s' has no PROCEDURE DIVISION"), program->program_name);
		} else {
			emit_entry (program->program_id, 0, NULL, NULL);
		}
	}
	program->last_source_line = backup_source_line;

	if (program->nested_level == 0) {
		return;
	}

	/* Remove any subprograms */
	l = CB_LIST (defined_prog_list);
	while (l) {
		if (CB_PROGRAM (l->value)->nested_level > program->nested_level) {
			remove_program_name (l, prev);
		} else {
			prev = l;
		}
		if (prev && prev->chain != NULL) {
			l = CB_LIST (prev->chain);
		} else {
			l = NULL;
		}
	}

	/* Remove the specified program, if it is not COMMON */
	if (!program->flag_common) {
		l = (struct cb_list *) defined_prog_list;
		while (l) {
			if (strcmp (program->orig_program_id,
				    CB_PROGRAM (l->value)->orig_program_id)
			    == 0) {
				remove_program_name (l, prev);
				if (prev && prev->chain != NULL) {
					l = CB_LIST (prev->chain);
				} else {
					l = NULL;
				}
				break;
			} else {
				prev = l;
				if (l->chain != NULL) {
					l = CB_LIST (l->chain);
				} else {
					l = NULL;
				}
			}
		}
	}
}

static void
setup_program_start (void)
{
	if (setup_from_identification) {
		setup_from_identification = 0;
		return;
	}
	current_section = NULL;
	current_paragraph = NULL;

	if (depth != 0 && first_nested_program) {
		check_headers_present (COBC_HD_PROCEDURE_DIVISION, 0, 0, 0);
	}
	first_nested_program = 1;
}

static int
setup_program (cb_tree id, cb_tree as_literal, const unsigned char type)
{
	const char	*external_name = NULL;

	setup_program_start ();

	/* finish last program/function */
	if (!first_prog) {
		if (!current_program->flag_validated) {
			current_program->flag_validated = 1;
			cb_validate_program_body (current_program);
		}

		clear_initial_values ();
		current_program = cb_build_program (current_program, depth);
		if (depth) {
			build_words_for_nested_programs();
		}
		cb_set_intr_when_compiled ();
		cb_build_registers ();
		cb_add_external_defined_registers ();
	} else {
		first_prog = 0;
	}

	/* set internal name */
	if (CB_LITERAL_P (id)) {
		current_program->program_name = (char *)CB_LITERAL (id)->data;
	} else {
		current_program->program_name = CB_NAME (id);
	}
	stack_progid[depth] = current_program->program_name;
	current_program->prog_type = type;

	if (depth != 0 && type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("functions may not be defined within a program/function"));
	}

	if (increment_depth ()) {
		return 1;
	}

	/* set external name if specified */
	if (as_literal) {
		external_name = (const char *)CB_LITERAL (as_literal)->data;
	} else {
		external_name = current_program->program_name;
	}

	/* build encoded external PROGRAM-ID */
	current_program->program_id
		= cb_build_program_id (external_name, type == COB_MODULE_TYPE_FUNCTION);

	if (type == COB_MODULE_TYPE_PROGRAM) {
		if (!main_flag_set) {
			main_flag_set = 1;
			current_program->flag_main = !!cobc_flag_main;
		}
	} else { /* COB_MODULE_TYPE_FUNCTION */
		current_program->flag_recursive = 1;
	}

	if (CB_REFERENCE_P (id)) {
		cb_define (id, CB_TREE (current_program));
	}

	begin_scope_of_program_name (current_program);

	return 0;
}

static void
decrement_depth (const char *name, const unsigned char type)
{
	int	d;

	if (depth) {
		depth--;
	}

	if (!strcmp (stack_progid[depth], name)) {
		return;
	}

	if (type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("END FUNCTION '%s' is different from FUNCTION-ID '%s'"),
			  name, stack_progid[depth]);
		return;
	}

	/* Set depth to that of whatever program we just ended, if it exists. */
	for (d = depth; d >= 0; --d) {
		if (!strcmp (stack_progid[d], name)) {
			depth = d;
			return;
		}
	}

	if (depth != d) {
		cb_error (_("END PROGRAM '%s' is different from PROGRAM-ID '%s'"),
			  name, stack_progid[depth]);
	}
}

static void
clean_up_program (cb_tree name, const unsigned char type)
{
	char		*s;

	end_scope_of_program_name (current_program, type);

	if (name) {
		if (CB_LITERAL_P (name)) {
			s = (char *)(CB_LITERAL (name)->data);
		} else {
			s = (char *)(CB_NAME (name));
		}

		decrement_depth (s, type);
	}

	current_section = NULL;
	current_paragraph = NULL;
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
}

static const char *
get_literal_or_word_name (const cb_tree x)
{
	if (CB_LITERAL_P (x)) {
		return (const char *) CB_LITERAL (x)->data;
	} else { /* CB_REFERENCE_P (x) */
		return (const char *) CB_NAME (x);
	}
}

/* verify and set currency symbol used in picture (compile time) and - if no currency
   string is explicitly set (which is currently not implemented) - as currency string
   (run time for display and [de-]editing)*/
static void
set_currency_picture_symbol (const cb_tree x)
{
	unsigned char	*s		= CB_LITERAL (x)->data;

	if (CB_LITERAL (x)->size != 1) {
		cb_error_x (x, _("currency symbol must be one character long"));
		return;
	}
	switch (*s) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'N':
	case 'P':
	case 'R':
	case 'S':
	case 'V':
	case 'X':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'n':
	case 'p':
	case 'r':
	case 's':
	case 'v':
	case 'x':
	case 'z':
	case '+':
	case '-':
	case ',':
	case '.':
	case '*':
	case '/':
	case ';':
	case '(':
	case ')':
	case '=':
	case '\'':
	case '"':
	case ' ':
#if 0 /* note: MicroFocus also dissalows L (VAX) and G (OSVS) */
	case 'L':
	case 'G':
	case 'l':
	case 'g':
#endif
		cb_error_x (x, _("invalid character '%c' in currency symbol"), s[0]);
		return;
	default:
		break;
	}
	current_program->currency_symbol = s[0];
}

/* Return 1 if the prototype name is the same as the current function's. */
static int
check_prototype_redefines_current_element (const cb_tree prototype_name)
{
	const char	*name = get_literal_or_word_name (prototype_name);

	if (strcasecmp (name, current_program->program_name) == 0) {
		cb_warning_x (COBC_WARN_FILLER, prototype_name,
			_("prototype has same name as current function and will be ignored"));
		return 1;
	}

	return 0;
}

/* Returns 1 if the prototype has been duplicated. */
static int
check_for_duplicate_prototype (const cb_tree prototype_name,
			       const cb_tree prototype)
{
	cb_tree	dup;

	if (CB_WORD_COUNT (prototype_name) > 0) {
		/* Make sure the duplicate is a prototype */
		dup = cb_ref (prototype_name);
		if (!CB_PROTOTYPE_P (dup)) {
			redefinition_error (prototype_name);
			return 1;
		}

		/* Check the duplicate prototypes match */
		if (strcmp (CB_PROTOTYPE (prototype)->ext_name,
			    CB_PROTOTYPE (dup)->ext_name)
		    || CB_PROTOTYPE (prototype)->type != CB_PROTOTYPE (dup)->type) {
			cb_error_x (prototype_name,
				    _("duplicate REPOSITORY entries for '%s' do not match"),
				    get_literal_or_word_name (prototype_name));
		} else {
			cb_warning_x (COBC_WARN_FILLER, prototype_name,
				      _("duplicate REPOSITORY entry for '%s'"),
				      get_literal_or_word_name (prototype_name));
		}
		return 1;
	}

	return 0;
}

static void
setup_prototype (cb_tree prototype_name, cb_tree ext_name,
		  const int type, const int is_current_element)
{
	cb_tree	prototype;
	int	name_redefinition_allowed;

	if (!is_current_element
	    && check_prototype_redefines_current_element (prototype_name)) {
		return;
	}

	prototype = cb_build_prototype (prototype_name, ext_name, type);

	if (!is_current_element
	    && check_for_duplicate_prototype (prototype_name, prototype)) {
		return;
	}

	name_redefinition_allowed = type == COB_MODULE_TYPE_PROGRAM
		&& is_current_element && cb_program_name_redefinition;
	if (!name_redefinition_allowed) {
		if (CB_LITERAL_P (prototype_name)) {
			cb_define (cb_build_reference ((const char *)CB_LITERAL (prototype_name)->data), prototype);
		} else {
			cb_define (prototype_name, prototype);
		}

		if (type == COB_MODULE_TYPE_PROGRAM) {
			current_program->program_spec_list =
				cb_list_add (current_program->program_spec_list, prototype);
		} else { /* COB_MODULE_TYPE_FUNCTION */
			current_program->user_spec_list =
				cb_list_add (current_program->user_spec_list, prototype);
		}
	}
}

static void
error_if_record_delimiter_incompatible (const int organization,
					const char *organization_name)
{
	int	is_compatible;

	if (!current_file->flag_delimiter) {
		return;
	}

	if (organization == COB_ORG_LINE_SEQUENTIAL) {
		is_compatible = current_file->organization == COB_ORG_SEQUENTIAL
			|| current_file->organization == COB_ORG_LINE_SEQUENTIAL;
	} else {
		is_compatible = current_file->organization == organization;
	}

	if (!is_compatible) {
		cb_error (_("ORGANIZATION %s is incompatible with RECORD DELIMITER"),
			  organization_name);
	}
}

static void
error_if_invalid_level_for_renames (cb_tree item)
{
	int	level = CB_FIELD (cb_ref (item))->level;

	if (level == 1 || level == 66 || level == 77) {
	        cb_verify (cb_renames_uncommon_levels,
			   _("RENAMES of 01-, 66- and 77-level items"));
	} else if (level == 88) {
		cb_error (_("RENAMES may not reference a level 88"));
	}
}

static int
set_current_field (cb_tree level, cb_tree name)
{
	cb_tree	x  = cb_build_field_tree (level, name, current_field,
					  current_storage, current_file, 0);
	cobc_parse_free (level);

	if (CB_INVALID_TREE (x)) {
		return 1;
	} else {
		current_field = CB_FIELD (x);
		check_pic_duplicate = 0;
	}

	return 0;
}

static void
check_not_both (const cob_flags_t flag1, const cob_flags_t flag2,
		const char *flag1_name, const char *flag2_name,
		const cob_flags_t flags, const cob_flags_t flag_to_set)
{
	if (flag_to_set == flag1 && (flags & flag2)) {
		cb_error (_("cannot specify both %s and %s"),
			  flag1_name, flag2_name);
	} else if (flag_to_set == flag2 && (flags & flag1)) {
		cb_error (_("cannot specify both %s and %s"),
			  flag1_name, flag2_name);

	}
}

static COB_INLINE COB_A_INLINE void
check_not_highlight_and_lowlight (const cob_flags_t flags,
				  const cob_flags_t flag_to_set)
{
	check_not_both (COB_SCREEN_HIGHLIGHT, COB_SCREEN_LOWLIGHT,
			"HIGHLIGHT", "LOWLIGHT", flags, flag_to_set);
}

static void
set_screen_attr (const char *clause, const cob_flags_t bitval)
{
	if (current_field->screen_flag & bitval) {
		emit_duplicate_clause_message (clause);
	} else {
		current_field->screen_flag |= bitval;
	}
}

static void
emit_conflicting_clause_message (const char *clause, const char *conflicting)
{
	if (cb_relaxed_syntax_checks) {
		cb_warning (COBC_WARN_FILLER, _("cannot specify both %s and %s; %s is ignored"),
			clause, conflicting, clause);
	} else {
		cb_error (_("cannot specify both %s and %s"),
			clause, conflicting);
	}

}

static void
set_attr_with_conflict (const char *clause, const cob_flags_t bitval,
			const char *confl_clause, const cob_flags_t confl_bit,
			const int local_check_duplicate, cob_flags_t *flags)
{
	if (local_check_duplicate && (*flags & bitval)) {
		emit_duplicate_clause_message (clause);
	} else if (*flags & confl_bit) {
		emit_conflicting_clause_message (clause, confl_clause);
	} else {
		*flags |= bitval;
	}
}

static COB_INLINE COB_A_INLINE void
set_screen_attr_with_conflict (const char *clause, const cob_flags_t bitval,
			       const char *confl_clause,
			       const cob_flags_t confl_bit)
{
	set_attr_with_conflict (clause, bitval, confl_clause, confl_bit, 1,
				&current_field->screen_flag);
}

static COB_INLINE COB_A_INLINE int
has_dispattr (const cob_flags_t attrib)
{
	return current_statement->attr_ptr
		&& current_statement->attr_ptr->dispattrs & attrib;
}

static void
attach_attrib_to_cur_stmt (void)
{
	if (!current_statement->attr_ptr) {
		current_statement->attr_ptr =
			cobc_parse_malloc (sizeof(struct cb_attr_struct));
	}
}

static COB_INLINE COB_A_INLINE void
set_dispattr (const cob_flags_t attrib)
{
	attach_attrib_to_cur_stmt ();
	current_statement->attr_ptr->dispattrs |= attrib;
}

static COB_INLINE COB_A_INLINE void
set_dispattr_with_conflict (const char *attrib_name, const cob_flags_t attrib,
			    const char *confl_name,
			    const cob_flags_t confl_attrib)
{
	attach_attrib_to_cur_stmt ();
	set_attr_with_conflict (attrib_name, attrib, confl_name, confl_attrib, 0,
				&current_statement->attr_ptr->dispattrs);
}

static void
bit_set_attr (const cb_tree on_off, const cob_flags_t attr_val)
{
	if (on_off == cb_int1) {
		set_attr_val_on |= attr_val;
	} else {
		set_attr_val_off |= attr_val;
	}
}

static void
set_field_attribs (cb_tree fgc, cb_tree bgc, cb_tree scroll,
		   cb_tree timeout, cb_tree prompt, cb_tree size_is)
{
	/* [WITH] FOREGROUND-COLOR [IS] */
	if (fgc) {
		current_statement->attr_ptr->fgc = fgc;
	}
	/* [WITH] BACKGROUND-COLOR [IS] */
	if (bgc) {
		current_statement->attr_ptr->bgc = bgc;
	}
	/* [WITH] SCROLL UP | DOWN */
	if (scroll) {
		current_statement->attr_ptr->scroll = scroll;
	}
	/* [WITH] TIME-OUT [AFTER] */
	if (timeout) {
		current_statement->attr_ptr->timeout = timeout;
	}
	/* [WITH] PROMPT CHARACTER [IS] */
	if (prompt) {
		current_statement->attr_ptr->prompt = prompt;
	}
	/* [WITH] SIZE [IS] */
	if (size_is) {
		current_statement->attr_ptr->size_is = size_is;
	}
}

static void
set_attribs (cb_tree fgc, cb_tree bgc, cb_tree scroll,
	     cb_tree timeout, cb_tree prompt, cb_tree size_is,
	     const cob_flags_t attrib)
{
	attach_attrib_to_cur_stmt ();
	set_field_attribs (fgc, bgc, scroll, timeout, prompt, size_is);

	current_statement->attr_ptr->dispattrs |= attrib;
}

static void
set_attribs_with_conflict  (cb_tree fgc, cb_tree bgc, cb_tree scroll,
			    cb_tree timeout, cb_tree prompt, cb_tree size_is,
			    const char *clause_name, const cob_flags_t attrib,
			    const char *confl_name, const cob_flags_t confl_attrib)
{
	attach_attrib_to_cur_stmt ();
	set_field_attribs (fgc, bgc, scroll, timeout, prompt, size_is);

	set_dispattr_with_conflict (clause_name, attrib, confl_name,
				    confl_attrib);
}

static cob_flags_t
zero_conflicting_flag (const cob_flags_t screen_flag, cob_flags_t parent_flag,
				const cob_flags_t flag1, const cob_flags_t flag2)
{
	if (screen_flag & flag1) {
		parent_flag &= ~flag2;
	} else if (screen_flag & flag2) {
		parent_flag &= ~flag1;
	}

	return parent_flag;
}

static cob_flags_t
zero_conflicting_flags (const cob_flags_t screen_flag, cob_flags_t parent_flag)
{
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_BLANK_LINE,
					     COB_SCREEN_BLANK_SCREEN);
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_ERASE_EOL,
					     COB_SCREEN_ERASE_EOS);
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_HIGHLIGHT,
					     COB_SCREEN_LOWLIGHT);

	return parent_flag;
}

static void
check_and_set_usage (const enum cb_usage usage)
{
	check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate);
	current_field->usage = usage;
}

static void
check_preceding_tallying_phrases (const enum tallying_phrase phrase)
{
	switch (phrase) {
	case FOR_PHRASE:
		if (previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES) {
			cb_error (_("FOR phrase cannot immediately follow ALL/LEADING/TRAILING"));
		} else if (previous_tallying_phrase == FOR_PHRASE) {
			cb_error (_("missing CHARACTERS/ALL/LEADING/TRAILING phrase after FOR phrase"));
		}
		break;

	case CHARACTERS_PHRASE:
	case ALL_LEADING_TRAILING_PHRASES:
		if (previous_tallying_phrase == NO_PHRASE) {
			cb_error (_("missing FOR phrase before CHARACTERS/ALL/LEADING/TRAILING phrase"));
		} else if (previous_tallying_phrase == CHARACTERS_PHRASE
			   || previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES) {
			cb_error (_("missing value between CHARACTERS/ALL/LEADING/TRAILING words"));
		}
		break;

	case VALUE_REGION_PHRASE:
		if (!(previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES
		      || previous_tallying_phrase == VALUE_REGION_PHRASE)) {
			cb_error (_("missing ALL/LEADING/TRAILING before value"));
		}
		break;

	/* LCOV_EXCL_START */
	default:
		/* This should never happen (and therefore doesn't get a translation) */
		cb_error ("unexpected tallying phrase");
		COBC_ABORT();
	/* LCOV_EXCL_STOP */
	}

	previous_tallying_phrase = phrase;
}

static int
has_relative_pos (struct cb_field const *field)
{
	return !!(field->screen_flag
		  & (COB_SCREEN_LINE_PLUS | COB_SCREEN_LINE_MINUS
		     | COB_SCREEN_COLUMN_PLUS | COB_SCREEN_COLUMN_MINUS));
}

static int
is_recursive_call (cb_tree target)
{
	const char *target_name = "";

	if (CB_LITERAL_P (target)) {
		target_name = (const char *)(CB_LITERAL(target)->data);
	} else if (CB_REFERENCE_P (target)
		   && CB_PROTOTYPE_P (cb_ref (target))) {
		target_name = CB_PROTOTYPE (cb_ref (target))->ext_name;
	}

	return !strcmp (target_name, current_program->orig_program_id);
}

static cb_tree
check_not_88_level (cb_tree x)
{
	struct cb_field	*f;

	if (x == cb_error_node) {
		return cb_error_node;
	}
	if (!CB_REF_OR_FIELD_P(x)) {
		return x;
	}

	f = CB_FIELD_PTR (x);

	if (f->level == 88) {
		cb_error (_("condition-name not allowed here: '%s'"), cb_name (x));
		/* invalidate field to prevent same error in typeck.c (validate_one) */
		/* FIXME: If we really need the additional check here then we missed
		          a call to validate_one() somewhere */
		return cb_error_node;
	} else {
		return x;
	}
}

static int
is_screen_field (cb_tree x)
{
	if (CB_FIELD_P (x)) {
		return (CB_FIELD (x))->storage == CB_STORAGE_SCREEN;
	} else if (CB_REFERENCE_P (x)) {
		return is_screen_field (cb_ref (x));
	} else {
		return 0;
	}
}

static void
error_if_no_advancing_in_screen_display (cb_tree advancing)
{
	if (advancing != cb_int1) {
		cb_error (_("cannot specify NO ADVANCING in screen DISPLAY"));
	}
}

static cb_tree
get_default_display_device (void)
{
	if (current_program->flag_console_is_crt
	    || cb_console_is_crt) {
		return cb_null;
	} else {
		return cb_int0;
	}
}

static COB_INLINE COB_A_INLINE int
contains_one_screen_field (struct cb_list *x_list)
{
	return (cb_tree) x_list != cb_null
		&& cb_list_length ((cb_tree) x_list) == 1
		&& is_screen_field (x_list->value);
}

static int
contains_only_screen_fields (struct cb_list *x_list)
{
	if ((cb_tree) x_list == cb_null) {
		return 0;
	}

	for (; x_list; x_list = (struct cb_list *) x_list->chain) {
		if (!is_screen_field (x_list->value)) {
			return 0;
		}
	}

	return 1;
}

static int
contains_fields_and_screens (struct cb_list *x_list)
{
	int	field_seen = 0;
	int	screen_seen = 0;

	if ((cb_tree) x_list == cb_null) {
		return 0;
	}

	for (; x_list; x_list = (struct cb_list *) x_list->chain) {
		if (is_screen_field (x_list->value)) {
			screen_seen = 1;
		} else {
			field_seen = 1;
		}
	}

	return screen_seen && field_seen;
}

static enum cb_display_type
deduce_display_type (cb_tree x_list, cb_tree local_upon_value, cb_tree local_line_column,
		     struct cb_attr_struct * const attr_ptr)
{
	int	using_default_device_which_is_crt =
		local_upon_value == NULL && get_default_display_device () == cb_null;

	/* TODO: Separate CGI DISPLAYs here */
	if (contains_only_screen_fields ((struct cb_list *) x_list)) {
		if (!contains_one_screen_field ((struct cb_list *) x_list)
		    || attr_ptr) {
			cb_verify_x (x_list, cb_accept_display_extensions,
				     _("non-standard DISPLAY"));
		}

		if (local_upon_value != NULL && local_upon_value != cb_null) {
			cb_error_x (x_list, _("screens may only be displayed on CRT"));
		}

		return SCREEN_DISPLAY;
	} else if (contains_fields_and_screens ((struct cb_list *) x_list)) {
		cb_error_x (x_list, _("cannot mix screens and fields in the same DISPLAY statement"));
		return MIXED_DISPLAY;
	} else if (local_line_column || attr_ptr) {
		if (local_upon_value != NULL && local_upon_value != cb_null) {
			cb_error_x (x_list, _("screen clauses may only be used for DISPLAY on CRT"));
		}

		cb_verify_x (x_list, cb_accept_display_extensions,
			     _("non-standard DISPLAY"));

		return FIELD_ON_SCREEN_DISPLAY;
	} else if (local_upon_value == cb_null || using_default_device_which_is_crt) {
		/* This is the only format permitted by the standard */
		return FIELD_ON_SCREEN_DISPLAY;
	} else if (display_type == FIELD_ON_SCREEN_DISPLAY && local_upon_value == NULL) {
		/* This is for when fields without clauses follow fields with screen clauses */
		return FIELD_ON_SCREEN_DISPLAY;
	} else {
		return DEVICE_DISPLAY;
	}
}

static void
set_display_type (cb_tree x_list, cb_tree local_upon_value,
		  cb_tree local_line_column, struct cb_attr_struct * const attr_ptr)
{
	display_type = deduce_display_type (x_list, local_upon_value, local_line_column, attr_ptr);
}

static void
error_if_different_display_type (cb_tree x_list, cb_tree local_upon_value,
				 cb_tree local_line_column, struct cb_attr_struct * const attr_ptr)
{
	const enum cb_display_type	type =
		deduce_display_type (x_list, local_upon_value, local_line_column, attr_ptr);

	/* Avoid re-displaying the same error for mixed DISPLAYs */
	if (type == display_type || display_type == MIXED_DISPLAY) {
		return;
	}

	if (type != MIXED_DISPLAY) {
		if (type == SCREEN_DISPLAY || display_type == SCREEN_DISPLAY) {
			cb_error_x (x_list, _("cannot mix screens and fields in the same DISPLAY statement"));
		} else {
			/*
			  The only other option is that there is a mix of
			  FIELD_ON_SCREEN_DISPLAY and DEVICE_DISPLAY.
			*/
			cb_error_x (x_list, _("ambiguous DISPLAY; put items to display on device in separate DISPLAY"));
		}
	}

	display_type = MIXED_DISPLAY;
}

static void
error_if_not_usage_display_or_nonnumeric_lit (cb_tree x)
{
	const int	is_numeric_literal = CB_NUMERIC_LITERAL_P (x);
	const int	is_field_with_usage_not_display =
		CB_REFERENCE_P (x) && CB_FIELD (cb_ref (x))
		&& CB_FIELD (cb_ref (x))->usage != CB_USAGE_DISPLAY;

	if (is_numeric_literal) {
		cb_error_x (x, _("%s is not an alphanumeric literal"), CB_LITERAL (x)->data);
	} else if (is_field_with_usage_not_display) {
		cb_error_x (x, _("'%s' is not USAGE DISPLAY"), cb_name (x));
	}
}

static void
check_validate_item (cb_tree x)
{
	struct cb_field	*f;
	enum cb_class	tree_class;

	if (CB_INVALID_TREE(x) || x->tag != CB_TAG_REFERENCE) {
		return;
	}
	x = cb_ref (x);
	if (CB_INVALID_TREE (x) || !CB_FIELD_P (x)) {
		cb_error (_("invalid target for %s"), "VALIDATE");
		return;
	}

	f = CB_FIELD (x);
	tree_class = CB_TREE_CLASS(f);
	if (is_screen_field(x)) {
		cb_error (_("SCREEN item cannot be used here"));
	} else if (f->level == 66) {
		cb_error (_("level %02d item '%s' may not be used here"), 66, cb_name (x));
	} else if (f->flag_any_length) {
		cb_error (_("ANY LENGTH item not allowed here"));
	} else if (tree_class == CB_CLASS_INDEX
		|| tree_class == CB_CLASS_OBJECT
		|| tree_class == CB_CLASS_POINTER) {
		cb_error (_("item '%s' has wrong class for VALIDATE"), cb_name (x));
	}
}

static void
error_if_following_every_clause (void)
{
	if (ml_suppress_list
	    && CB_ML_SUPPRESS (CB_VALUE (ml_suppress_list))->target == CB_ML_SUPPRESS_TYPE) {
		cb_error (_("WHEN clause must follow EVERY clause"));
	}
}

static void
prepend_to_ml_suppress_list (cb_tree suppress_entry)
{
	cb_tree	new_list_head = CB_LIST_INIT (suppress_entry);
	cb_list_append (new_list_head, ml_suppress_list);
	ml_suppress_list = new_list_head;
}

static void
add_identifier_to_ml_suppress_conds (cb_tree identifier)
{
	cb_tree suppress_id = cb_build_ml_suppress_clause ();
	CB_ML_SUPPRESS (suppress_id)->target = CB_ML_SUPPRESS_IDENTIFIER;
	CB_ML_SUPPRESS (suppress_id)->identifier = identifier;
	prepend_to_ml_suppress_list (suppress_id);
}

static void
add_when_to_ml_suppress_conds (cb_tree when_list)
{
	struct cb_ml_suppress_clause	*last_suppress_clause;
	cb_tree	suppress_all;

	/*
	  If the preceding clause in SUPPRESS was an identifier, the WHEN
	  belongs to the identifier. If EVERY was preceding, the WHEN belongs to
	  the EVERY. Otherwise, the WHEN acts on the entire record.
	*/
	if (ml_suppress_list) {
		last_suppress_clause = CB_ML_SUPPRESS (CB_VALUE (ml_suppress_list));
		if ((last_suppress_clause->target == CB_ML_SUPPRESS_IDENTIFIER
		     || last_suppress_clause->target == CB_ML_SUPPRESS_TYPE)
		    && !last_suppress_clause->when_list) {
			last_suppress_clause->when_list = when_list;
			return;
		}
	}

	suppress_all = cb_build_ml_suppress_clause ();
	CB_ML_SUPPRESS (suppress_all)->when_list = when_list;
	prepend_to_ml_suppress_list (suppress_all);
}

static void
add_type_to_ml_suppress_conds (enum cb_ml_suppress_category category,
			       enum cb_ml_type ml_type)
{
	cb_tree	suppress_type = cb_build_ml_suppress_clause ();
	CB_ML_SUPPRESS (suppress_type)->target = CB_ML_SUPPRESS_TYPE;
	CB_ML_SUPPRESS (suppress_type)->category = category;
	CB_ML_SUPPRESS (suppress_type)->ml_type = ml_type;
	prepend_to_ml_suppress_list (suppress_type);
}


#line 2016 "parser.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TOKEN_EOF = 0,
    THREEDIMENSIONAL = 258,
    ABSENT = 259,
    ACCEPT = 260,
    ACCESS = 261,
    ACTIVEX = 262,
    ACTION = 263,
    ADD = 264,
    ADDRESS = 265,
    ADJUSTABLE_COLUMNS = 266,
    ADVANCING = 267,
    AFTER = 268,
    ALIGNMENT = 269,
    ALL = 270,
    ALLOCATE = 271,
    ALLOWING = 272,
    ALPHABET = 273,
    ALPHABETIC = 274,
    ALPHABETIC_LOWER = 275,
    ALPHABETIC_UPPER = 276,
    ALPHANUMERIC = 277,
    ALPHANUMERIC_EDITED = 278,
    ALSO = 279,
    ALTER = 280,
    ALTERNATE = 281,
    AND = 282,
    ANY = 283,
    APPLY = 284,
    ARE = 285,
    AREA = 286,
    AREAS = 287,
    ARGUMENT_NUMBER = 288,
    ARGUMENT_VALUE = 289,
    ARITHMETIC = 290,
    AS = 291,
    ASCENDING = 292,
    ASCII = 293,
    ASSIGN = 294,
    AT = 295,
    ATTRIBUTE = 296,
    ATTRIBUTES = 297,
    AUTO = 298,
    AUTO_DECIMAL = 299,
    AUTO_SPIN = 300,
    AUTOMATIC = 301,
    AWAY_FROM_ZERO = 302,
    BACKGROUND_COLOR = 303,
    BACKGROUND_HIGH = 304,
    BACKGROUND_LOW = 305,
    BACKGROUND_STANDARD = 306,
    BAR = 307,
    BASED = 308,
    BEFORE = 309,
    BELL = 310,
    BINARY = 311,
    BINARY_C_LONG = 312,
    BINARY_CHAR = 313,
    BINARY_DOUBLE = 314,
    BINARY_LONG = 315,
    BINARY_SEQUENTIAL = 316,
    BINARY_SHORT = 317,
    BIT = 318,
    BITMAP = 319,
    BITMAP_END = 320,
    BITMAP_HANDLE = 321,
    BITMAP_NUMBER = 322,
    BITMAP_START = 323,
    BITMAP_TIMER = 324,
    BITMAP_TRAILING = 325,
    BITMAP_TRANSPARENT_COLOR = 326,
    BITMAP_WIDTH = 327,
    BLANK = 328,
    BLINK = 329,
    BLOCK = 330,
    BOTTOM = 331,
    BOX = 332,
    BOXED = 333,
    BULK_ADDITION = 334,
    BUSY = 335,
    BUTTONS = 336,
    BY = 337,
    BYTE_LENGTH = 338,
    CALENDAR_FONT = 339,
    CALL = 340,
    CANCEL = 341,
    CANCEL_BUTTON = 342,
    CAPACITY = 343,
    CARD_PUNCH = 344,
    CARD_READER = 345,
    CASSETTE = 346,
    CCOL = 347,
    CD = 348,
    CELL = 349,
    CELL_COLOR = 350,
    CELL_DATA = 351,
    CELL_FONT = 352,
    CELL_PROTECTION = 353,
    CENTER = 354,
    CENTERED = 355,
    CENTERED_HEADINGS = 356,
    CENTURY_DATE = 357,
    CF = 358,
    CH = 359,
    CHAINING = 360,
    CHARACTER = 361,
    CHARACTERS = 362,
    CHECK_BOX = 363,
    CLASS = 364,
    CLASSIFICATION = 365,
    CLASS_NAME = 366,
    CLEAR_SELECTION = 367,
    CLINE = 368,
    CLINES = 369,
    CLOSE = 370,
    COBOL = 371,
    CODE = 372,
    CODE_SET = 373,
    COLLATING = 374,
    COL = 375,
    COLOR = 376,
    COLORS = 377,
    COLS = 378,
    COLUMN = 379,
    COLUMN_COLOR = 380,
    COLUMN_DIVIDERS = 381,
    COLUMN_FONT = 382,
    COLUMN_HEADINGS = 383,
    COLUMN_PROTECTION = 384,
    COLUMNS = 385,
    COMBO_BOX = 386,
    COMMA = 387,
    COMMAND_LINE = 388,
    COMMA_DELIM = 389,
    COMMIT = 390,
    COMMON = 391,
    COMMUNICATION = 392,
    COMP = 393,
    COMPUTE = 394,
    COMP_0 = 395,
    COMP_1 = 396,
    COMP_2 = 397,
    COMP_3 = 398,
    COMP_4 = 399,
    COMP_5 = 400,
    COMP_6 = 401,
    COMP_N = 402,
    COMP_X = 403,
    CONCATENATE_FUNC = 404,
    CONDITION = 405,
    CONFIGURATION = 406,
    CONSTANT = 407,
    CONTAINS = 408,
    CONTENT = 409,
    CONTENT_LENGTH_FUNC = 410,
    CONTENT_OF_FUNC = 411,
    CONTINUE = 412,
    CONTROL = 413,
    CONTROLS = 414,
    CONVERSION = 415,
    CONVERTING = 416,
    COPY = 417,
    COPY_SELECTION = 418,
    CORRESPONDING = 419,
    COUNT = 420,
    CRT = 421,
    CRT_UNDER = 422,
    CSIZE = 423,
    CURRENCY = 424,
    CURRENT_DATE_FUNC = 425,
    CURSOR = 426,
    CURSOR_COL = 427,
    CURSOR_COLOR = 428,
    CURSOR_FRAME_WIDTH = 429,
    CURSOR_ROW = 430,
    CURSOR_X = 431,
    CURSOR_Y = 432,
    CUSTOM_PRINT_TEMPLATE = 433,
    CYCLE = 434,
    DASHED = 435,
    DATA = 436,
    DATA_COLUMNS = 437,
    DATA_TYPES = 438,
    DATE = 439,
    DATE_ENTRY = 440,
    DAY = 441,
    DAY_OF_WEEK = 442,
    DE = 443,
    DEBUGGING = 444,
    DECIMAL_POINT = 445,
    DECLARATIVES = 446,
    DEFAULT = 447,
    DEFAULT_BUTTON = 448,
    DEFAULT_FONT = 449,
    DELETE = 450,
    DELIMITED = 451,
    DELIMITER = 452,
    DEPENDING = 453,
    DESCENDING = 454,
    DESTINATION = 455,
    DESTROY = 456,
    DETAIL = 457,
    DISABLE = 458,
    DISC = 459,
    DISK = 460,
    DISPLAY = 461,
    DISPLAY_COLUMNS = 462,
    DISPLAY_FORMAT = 463,
    DISPLAY_OF_FUNC = 464,
    DIVIDE = 465,
    DIVIDERS = 466,
    DIVIDER_COLOR = 467,
    DIVISION = 468,
    DOTDASH = 469,
    DOTTED = 470,
    DRAG_COLOR = 471,
    DROP_DOWN = 472,
    DROP_LIST = 473,
    DOWN = 474,
    DUPLICATES = 475,
    DYNAMIC = 476,
    EBCDIC = 477,
    EC = 478,
    ECHO = 479,
    EGI = 480,
    EIGHTY_EIGHT = 481,
    ENABLE = 482,
    ELEMENT = 483,
    ELSE = 484,
    EMI = 485,
    ENCRYPTION = 486,
    ENCODING = 487,
    END = 488,
    END_ACCEPT = 489,
    END_ADD = 490,
    END_CALL = 491,
    END_COMPUTE = 492,
    END_COLOR = 493,
    END_DELETE = 494,
    END_DISPLAY = 495,
    END_DIVIDE = 496,
    END_EVALUATE = 497,
    END_FUNCTION = 498,
    END_IF = 499,
    END_JSON = 500,
    END_MODIFY = 501,
    END_MULTIPLY = 502,
    END_PERFORM = 503,
    END_PROGRAM = 504,
    END_READ = 505,
    END_RECEIVE = 506,
    END_RETURN = 507,
    END_REWRITE = 508,
    END_SEARCH = 509,
    END_START = 510,
    END_STRING = 511,
    END_SUBTRACT = 512,
    END_UNSTRING = 513,
    END_WRITE = 514,
    END_XML = 515,
    ENGRAVED = 516,
    ENSURE_VISIBLE = 517,
    ENTRY = 518,
    ENTRY_CONVENTION = 519,
    ENTRY_FIELD = 520,
    ENTRY_REASON = 521,
    ENVIRONMENT = 522,
    ENVIRONMENT_NAME = 523,
    ENVIRONMENT_VALUE = 524,
    EOL = 525,
    EOP = 526,
    EOS = 527,
    EQUAL = 528,
    ERASE = 529,
    ERROR = 530,
    ESCAPE = 531,
    ESCAPE_BUTTON = 532,
    ESI = 533,
    EVALUATE = 534,
    EVENT = 535,
    EVENT_LIST = 536,
    EVENT_STATUS = 537,
    EVERY = 538,
    EXCEPTION = 539,
    EXCEPTION_CONDITION = 540,
    EXCEPTION_VALUE = 541,
    EXPAND = 542,
    EXCLUSIVE = 543,
    EXIT = 544,
    EXPONENTIATION = 545,
    EXTEND = 546,
    EXTERNAL = 547,
    EXTERNAL_FORM = 548,
    F = 549,
    FD = 550,
    FH__FCD = 551,
    FH__KEYDEF = 552,
    FILE_CONTROL = 553,
    FILE_ID = 554,
    FILE_NAME = 555,
    FILE_POS = 556,
    FILL_COLOR = 557,
    FILL_COLOR2 = 558,
    FILL_PERCENT = 559,
    FILLER = 560,
    FINAL = 561,
    FINISH_REASON = 562,
    FIRST = 563,
    FIXED = 564,
    FIXED_FONT = 565,
    FIXED_WIDTH = 566,
    FLAT = 567,
    FLAT_BUTTONS = 568,
    FLOAT_BINARY_128 = 569,
    FLOAT_BINARY_32 = 570,
    FLOAT_BINARY_64 = 571,
    FLOAT_DECIMAL_16 = 572,
    FLOAT_DECIMAL_34 = 573,
    FLOAT_DECIMAL_7 = 574,
    FLOAT_EXTENDED = 575,
    FLOAT_LONG = 576,
    FLOAT_SHORT = 577,
    FLOATING = 578,
    FONT = 579,
    FOOTING = 580,
    FOR = 581,
    FOREGROUND_COLOR = 582,
    FOREVER = 583,
    FORMATTED_DATE_FUNC = 584,
    FORMATTED_DATETIME_FUNC = 585,
    FORMATTED_TIME_FUNC = 586,
    FRAME = 587,
    FRAMED = 588,
    FREE = 589,
    FROM = 590,
    FROM_CRT = 591,
    FULL = 592,
    FULL_HEIGHT = 593,
    FUNCTION = 594,
    FUNCTION_ID = 595,
    FUNCTION_NAME = 596,
    GENERATE = 597,
    GIVING = 598,
    GLOBAL = 599,
    GO = 600,
    GO_BACK = 601,
    GO_FORWARD = 602,
    GO_HOME = 603,
    GO_SEARCH = 604,
    GOBACK = 605,
    GRAPHICAL = 606,
    GREATER = 607,
    GREATER_OR_EQUAL = 608,
    GRID = 609,
    GROUP = 610,
    GROUP_VALUE = 611,
    HANDLE = 612,
    HAS_CHILDREN = 613,
    HEADING = 614,
    HEADING_COLOR = 615,
    HEADING_DIVIDER_COLOR = 616,
    HEADING_FONT = 617,
    HEAVY = 618,
    HEIGHT_IN_CELLS = 619,
    HIDDEN_DATA = 620,
    HIGHLIGHT = 621,
    HIGH_COLOR = 622,
    HIGH_VALUE = 623,
    HOT_TRACK = 624,
    HSCROLL = 625,
    HSCROLL_POS = 626,
    ICON = 627,
    ID = 628,
    IDENTIFIED = 629,
    IDENTIFICATION = 630,
    IF = 631,
    IGNORE = 632,
    IGNORING = 633,
    IN = 634,
    INDEPENDENT = 635,
    INDEX = 636,
    INDEXED = 637,
    INDICATE = 638,
    INITIALIZE = 639,
    INITIALIZED = 640,
    INITIATE = 641,
    INPUT = 642,
    INPUT_OUTPUT = 643,
    INQUIRE = 644,
    INSERTION_INDEX = 645,
    INSERT_ROWS = 646,
    INSPECT = 647,
    INTERMEDIATE = 648,
    INTO = 649,
    INTRINSIC = 650,
    INVALID = 651,
    INVALID_KEY = 652,
    IS = 653,
    ITEM = 654,
    ITEM_TEXT = 655,
    ITEM_TO_ADD = 656,
    ITEM_TO_DELETE = 657,
    ITEM_TO_EMPTY = 658,
    ITEM_VALUE = 659,
    I_O = 660,
    I_O_CONTROL = 661,
    JSON = 662,
    JUSTIFIED = 663,
    KEPT = 664,
    KEY = 665,
    KEYBOARD = 666,
    LABEL = 667,
    LABEL_OFFSET = 668,
    LARGE_FONT = 669,
    LARGE_OFFSET = 670,
    LAST = 671,
    LAST_ROW = 672,
    LAYOUT_DATA = 673,
    LAYOUT_MANAGER = 674,
    LEADING = 675,
    LEADING_SHIFT = 676,
    LEFT = 677,
    LEFTLINE = 678,
    LEFT_TEXT = 679,
    LENGTH = 680,
    LENGTH_FUNC = 681,
    LESS = 682,
    LESS_OR_EQUAL = 683,
    LEVEL_NUMBER = 684,
    LIMIT = 685,
    LIMITS = 686,
    LINAGE = 687,
    LINAGE_COUNTER = 688,
    LINE = 689,
    LINE_COUNTER = 690,
    LINE_LIMIT = 691,
    LINE_SEQUENTIAL = 692,
    LINES = 693,
    LINES_AT_ROOT = 694,
    LINKAGE = 695,
    LIST_BOX = 696,
    LITERAL = 697,
    LM_RESIZE = 698,
    LOC = 699,
    LOCALE = 700,
    LOCALE_DATE_FUNC = 701,
    LOCALE_TIME_FUNC = 702,
    LOCALE_TIME_FROM_FUNC = 703,
    LOCAL_STORAGE = 704,
    LOCK = 705,
    LONG_DATE = 706,
    LOWER = 707,
    LOWERED = 708,
    LOWER_CASE_FUNC = 709,
    LOWLIGHT = 710,
    LOW_COLOR = 711,
    LOW_VALUE = 712,
    MAGNETIC_TAPE = 713,
    MANUAL = 714,
    MASS_UPDATE = 715,
    MAX_LINES = 716,
    MAX_PROGRESS = 717,
    MAX_TEXT = 718,
    MAX_VAL = 719,
    MEMORY = 720,
    MEDIUM_FONT = 721,
    MENU = 722,
    MERGE = 723,
    MESSAGE = 724,
    MINUS = 725,
    MIN_VAL = 726,
    MNEMONIC_NAME = 727,
    MODE = 728,
    MODIFY = 729,
    MODULES = 730,
    MOVE = 731,
    MULTILINE = 732,
    MULTIPLE = 733,
    MULTIPLY = 734,
    NAME = 735,
    NAMESPACE = 736,
    NAMESPACE_PREFIX = 737,
    NATIONAL = 738,
    NATIONAL_EDITED = 739,
    NATIONAL_OF_FUNC = 740,
    NATIVE = 741,
    NAVIGATE_URL = 742,
    NEAREST_AWAY_FROM_ZERO = 743,
    NEAREST_EVEN = 744,
    NEAREST_TOWARD_ZERO = 745,
    NEGATIVE = 746,
    NESTED = 747,
    NEW = 748,
    NEXT = 749,
    NEXT_ITEM = 750,
    NEXT_GROUP = 751,
    NEXT_PAGE = 752,
    NO = 753,
    NO_ADVANCING = 754,
    NO_AUTOSEL = 755,
    NO_AUTO_DEFAULT = 756,
    NO_BOX = 757,
    NO_DATA = 758,
    NO_DIVIDERS = 759,
    NO_ECHO = 760,
    NO_F4 = 761,
    NO_FOCUS = 762,
    NO_GROUP_TAB = 763,
    NO_KEY_LETTER = 764,
    NO_SEARCH = 765,
    NO_UPDOWN = 766,
    NONNUMERIC = 767,
    NORMAL = 768,
    NOT = 769,
    NOTAB = 770,
    NOTHING = 771,
    NOTIFY = 772,
    NOTIFY_CHANGE = 773,
    NOTIFY_DBLCLICK = 774,
    NOTIFY_SELCHANGE = 775,
    NOT_END = 776,
    NOT_EOP = 777,
    NOT_ESCAPE = 778,
    NOT_EQUAL = 779,
    NOT_EXCEPTION = 780,
    NOT_INVALID_KEY = 781,
    NOT_OVERFLOW = 782,
    NOT_SIZE_ERROR = 783,
    NUM_COL_HEADINGS = 784,
    NUM_ROWS = 785,
    NUMBER = 786,
    NUMBERS = 787,
    NUMERIC = 788,
    NUMERIC_EDITED = 789,
    NUMVALC_FUNC = 790,
    OBJECT = 791,
    OBJECT_COMPUTER = 792,
    OCCURS = 793,
    OF = 794,
    OFF = 795,
    OK_BUTTON = 796,
    OMITTED = 797,
    ON = 798,
    ONLY = 799,
    OPEN = 800,
    OPTIONAL = 801,
    OPTIONS = 802,
    OR = 803,
    ORDER = 804,
    ORGANIZATION = 805,
    OTHER = 806,
    OTHERS = 807,
    OUTPUT = 808,
    OVERLAP_LEFT = 809,
    OVERLAP_TOP = 810,
    OVERLINE = 811,
    PACKED_DECIMAL = 812,
    PADDING = 813,
    PAGE = 814,
    PAGE_COUNTER = 815,
    PAGE_SETUP = 816,
    PAGED = 817,
    PARAGRAPH = 818,
    PARENT = 819,
    PARSE = 820,
    PASSWORD = 821,
    PERFORM = 822,
    PERMANENT = 823,
    PH = 824,
    PF = 825,
    PHYSICAL = 826,
    PICTURE = 827,
    PICTURE_SYMBOL = 828,
    PIXEL = 829,
    PLACEMENT = 830,
    PLUS = 831,
    POINTER = 832,
    POP_UP = 833,
    POS = 834,
    POSITION = 835,
    POSITION_SHIFT = 836,
    POSITIVE = 837,
    PRESENT = 838,
    PREVIOUS = 839,
    PRINT = 840,
    PRINT_NO_PROMPT = 841,
    PRINT_PREVIEW = 842,
    PRINTER = 843,
    PRINTER_1 = 844,
    PRINTING = 845,
    PRIORITY = 846,
    PROCEDURE = 847,
    PROCEDURES = 848,
    PROCEED = 849,
    PROCESSING = 850,
    PROGRAM = 851,
    PROGRAM_ID = 852,
    PROGRAM_NAME = 853,
    PROGRAM_POINTER = 854,
    PROGRESS = 855,
    PROHIBITED = 856,
    PROMPT = 857,
    PROPERTIES = 858,
    PROPERTY = 859,
    PROTECTED = 860,
    PURGE = 861,
    PUSH_BUTTON = 862,
    QUERY_INDEX = 863,
    QUEUE = 864,
    QUOTE = 865,
    RADIO_BUTTON = 866,
    RAISE = 867,
    RAISED = 868,
    RANDOM = 869,
    RD = 870,
    READ = 871,
    READERS = 872,
    READ_ONLY = 873,
    READY_TRACE = 874,
    RECEIVE = 875,
    RECORD = 876,
    RECORD_DATA = 877,
    RECORD_TO_ADD = 878,
    RECORD_TO_DELETE = 879,
    RECORDING = 880,
    RECORDS = 881,
    RECURSIVE = 882,
    REDEFINES = 883,
    REEL = 884,
    REFERENCE = 885,
    REFERENCES = 886,
    REFRESH = 887,
    REGION_COLOR = 888,
    RELATIVE = 889,
    RELEASE = 890,
    REMAINDER = 891,
    REMOVAL = 892,
    RENAMES = 893,
    REPLACE = 894,
    REPLACING = 895,
    REPORT = 896,
    REPORTING = 897,
    REPORTS = 898,
    REPOSITORY = 899,
    REQUIRED = 900,
    RESERVE = 901,
    RESET = 902,
    RESET_TRACE = 903,
    RESET_GRID = 904,
    RESET_LIST = 905,
    RESET_TABS = 906,
    RETRY = 907,
    RETURN = 908,
    RETURNING = 909,
    REVERSE = 910,
    REVERSE_FUNC = 911,
    REVERSE_VIDEO = 912,
    REVERSED = 913,
    REWIND = 914,
    REWRITE = 915,
    RF = 916,
    RH = 917,
    RIGHT = 918,
    RIGHT_ALIGN = 919,
    RIMMED = 920,
    ROLLBACK = 921,
    ROUNDED = 922,
    ROUNDING = 923,
    ROW_COLOR = 924,
    ROW_COLOR_PATTERN = 925,
    ROW_DIVIDERS = 926,
    ROW_FONT = 927,
    ROW_HEADINGS = 928,
    ROW_PROTECTION = 929,
    RUN = 930,
    S = 931,
    SAME = 932,
    SAVE_AS = 933,
    SAVE_AS_NO_PROMPT = 934,
    SCREEN = 935,
    SCREEN_CONTROL = 936,
    SCROLL = 937,
    SCROLL_BAR = 938,
    SD = 939,
    SEARCH = 940,
    SEARCH_OPTIONS = 941,
    SEARCH_TEXT = 942,
    SECONDS = 943,
    SECTION = 944,
    SECURE = 945,
    SEGMENT = 946,
    SEGMENT_LIMIT = 947,
    SELECT = 948,
    SELECTION_INDEX = 949,
    SELECTION_TEXT = 950,
    SELECT_ALL = 951,
    SELF_ACT = 952,
    SEMI_COLON = 953,
    SEND = 954,
    SENTENCE = 955,
    SEPARATE = 956,
    SEPARATION = 957,
    SEQUENCE = 958,
    SEQUENTIAL = 959,
    SET = 960,
    SEVENTY_EIGHT = 961,
    SHADING = 962,
    SHADOW = 963,
    SHARING = 964,
    SHORT_DATE = 965,
    SHOW_LINES = 966,
    SHOW_NONE = 967,
    SHOW_SEL_ALWAYS = 968,
    SIGN = 969,
    SIGNED = 970,
    SIGNED_INT = 971,
    SIGNED_LONG = 972,
    SIGNED_SHORT = 973,
    SIXTY_SIX = 974,
    SIZE = 975,
    SIZE_ERROR = 976,
    SMALL_FONT = 977,
    SORT = 978,
    SORT_MERGE = 979,
    SORT_ORDER = 980,
    SOURCE = 981,
    SOURCE_COMPUTER = 982,
    SPACE = 983,
    SPECIAL_NAMES = 984,
    SPINNER = 985,
    SQUARE = 986,
    STANDARD = 987,
    STANDARD_1 = 988,
    STANDARD_2 = 989,
    STANDARD_BINARY = 990,
    STANDARD_DECIMAL = 991,
    START = 992,
    START_X = 993,
    START_Y = 994,
    STATIC = 995,
    STATIC_LIST = 996,
    STATUS = 997,
    STATUS_BAR = 998,
    STATUS_TEXT = 999,
    STDCALL = 1000,
    STEP = 1001,
    STOP = 1002,
    STRING = 1003,
    STYLE = 1004,
    SUB_QUEUE_1 = 1005,
    SUB_QUEUE_2 = 1006,
    SUB_QUEUE_3 = 1007,
    SUBSTITUTE_FUNC = 1008,
    SUBSTITUTE_CASE_FUNC = 1009,
    SUBTRACT = 1010,
    SUBWINDOW = 1011,
    SUM = 1012,
    SUPPRESS = 1013,
    SUPPRESS_XML = 1014,
    SYMBOLIC = 1015,
    SYNCHRONIZED = 1016,
    SYSTEM_DEFAULT = 1017,
    SYSTEM_INFO = 1018,
    SYSTEM_OFFSET = 1019,
    TAB = 1020,
    TAB_TO_ADD = 1021,
    TAB_TO_DELETE = 1022,
    TABLE = 1023,
    TALLYING = 1024,
    TEMPORARY = 1025,
    TAPE = 1026,
    TERMINAL = 1027,
    TERMINATE = 1028,
    TERMINAL_INFO = 1029,
    TERMINATION_VALUE = 1030,
    TEST = 1031,
    TEXT = 1032,
    THAN = 1033,
    THEN = 1034,
    THREAD = 1035,
    THREADS = 1036,
    THRU = 1037,
    THUMB_POSITION = 1038,
    TILED_HEADINGS = 1039,
    TIME = 1040,
    TIME_OUT = 1041,
    TIMES = 1042,
    TITLE = 1043,
    TITLE_POSITION = 1044,
    TO = 1045,
    TOK_AMPER = 1046,
    TOK_CLOSE_PAREN = 1047,
    TOK_COLON = 1048,
    TOK_DIV = 1049,
    TOK_DOT = 1050,
    TOK_EQUAL = 1051,
    TOK_EXTERN = 1052,
    TOK_FALSE = 1053,
    TOK_FILE = 1054,
    TOK_GREATER = 1055,
    TOK_INITIAL = 1056,
    TOK_LESS = 1057,
    TOK_MINUS = 1058,
    TOK_MUL = 1059,
    TOK_NULL = 1060,
    TOK_OVERFLOW = 1061,
    TOK_OPEN_PAREN = 1062,
    TOK_PLUS = 1063,
    TOK_TRUE = 1064,
    TOP = 1065,
    TOWARD_GREATER = 1066,
    TOWARD_LESSER = 1067,
    TRADITIONAL_FONT = 1068,
    TRAILING = 1069,
    TRAILING_SHIFT = 1070,
    TRANSFORM = 1071,
    TRANSPARENT = 1072,
    TREE_VIEW = 1073,
    TRIM_FUNC = 1074,
    TRUNCATION = 1075,
    TYPE = 1076,
    U = 1077,
    UCS_4 = 1078,
    UNBOUNDED = 1079,
    UNDERLINE = 1080,
    UNFRAMED = 1081,
    UNIT = 1082,
    UNLOCK = 1083,
    UNSIGNED = 1084,
    UNSIGNED_INT = 1085,
    UNSIGNED_LONG = 1086,
    UNSIGNED_SHORT = 1087,
    UNSORTED = 1088,
    UNSTRING = 1089,
    UNTIL = 1090,
    UP = 1091,
    UPDATE = 1092,
    UPDATERS = 1093,
    UPON = 1094,
    UPON_ARGUMENT_NUMBER = 1095,
    UPON_COMMAND_LINE = 1096,
    UPON_ENVIRONMENT_NAME = 1097,
    UPON_ENVIRONMENT_VALUE = 1098,
    UPPER = 1099,
    UPPER_CASE_FUNC = 1100,
    USAGE = 1101,
    USE = 1102,
    USE_ALT = 1103,
    USE_RETURN = 1104,
    USE_TAB = 1105,
    USER = 1106,
    USER_DEFAULT = 1107,
    USER_FUNCTION_NAME = 1108,
    USING = 1109,
    UTF_8 = 1110,
    UTF_16 = 1111,
    V = 1112,
    VALIDATE = 1113,
    VALIDATING = 1114,
    VALUE = 1115,
    VALUE_FORMAT = 1116,
    VARIABLE = 1117,
    VARIANT = 1118,
    VARYING = 1119,
    VERTICAL = 1120,
    VERY_HEAVY = 1121,
    VIRTUAL_WIDTH = 1122,
    VOLATILE = 1123,
    VPADDING = 1124,
    VSCROLL = 1125,
    VSCROLL_BAR = 1126,
    VSCROLL_POS = 1127,
    VTOP = 1128,
    WAIT = 1129,
    WEB_BROWSER = 1130,
    WHEN = 1131,
    WHEN_COMPILED_FUNC = 1132,
    WHEN_XML = 1133,
    WIDTH = 1134,
    WIDTH_IN_CELLS = 1135,
    WINDOW = 1136,
    WITH = 1137,
    WORD = 1138,
    WORDS = 1139,
    WORKING_STORAGE = 1140,
    WRAP = 1141,
    WRITE = 1142,
    WRITERS = 1143,
    X = 1144,
    XML = 1145,
    XML_DECLARATION = 1146,
    Y = 1147,
    YYYYDDD = 1148,
    YYYYMMDD = 1149,
    ZERO = 1150,
    SHIFT_PREFER = 1151
  };
#endif
/* Tokens.  */
#define TOKEN_EOF 0
#define THREEDIMENSIONAL 258
#define ABSENT 259
#define ACCEPT 260
#define ACCESS 261
#define ACTIVEX 262
#define ACTION 263
#define ADD 264
#define ADDRESS 265
#define ADJUSTABLE_COLUMNS 266
#define ADVANCING 267
#define AFTER 268
#define ALIGNMENT 269
#define ALL 270
#define ALLOCATE 271
#define ALLOWING 272
#define ALPHABET 273
#define ALPHABETIC 274
#define ALPHABETIC_LOWER 275
#define ALPHABETIC_UPPER 276
#define ALPHANUMERIC 277
#define ALPHANUMERIC_EDITED 278
#define ALSO 279
#define ALTER 280
#define ALTERNATE 281
#define AND 282
#define ANY 283
#define APPLY 284
#define ARE 285
#define AREA 286
#define AREAS 287
#define ARGUMENT_NUMBER 288
#define ARGUMENT_VALUE 289
#define ARITHMETIC 290
#define AS 291
#define ASCENDING 292
#define ASCII 293
#define ASSIGN 294
#define AT 295
#define ATTRIBUTE 296
#define ATTRIBUTES 297
#define AUTO 298
#define AUTO_DECIMAL 299
#define AUTO_SPIN 300
#define AUTOMATIC 301
#define AWAY_FROM_ZERO 302
#define BACKGROUND_COLOR 303
#define BACKGROUND_HIGH 304
#define BACKGROUND_LOW 305
#define BACKGROUND_STANDARD 306
#define BAR 307
#define BASED 308
#define BEFORE 309
#define BELL 310
#define BINARY 311
#define BINARY_C_LONG 312
#define BINARY_CHAR 313
#define BINARY_DOUBLE 314
#define BINARY_LONG 315
#define BINARY_SEQUENTIAL 316
#define BINARY_SHORT 317
#define BIT 318
#define BITMAP 319
#define BITMAP_END 320
#define BITMAP_HANDLE 321
#define BITMAP_NUMBER 322
#define BITMAP_START 323
#define BITMAP_TIMER 324
#define BITMAP_TRAILING 325
#define BITMAP_TRANSPARENT_COLOR 326
#define BITMAP_WIDTH 327
#define BLANK 328
#define BLINK 329
#define BLOCK 330
#define BOTTOM 331
#define BOX 332
#define BOXED 333
#define BULK_ADDITION 334
#define BUSY 335
#define BUTTONS 336
#define BY 337
#define BYTE_LENGTH 338
#define CALENDAR_FONT 339
#define CALL 340
#define CANCEL 341
#define CANCEL_BUTTON 342
#define CAPACITY 343
#define CARD_PUNCH 344
#define CARD_READER 345
#define CASSETTE 346
#define CCOL 347
#define CD 348
#define CELL 349
#define CELL_COLOR 350
#define CELL_DATA 351
#define CELL_FONT 352
#define CELL_PROTECTION 353
#define CENTER 354
#define CENTERED 355
#define CENTERED_HEADINGS 356
#define CENTURY_DATE 357
#define CF 358
#define CH 359
#define CHAINING 360
#define CHARACTER 361
#define CHARACTERS 362
#define CHECK_BOX 363
#define CLASS 364
#define CLASSIFICATION 365
#define CLASS_NAME 366
#define CLEAR_SELECTION 367
#define CLINE 368
#define CLINES 369
#define CLOSE 370
#define COBOL 371
#define CODE 372
#define CODE_SET 373
#define COLLATING 374
#define COL 375
#define COLOR 376
#define COLORS 377
#define COLS 378
#define COLUMN 379
#define COLUMN_COLOR 380
#define COLUMN_DIVIDERS 381
#define COLUMN_FONT 382
#define COLUMN_HEADINGS 383
#define COLUMN_PROTECTION 384
#define COLUMNS 385
#define COMBO_BOX 386
#define COMMA 387
#define COMMAND_LINE 388
#define COMMA_DELIM 389
#define COMMIT 390
#define COMMON 391
#define COMMUNICATION 392
#define COMP 393
#define COMPUTE 394
#define COMP_0 395
#define COMP_1 396
#define COMP_2 397
#define COMP_3 398
#define COMP_4 399
#define COMP_5 400
#define COMP_6 401
#define COMP_N 402
#define COMP_X 403
#define CONCATENATE_FUNC 404
#define CONDITION 405
#define CONFIGURATION 406
#define CONSTANT 407
#define CONTAINS 408
#define CONTENT 409
#define CONTENT_LENGTH_FUNC 410
#define CONTENT_OF_FUNC 411
#define CONTINUE 412
#define CONTROL 413
#define CONTROLS 414
#define CONVERSION 415
#define CONVERTING 416
#define COPY 417
#define COPY_SELECTION 418
#define CORRESPONDING 419
#define COUNT 420
#define CRT 421
#define CRT_UNDER 422
#define CSIZE 423
#define CURRENCY 424
#define CURRENT_DATE_FUNC 425
#define CURSOR 426
#define CURSOR_COL 427
#define CURSOR_COLOR 428
#define CURSOR_FRAME_WIDTH 429
#define CURSOR_ROW 430
#define CURSOR_X 431
#define CURSOR_Y 432
#define CUSTOM_PRINT_TEMPLATE 433
#define CYCLE 434
#define DASHED 435
#define DATA 436
#define DATA_COLUMNS 437
#define DATA_TYPES 438
#define DATE 439
#define DATE_ENTRY 440
#define DAY 441
#define DAY_OF_WEEK 442
#define DE 443
#define DEBUGGING 444
#define DECIMAL_POINT 445
#define DECLARATIVES 446
#define DEFAULT 447
#define DEFAULT_BUTTON 448
#define DEFAULT_FONT 449
#define DELETE 450
#define DELIMITED 451
#define DELIMITER 452
#define DEPENDING 453
#define DESCENDING 454
#define DESTINATION 455
#define DESTROY 456
#define DETAIL 457
#define DISABLE 458
#define DISC 459
#define DISK 460
#define DISPLAY 461
#define DISPLAY_COLUMNS 462
#define DISPLAY_FORMAT 463
#define DISPLAY_OF_FUNC 464
#define DIVIDE 465
#define DIVIDERS 466
#define DIVIDER_COLOR 467
#define DIVISION 468
#define DOTDASH 469
#define DOTTED 470
#define DRAG_COLOR 471
#define DROP_DOWN 472
#define DROP_LIST 473
#define DOWN 474
#define DUPLICATES 475
#define DYNAMIC 476
#define EBCDIC 477
#define EC 478
#define ECHO 479
#define EGI 480
#define EIGHTY_EIGHT 481
#define ENABLE 482
#define ELEMENT 483
#define ELSE 484
#define EMI 485
#define ENCRYPTION 486
#define ENCODING 487
#define END 488
#define END_ACCEPT 489
#define END_ADD 490
#define END_CALL 491
#define END_COMPUTE 492
#define END_COLOR 493
#define END_DELETE 494
#define END_DISPLAY 495
#define END_DIVIDE 496
#define END_EVALUATE 497
#define END_FUNCTION 498
#define END_IF 499
#define END_JSON 500
#define END_MODIFY 501
#define END_MULTIPLY 502
#define END_PERFORM 503
#define END_PROGRAM 504
#define END_READ 505
#define END_RECEIVE 506
#define END_RETURN 507
#define END_REWRITE 508
#define END_SEARCH 509
#define END_START 510
#define END_STRING 511
#define END_SUBTRACT 512
#define END_UNSTRING 513
#define END_WRITE 514
#define END_XML 515
#define ENGRAVED 516
#define ENSURE_VISIBLE 517
#define ENTRY 518
#define ENTRY_CONVENTION 519
#define ENTRY_FIELD 520
#define ENTRY_REASON 521
#define ENVIRONMENT 522
#define ENVIRONMENT_NAME 523
#define ENVIRONMENT_VALUE 524
#define EOL 525
#define EOP 526
#define EOS 527
#define EQUAL 528
#define ERASE 529
#define ERROR 530
#define ESCAPE 531
#define ESCAPE_BUTTON 532
#define ESI 533
#define EVALUATE 534
#define EVENT 535
#define EVENT_LIST 536
#define EVENT_STATUS 537
#define EVERY 538
#define EXCEPTION 539
#define EXCEPTION_CONDITION 540
#define EXCEPTION_VALUE 541
#define EXPAND 542
#define EXCLUSIVE 543
#define EXIT 544
#define EXPONENTIATION 545
#define EXTEND 546
#define EXTERNAL 547
#define EXTERNAL_FORM 548
#define F 549
#define FD 550
#define FH__FCD 551
#define FH__KEYDEF 552
#define FILE_CONTROL 553
#define FILE_ID 554
#define FILE_NAME 555
#define FILE_POS 556
#define FILL_COLOR 557
#define FILL_COLOR2 558
#define FILL_PERCENT 559
#define FILLER 560
#define FINAL 561
#define FINISH_REASON 562
#define FIRST 563
#define FIXED 564
#define FIXED_FONT 565
#define FIXED_WIDTH 566
#define FLAT 567
#define FLAT_BUTTONS 568
#define FLOAT_BINARY_128 569
#define FLOAT_BINARY_32 570
#define FLOAT_BINARY_64 571
#define FLOAT_DECIMAL_16 572
#define FLOAT_DECIMAL_34 573
#define FLOAT_DECIMAL_7 574
#define FLOAT_EXTENDED 575
#define FLOAT_LONG 576
#define FLOAT_SHORT 577
#define FLOATING 578
#define FONT 579
#define FOOTING 580
#define FOR 581
#define FOREGROUND_COLOR 582
#define FOREVER 583
#define FORMATTED_DATE_FUNC 584
#define FORMATTED_DATETIME_FUNC 585
#define FORMATTED_TIME_FUNC 586
#define FRAME 587
#define FRAMED 588
#define FREE 589
#define FROM 590
#define FROM_CRT 591
#define FULL 592
#define FULL_HEIGHT 593
#define FUNCTION 594
#define FUNCTION_ID 595
#define FUNCTION_NAME 596
#define GENERATE 597
#define GIVING 598
#define GLOBAL 599
#define GO 600
#define GO_BACK 601
#define GO_FORWARD 602
#define GO_HOME 603
#define GO_SEARCH 604
#define GOBACK 605
#define GRAPHICAL 606
#define GREATER 607
#define GREATER_OR_EQUAL 608
#define GRID 609
#define GROUP 610
#define GROUP_VALUE 611
#define HANDLE 612
#define HAS_CHILDREN 613
#define HEADING 614
#define HEADING_COLOR 615
#define HEADING_DIVIDER_COLOR 616
#define HEADING_FONT 617
#define HEAVY 618
#define HEIGHT_IN_CELLS 619
#define HIDDEN_DATA 620
#define HIGHLIGHT 621
#define HIGH_COLOR 622
#define HIGH_VALUE 623
#define HOT_TRACK 624
#define HSCROLL 625
#define HSCROLL_POS 626
#define ICON 627
#define ID 628
#define IDENTIFIED 629
#define IDENTIFICATION 630
#define IF 631
#define IGNORE 632
#define IGNORING 633
#define IN 634
#define INDEPENDENT 635
#define INDEX 636
#define INDEXED 637
#define INDICATE 638
#define INITIALIZE 639
#define INITIALIZED 640
#define INITIATE 641
#define INPUT 642
#define INPUT_OUTPUT 643
#define INQUIRE 644
#define INSERTION_INDEX 645
#define INSERT_ROWS 646
#define INSPECT 647
#define INTERMEDIATE 648
#define INTO 649
#define INTRINSIC 650
#define INVALID 651
#define INVALID_KEY 652
#define IS 653
#define ITEM 654
#define ITEM_TEXT 655
#define ITEM_TO_ADD 656
#define ITEM_TO_DELETE 657
#define ITEM_TO_EMPTY 658
#define ITEM_VALUE 659
#define I_O 660
#define I_O_CONTROL 661
#define JSON 662
#define JUSTIFIED 663
#define KEPT 664
#define KEY 665
#define KEYBOARD 666
#define LABEL 667
#define LABEL_OFFSET 668
#define LARGE_FONT 669
#define LARGE_OFFSET 670
#define LAST 671
#define LAST_ROW 672
#define LAYOUT_DATA 673
#define LAYOUT_MANAGER 674
#define LEADING 675
#define LEADING_SHIFT 676
#define LEFT 677
#define LEFTLINE 678
#define LEFT_TEXT 679
#define LENGTH 680
#define LENGTH_FUNC 681
#define LESS 682
#define LESS_OR_EQUAL 683
#define LEVEL_NUMBER 684
#define LIMIT 685
#define LIMITS 686
#define LINAGE 687
#define LINAGE_COUNTER 688
#define LINE 689
#define LINE_COUNTER 690
#define LINE_LIMIT 691
#define LINE_SEQUENTIAL 692
#define LINES 693
#define LINES_AT_ROOT 694
#define LINKAGE 695
#define LIST_BOX 696
#define LITERAL 697
#define LM_RESIZE 698
#define LOC 699
#define LOCALE 700
#define LOCALE_DATE_FUNC 701
#define LOCALE_TIME_FUNC 702
#define LOCALE_TIME_FROM_FUNC 703
#define LOCAL_STORAGE 704
#define LOCK 705
#define LONG_DATE 706
#define LOWER 707
#define LOWERED 708
#define LOWER_CASE_FUNC 709
#define LOWLIGHT 710
#define LOW_COLOR 711
#define LOW_VALUE 712
#define MAGNETIC_TAPE 713
#define MANUAL 714
#define MASS_UPDATE 715
#define MAX_LINES 716
#define MAX_PROGRESS 717
#define MAX_TEXT 718
#define MAX_VAL 719
#define MEMORY 720
#define MEDIUM_FONT 721
#define MENU 722
#define MERGE 723
#define MESSAGE 724
#define MINUS 725
#define MIN_VAL 726
#define MNEMONIC_NAME 727
#define MODE 728
#define MODIFY 729
#define MODULES 730
#define MOVE 731
#define MULTILINE 732
#define MULTIPLE 733
#define MULTIPLY 734
#define NAME 735
#define NAMESPACE 736
#define NAMESPACE_PREFIX 737
#define NATIONAL 738
#define NATIONAL_EDITED 739
#define NATIONAL_OF_FUNC 740
#define NATIVE 741
#define NAVIGATE_URL 742
#define NEAREST_AWAY_FROM_ZERO 743
#define NEAREST_EVEN 744
#define NEAREST_TOWARD_ZERO 745
#define NEGATIVE 746
#define NESTED 747
#define NEW 748
#define NEXT 749
#define NEXT_ITEM 750
#define NEXT_GROUP 751
#define NEXT_PAGE 752
#define NO 753
#define NO_ADVANCING 754
#define NO_AUTOSEL 755
#define NO_AUTO_DEFAULT 756
#define NO_BOX 757
#define NO_DATA 758
#define NO_DIVIDERS 759
#define NO_ECHO 760
#define NO_F4 761
#define NO_FOCUS 762
#define NO_GROUP_TAB 763
#define NO_KEY_LETTER 764
#define NO_SEARCH 765
#define NO_UPDOWN 766
#define NONNUMERIC 767
#define NORMAL 768
#define NOT 769
#define NOTAB 770
#define NOTHING 771
#define NOTIFY 772
#define NOTIFY_CHANGE 773
#define NOTIFY_DBLCLICK 774
#define NOTIFY_SELCHANGE 775
#define NOT_END 776
#define NOT_EOP 777
#define NOT_ESCAPE 778
#define NOT_EQUAL 779
#define NOT_EXCEPTION 780
#define NOT_INVALID_KEY 781
#define NOT_OVERFLOW 782
#define NOT_SIZE_ERROR 783
#define NUM_COL_HEADINGS 784
#define NUM_ROWS 785
#define NUMBER 786
#define NUMBERS 787
#define NUMERIC 788
#define NUMERIC_EDITED 789
#define NUMVALC_FUNC 790
#define OBJECT 791
#define OBJECT_COMPUTER 792
#define OCCURS 793
#define OF 794
#define OFF 795
#define OK_BUTTON 796
#define OMITTED 797
#define ON 798
#define ONLY 799
#define OPEN 800
#define OPTIONAL 801
#define OPTIONS 802
#define OR 803
#define ORDER 804
#define ORGANIZATION 805
#define OTHER 806
#define OTHERS 807
#define OUTPUT 808
#define OVERLAP_LEFT 809
#define OVERLAP_TOP 810
#define OVERLINE 811
#define PACKED_DECIMAL 812
#define PADDING 813
#define PAGE 814
#define PAGE_COUNTER 815
#define PAGE_SETUP 816
#define PAGED 817
#define PARAGRAPH 818
#define PARENT 819
#define PARSE 820
#define PASSWORD 821
#define PERFORM 822
#define PERMANENT 823
#define PH 824
#define PF 825
#define PHYSICAL 826
#define PICTURE 827
#define PICTURE_SYMBOL 828
#define PIXEL 829
#define PLACEMENT 830
#define PLUS 831
#define POINTER 832
#define POP_UP 833
#define POS 834
#define POSITION 835
#define POSITION_SHIFT 836
#define POSITIVE 837
#define PRESENT 838
#define PREVIOUS 839
#define PRINT 840
#define PRINT_NO_PROMPT 841
#define PRINT_PREVIEW 842
#define PRINTER 843
#define PRINTER_1 844
#define PRINTING 845
#define PRIORITY 846
#define PROCEDURE 847
#define PROCEDURES 848
#define PROCEED 849
#define PROCESSING 850
#define PROGRAM 851
#define PROGRAM_ID 852
#define PROGRAM_NAME 853
#define PROGRAM_POINTER 854
#define PROGRESS 855
#define PROHIBITED 856
#define PROMPT 857
#define PROPERTIES 858
#define PROPERTY 859
#define PROTECTED 860
#define PURGE 861
#define PUSH_BUTTON 862
#define QUERY_INDEX 863
#define QUEUE 864
#define QUOTE 865
#define RADIO_BUTTON 866
#define RAISE 867
#define RAISED 868
#define RANDOM 869
#define RD 870
#define READ 871
#define READERS 872
#define READ_ONLY 873
#define READY_TRACE 874
#define RECEIVE 875
#define RECORD 876
#define RECORD_DATA 877
#define RECORD_TO_ADD 878
#define RECORD_TO_DELETE 879
#define RECORDING 880
#define RECORDS 881
#define RECURSIVE 882
#define REDEFINES 883
#define REEL 884
#define REFERENCE 885
#define REFERENCES 886
#define REFRESH 887
#define REGION_COLOR 888
#define RELATIVE 889
#define RELEASE 890
#define REMAINDER 891
#define REMOVAL 892
#define RENAMES 893
#define REPLACE 894
#define REPLACING 895
#define REPORT 896
#define REPORTING 897
#define REPORTS 898
#define REPOSITORY 899
#define REQUIRED 900
#define RESERVE 901
#define RESET 902
#define RESET_TRACE 903
#define RESET_GRID 904
#define RESET_LIST 905
#define RESET_TABS 906
#define RETRY 907
#define RETURN 908
#define RETURNING 909
#define REVERSE 910
#define REVERSE_FUNC 911
#define REVERSE_VIDEO 912
#define REVERSED 913
#define REWIND 914
#define REWRITE 915
#define RF 916
#define RH 917
#define RIGHT 918
#define RIGHT_ALIGN 919
#define RIMMED 920
#define ROLLBACK 921
#define ROUNDED 922
#define ROUNDING 923
#define ROW_COLOR 924
#define ROW_COLOR_PATTERN 925
#define ROW_DIVIDERS 926
#define ROW_FONT 927
#define ROW_HEADINGS 928
#define ROW_PROTECTION 929
#define RUN 930
#define S 931
#define SAME 932
#define SAVE_AS 933
#define SAVE_AS_NO_PROMPT 934
#define SCREEN 935
#define SCREEN_CONTROL 936
#define SCROLL 937
#define SCROLL_BAR 938
#define SD 939
#define SEARCH 940
#define SEARCH_OPTIONS 941
#define SEARCH_TEXT 942
#define SECONDS 943
#define SECTION 944
#define SECURE 945
#define SEGMENT 946
#define SEGMENT_LIMIT 947
#define SELECT 948
#define SELECTION_INDEX 949
#define SELECTION_TEXT 950
#define SELECT_ALL 951
#define SELF_ACT 952
#define SEMI_COLON 953
#define SEND 954
#define SENTENCE 955
#define SEPARATE 956
#define SEPARATION 957
#define SEQUENCE 958
#define SEQUENTIAL 959
#define SET 960
#define SEVENTY_EIGHT 961
#define SHADING 962
#define SHADOW 963
#define SHARING 964
#define SHORT_DATE 965
#define SHOW_LINES 966
#define SHOW_NONE 967
#define SHOW_SEL_ALWAYS 968
#define SIGN 969
#define SIGNED 970
#define SIGNED_INT 971
#define SIGNED_LONG 972
#define SIGNED_SHORT 973
#define SIXTY_SIX 974
#define SIZE 975
#define SIZE_ERROR 976
#define SMALL_FONT 977
#define SORT 978
#define SORT_MERGE 979
#define SORT_ORDER 980
#define SOURCE 981
#define SOURCE_COMPUTER 982
#define SPACE 983
#define SPECIAL_NAMES 984
#define SPINNER 985
#define SQUARE 986
#define STANDARD 987
#define STANDARD_1 988
#define STANDARD_2 989
#define STANDARD_BINARY 990
#define STANDARD_DECIMAL 991
#define START 992
#define START_X 993
#define START_Y 994
#define STATIC 995
#define STATIC_LIST 996
#define STATUS 997
#define STATUS_BAR 998
#define STATUS_TEXT 999
#define STDCALL 1000
#define STEP 1001
#define STOP 1002
#define STRING 1003
#define STYLE 1004
#define SUB_QUEUE_1 1005
#define SUB_QUEUE_2 1006
#define SUB_QUEUE_3 1007
#define SUBSTITUTE_FUNC 1008
#define SUBSTITUTE_CASE_FUNC 1009
#define SUBTRACT 1010
#define SUBWINDOW 1011
#define SUM 1012
#define SUPPRESS 1013
#define SUPPRESS_XML 1014
#define SYMBOLIC 1015
#define SYNCHRONIZED 1016
#define SYSTEM_DEFAULT 1017
#define SYSTEM_INFO 1018
#define SYSTEM_OFFSET 1019
#define TAB 1020
#define TAB_TO_ADD 1021
#define TAB_TO_DELETE 1022
#define TABLE 1023
#define TALLYING 1024
#define TEMPORARY 1025
#define TAPE 1026
#define TERMINAL 1027
#define TERMINATE 1028
#define TERMINAL_INFO 1029
#define TERMINATION_VALUE 1030
#define TEST 1031
#define TEXT 1032
#define THAN 1033
#define THEN 1034
#define THREAD 1035
#define THREADS 1036
#define THRU 1037
#define THUMB_POSITION 1038
#define TILED_HEADINGS 1039
#define TIME 1040
#define TIME_OUT 1041
#define TIMES 1042
#define TITLE 1043
#define TITLE_POSITION 1044
#define TO 1045
#define TOK_AMPER 1046
#define TOK_CLOSE_PAREN 1047
#define TOK_COLON 1048
#define TOK_DIV 1049
#define TOK_DOT 1050
#define TOK_EQUAL 1051
#define TOK_EXTERN 1052
#define TOK_FALSE 1053
#define TOK_FILE 1054
#define TOK_GREATER 1055
#define TOK_INITIAL 1056
#define TOK_LESS 1057
#define TOK_MINUS 1058
#define TOK_MUL 1059
#define TOK_NULL 1060
#define TOK_OVERFLOW 1061
#define TOK_OPEN_PAREN 1062
#define TOK_PLUS 1063
#define TOK_TRUE 1064
#define TOP 1065
#define TOWARD_GREATER 1066
#define TOWARD_LESSER 1067
#define TRADITIONAL_FONT 1068
#define TRAILING 1069
#define TRAILING_SHIFT 1070
#define TRANSFORM 1071
#define TRANSPARENT 1072
#define TREE_VIEW 1073
#define TRIM_FUNC 1074
#define TRUNCATION 1075
#define TYPE 1076
#define U 1077
#define UCS_4 1078
#define UNBOUNDED 1079
#define UNDERLINE 1080
#define UNFRAMED 1081
#define UNIT 1082
#define UNLOCK 1083
#define UNSIGNED 1084
#define UNSIGNED_INT 1085
#define UNSIGNED_LONG 1086
#define UNSIGNED_SHORT 1087
#define UNSORTED 1088
#define UNSTRING 1089
#define UNTIL 1090
#define UP 1091
#define UPDATE 1092
#define UPDATERS 1093
#define UPON 1094
#define UPON_ARGUMENT_NUMBER 1095
#define UPON_COMMAND_LINE 1096
#define UPON_ENVIRONMENT_NAME 1097
#define UPON_ENVIRONMENT_VALUE 1098
#define UPPER 1099
#define UPPER_CASE_FUNC 1100
#define USAGE 1101
#define USE 1102
#define USE_ALT 1103
#define USE_RETURN 1104
#define USE_TAB 1105
#define USER 1106
#define USER_DEFAULT 1107
#define USER_FUNCTION_NAME 1108
#define USING 1109
#define UTF_8 1110
#define UTF_16 1111
#define V 1112
#define VALIDATE 1113
#define VALIDATING 1114
#define VALUE 1115
#define VALUE_FORMAT 1116
#define VARIABLE 1117
#define VARIANT 1118
#define VARYING 1119
#define VERTICAL 1120
#define VERY_HEAVY 1121
#define VIRTUAL_WIDTH 1122
#define VOLATILE 1123
#define VPADDING 1124
#define VSCROLL 1125
#define VSCROLL_BAR 1126
#define VSCROLL_POS 1127
#define VTOP 1128
#define WAIT 1129
#define WEB_BROWSER 1130
#define WHEN 1131
#define WHEN_COMPILED_FUNC 1132
#define WHEN_XML 1133
#define WIDTH 1134
#define WIDTH_IN_CELLS 1135
#define WINDOW 1136
#define WITH 1137
#define WORD 1138
#define WORDS 1139
#define WORKING_STORAGE 1140
#define WRAP 1141
#define WRITE 1142
#define WRITERS 1143
#define X 1144
#define XML 1145
#define XML_DECLARATION 1146
#define Y 1147
#define YYYYDDD 1148
#define YYYYMMDD 1149
#define ZERO 1150
#define SHIFT_PREFER 1151

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 3861 "parser.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   17587

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  897
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  1254
/* YYNRULES -- Number of rules.  */
#define YYNRULES  3061
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  4291

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   1151

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint16 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250,   251,   252,   253,   254,
     255,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   570,   571,   572,   573,   574,
     575,   576,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   588,   589,   590,   591,   592,   593,   594,
     595,   596,   597,   598,   599,   600,   601,   602,   603,   604,
     605,   606,   607,   608,   609,   610,   611,   612,   613,   614,
     615,   616,   617,   618,   619,   620,   621,   622,   623,   624,
     625,   626,   627,   628,   629,   630,   631,   632,   633,   634,
     635,   636,   637,   638,   639,   640,   641,   642,   643,   644,
     645,   646,   647,   648,   649,   650,   651,   652,   653,   654,
     655,   656,   657,   658,   659,   660,   661,   662,   663,   664,
     665,   666,   667,   668,   669,   670,   671,   672,   673,   674,
     675,   676,   677,   678,   679,   680,   681,   682,   683,   684,
     685,   686,   687,   688,   689,   690,   691,   692,   693,   694,
     695,   696,   697,   698,   699,   700,   701,   702,   703,   704,
     705,   706,   707,   708,   709,   710,   711,   712,   713,   714,
     715,   716,   717,   718,   719,   720,   721,   722,   723,   724,
     725,   726,   727,   728,   729,   730,   731,   732,   733,   734,
     735,   736,   737,   738,   739,   740,   741,   742,   743,   744,
     745,   746,   747,   748,   749,   750,   751,   752,   753,   754,
     755,   756,   757,   758,   759,   760,   761,   762,   763,   764,
     765,   766,   767,   768,   769,   770,   771,   772,   773,   774,
     775,   776,   777,   778,   779,   780,   781,   782,   783,   784,
     785,   786,   787,   788,   789,   790,   791,   792,   793,   794,
     795,   796,   797,   798,   799,   800,   801,   802,   803,   804,
     805,   806,   807,   808,   809,   810,   811,   812,   813,   814,
     815,   816,   817,   818,   819,   820,   821,   822,   823,   824,
     825,   826,   827,   828,   829,   830,   831,   832,   833,   834,
     835,   836,   837,   838,   839,   840,   841,   842,   843,   844,
     845,   846,   847,   848,   849,   850,   851,   852,   853,   854,
     855,   856,   857,   858,   859,   860,   861,   862,   863,   864,
     865,   866,   867,   868,   869,   870,   871,   872,   873,   874,
     875,   876,   877,   878,   879,   880,   881,   882,   883,   884,
     885,   886,   887,   888,   889,   890,   891,   892,   893,   894,
     895,   896
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  2997,  2997,  2997,  3031,  3032,  3036,  3036,  3045,  3046,
    3050,  3051,  3055,  3055,  3081,  3093,  3102,  3106,  3110,  3111,
    3116,  3115,  3128,  3127,  3141,  3145,  3140,  3156,  3157,  3166,
    3166,  3171,  3175,  3170,  3191,  3190,  3206,  3217,  3224,  3225,
    3232,  3233,  3236,  3237,  3241,  3250,  3259,  3260,  3267,  3268,
    3272,  3276,  3282,  3284,  3292,  3299,  3301,  3305,  3312,  3316,
    3320,  3336,  3339,  3349,  3351,  3358,  3362,  3366,  3372,  3374,
    3381,  3385,  3389,  3393,  3402,  3407,  3408,  3417,  3421,  3422,
    3432,  3434,  3438,  3439,  3443,  3444,  3445,  3446,  3447,  3454,
    3453,  3464,  3465,  3468,  3469,  3482,  3481,  3495,  3496,  3497,
    3498,  3502,  3503,  3507,  3508,  3509,  3510,  3514,  3522,  3531,
    3530,  3538,  3542,  3548,  3552,  3557,  3564,  3574,  3590,  3601,
    3605,  3609,  3613,  3620,  3621,  3628,  3627,  3640,  3642,  3643,
    3650,  3651,  3655,  3659,  3665,  3666,  3673,  3680,  3685,  3696,
    3710,  3713,  3714,  3718,  3719,  3720,  3721,  3722,  3723,  3724,
    3725,  3726,  3727,  3728,  3729,  3730,  3738,  3737,  3756,  3767,
    3786,  3794,  3797,  3798,  3802,  3809,  3824,  3845,  3844,  3869,
    3868,  3877,  3876,  3886,  3888,  3892,  3896,  3897,  3903,  3909,
    3915,  3924,  3925,  3932,  3939,  3949,  3955,  3963,  3973,  3977,
    3984,  3988,  3993,  3992,  4003,  4007,  4014,  4015,  4016,  4017,
    4018,  4019,  4023,  4024,  4031,  4046,  4049,  4056,  4064,  4068,
    4079,  4099,  4107,  4118,  4119,  4126,  4140,  4141,  4145,  4166,
    4187,  4188,  4192,  4196,  4214,  4216,  4220,  4227,  4229,  4239,
    4260,  4327,  4330,  4339,  4358,  4374,  4392,  4410,  4427,  4444,
    4450,  4451,  4460,  4461,  4469,  4470,  4475,  4474,  4514,  4515,
    4521,  4522,  4531,  4532,  4533,  4534,  4535,  4536,  4537,  4538,
    4539,  4540,  4541,  4542,  4543,  4544,  4567,  4572,  4581,  4591,
    4603,  4615,  4647,  4648,  4649,  4654,  4655,  4656,  4657,  4661,
    4662,  4663,  4664,  4665,  4666,  4667,  4670,  4672,  4678,  4680,
    4684,  4688,  4689,  4694,  4697,  4698,  4705,  4712,  4713,  4714,
    4721,  4776,  4779,  4784,  4783,  4809,  4812,  4816,  4826,  4837,
    4836,  4844,  4848,  4854,  4858,  4863,  4870,  4880,  4891,  4906,
    4924,  4926,  4927,  4933,  4933,  4940,  4944,  4948,  4955,  4956,
    4957,  4961,  4967,  4968,  4972,  4978,  4979,  4995,  4996,  5000,
    5006,  5012,  5018,  5031,  5042,  5041,  5050,  5062,  5075,  5087,
    5104,  5143,  5146,  5153,  5154,  5158,  5158,  5162,  5167,  5185,
    5196,  5203,  5204,  5210,  5218,  5219,  5220,  5225,  5226,  5230,
    5238,  5239,  5240,  5247,  5248,  5252,  5253,  5254,  5260,  5288,
    5289,  5290,  5291,  5297,  5308,  5307,  5323,  5324,  5328,  5331,
    5332,  5342,  5339,  5353,  5354,  5362,  5363,  5371,  5372,  5376,
    5396,  5395,  5418,  5425,  5429,  5435,  5436,  5440,  5450,  5465,
    5466,  5467,  5468,  5469,  5470,  5471,  5472,  5473,  5480,  5487,
    5487,  5487,  5493,  5517,  5556,  5598,  5599,  5606,  5607,  5611,
    5612,  5619,  5630,  5635,  5646,  5647,  5651,  5652,  5658,  5669,
    5687,  5688,  5692,  5693,  5694,  5698,  5705,  5712,  5721,  5730,
    5731,  5732,  5733,  5734,  5743,  5744,  5750,  5787,  5788,  5801,
    5816,  5817,  5821,  5835,  5853,  5855,  5854,  5872,  5873,  5877,
    5894,  5893,  5914,  5915,  5919,  5920,  5921,  5924,  5926,  5927,
    5931,  5932,  5936,  5937,  5938,  5939,  5940,  5941,  5942,  5943,
    5944,  5945,  5946,  5950,  5954,  5956,  5960,  5961,  5965,  5966,
    5967,  5968,  5969,  5970,  5971,  5974,  5976,  5977,  5981,  5982,
    5986,  5987,  5988,  5989,  5990,  5991,  5995,  6000,  6002,  6001,
    6017,  6021,  6021,  6039,  6040,  6044,  6045,  6046,  6048,  6047,
    6063,  6080,  6086,  6088,  6092,  6099,  6103,  6114,  6117,  6129,
    6130,  6132,  6137,  6141,  6147,  6151,  6155,  6159,  6163,  6167,
    6171,  6179,  6183,  6187,  6191,  6195,  6199,  6210,  6211,  6215,
    6216,  6220,  6221,  6222,  6226,  6227,  6231,  6256,  6259,  6267,
    6266,  6279,  6307,  6306,  6321,  6325,  6332,  6338,  6342,  6349,
    6350,  6354,  6355,  6356,  6357,  6358,  6359,  6360,  6361,  6362,
    6363,  6366,  6368,  6372,  6376,  6380,  6381,  6382,  6383,  6384,
    6385,  6386,  6387,  6388,  6389,  6390,  6391,  6392,  6393,  6394,
    6395,  6402,  6423,  6451,  6454,  6462,  6463,  6467,  6492,  6509,
    6530,  6531,  6538,  6540,  6548,  6563,  6564,  6565,  6578,  6585,
    6589,  6594,  6598,  6603,  6612,  6616,  6620,  6624,  6628,  6632,
    6636,  6640,  6644,  6648,  6652,  6656,  6661,  6666,  6670,  6674,
    6678,  6683,  6687,  6692,  6696,  6701,  6706,  6711,  6715,  6719,
    6727,  6731,  6735,  6743,  6747,  6751,  6755,  6759,  6763,  6767,
    6771,  6775,  6783,  6791,  6795,  6799,  6803,  6807,  6811,  6819,
    6820,  6823,  6825,  6826,  6827,  6828,  6829,  6830,  6833,  6835,
    6841,  6848,  6861,  6870,  6871,  6880,  6887,  6899,  6917,  6918,
    6922,  6923,  6927,  6928,  6931,  6932,  6937,  6938,  6945,  6946,
    6952,  6954,  6956,  6955,  6964,  6965,  6969,  6993,  6994,  6998,
    7031,  7032,  7035,  7037,  7040,  7047,  7048,  7053,  7064,  7075,
    7082,  7084,  7085,  7095,  7106,  7133,  7132,  7141,  7142,  7146,
    7147,  7150,  7152,  7164,  7173,  7188,  7211,  7230,  7232,  7231,
    7251,  7253,  7252,  7268,  7270,  7269,  7280,  7281,  7288,  7287,
    7317,  7318,  7319,  7326,  7332,  7337,  7338,  7344,  7351,  7352,
    7353,  7357,  7364,  7365,  7369,  7379,  7418,  7429,  7430,  7444,
    7457,  7458,  7461,  7462,  7467,  7468,  7469,  7470,  7471,  7472,
    7484,  7498,  7512,  7526,  7540,  7553,  7554,  7559,  7558,  7577,
    7589,  7590,  7594,  7595,  7596,  7597,  7598,  7599,  7600,  7601,
    7602,  7603,  7604,  7605,  7606,  7607,  7608,  7609,  7613,  7620,
    7624,  7628,  7629,  7630,  7637,  7641,  7649,  7652,  7660,  7670,
    7671,  7676,  7679,  7684,  7688,  7696,  7703,  7712,  7717,  7722,
    7729,  7730,  7731,  7735,  7743,  7744,  7745,  7752,  7756,  7763,
    7768,  7774,  7781,  7787,  7797,  7801,  7808,  7810,  7814,  7818,
    7822,  7826,  7833,  7841,  7844,  7846,  7850,  7856,  7860,  7875,
    7893,  7911,  7926,  7929,  7931,  7935,  7939,  7943,  7950,  7970,
    7974,  7975,  7979,  8011,  8019,  8028,  8030,  8029,  8052,  8053,
    8057,  8058,  8062,  8065,  8064,  8124,  8144,  8123,  8188,  8208,
    8210,  8214,  8219,  8224,  8228,  8232,  8237,  8242,  8247,  8252,
    8261,  8265,  8269,  8273,  8277,  8283,  8287,  8292,  8298,  8302,
    8307,  8312,  8317,  8322,  8327,  8332,  8341,  8345,  8349,  8354,
    8358,  8362,  8366,  8370,  8374,  8378,  8382,  8393,  8398,  8403,
    8404,  8405,  8406,  8407,  8408,  8409,  8410,  8411,  8420,  8425,
    8436,  8437,  8444,  8445,  8446,  8447,  8448,  8449,  8450,  8451,
    8452,  8455,  8458,  8459,  8460,  8461,  8462,  8463,  8470,  8471,
    8476,  8477,  8480,  8482,  8486,  8487,  8491,  8492,  8496,  8497,
    8501,  8502,  8506,  8507,  8508,  8509,  8510,  8513,  8514,  8515,
    8516,  8517,  8519,  8520,  8522,  8523,  8527,  8528,  8529,  8530,
    8532,  8534,  8536,  8537,  8538,  8539,  8540,  8541,  8542,  8543,
    8544,  8550,  8551,  8552,  8553,  8554,  8555,  8556,  8557,  8558,
    8559,  8563,  8564,  8569,  8570,  8571,  8572,  8573,  8577,  8585,
    8586,  8587,  8588,  8589,  8590,  8591,  8592,  8593,  8594,  8595,
    8597,  8599,  8600,  8601,  8605,  8606,  8607,  8608,  8609,  8610,
    8611,  8612,  8613,  8614,  8619,  8620,  8621,  8622,  8623,  8624,
    8625,  8626,  8627,  8628,  8633,  8634,  8645,  8646,  8670,  8671,
    8688,  8691,  8692,  8693,  8696,  8700,  8701,  8702,  8703,  8704,
    8705,  8706,  8707,  8708,  8709,  8710,  8711,  8712,  8713,  8714,
    8715,  8721,  8722,  8723,  8743,  8744,  8745,  8746,  8747,  8748,
    8749,  8750,  8754,  8755,  8756,  8757,  8758,  8759,  8765,  8766,
    8767,  8768,  8769,  8770,  8771,  8772,  8777,  8779,  8780,  8781,
    8786,  8787,  8788,  8792,  8793,  8794,  8795,  8796,  8797,  8808,
    8809,  8810,  8811,  8816,  8819,  8820,  8821,  8822,  8823,  8825,
    8830,  8831,  8832,  8838,  8839,  8840,  8841,  8842,  8843,  8844,
    8845,  8846,  8847,  8851,  8852,  8853,  8854,  8855,  8856,  8857,
    8858,  8859,  8860,  8861,  8862,  8863,  8864,  8866,  8867,  8868,
    8869,  8870,  8871,  8872,  8873,  8874,  8875,  8876,  8877,  8878,
    8879,  8880,  8881,  8882,  8885,  8886,  8887,  8895,  8896,  8897,
    8901,  8902,  8903,  8907,  8908,  8911,  8912,  8913,  8916,  8925,
    8926,  8927,  8928,  8929,  8930,  8931,  8932,  8933,  8934,  8935,
    8936,  8937,  8939,  8940,  8941,  8942,  8943,  8944,  8945,  8946,
    8947,  8948,  8955,  8959,  8963,  8964,  8965,  8966,  8967,  8968,
    8969,  8970,  8976,  8977,  8978,  8983,  8984,  8989,  8994,  8995,
    8999,  9000,  9005,  9006,  9010,  9011,  9012,  9017,  9018,  9022,
    9023,  9027,  9028,  9032,  9033,  9037,  9041,  9042,  9046,  9047,
    9051,  9059,  9061,  9065,  9072,  9082,  9085,  9089,  9096,  9108,
    9118,  9128,  9138,  9150,  9127,  9178,  9178,  9212,  9216,  9215,
    9229,  9228,  9248,  9249,  9254,  9276,  9278,  9282,  9293,  9295,
    9303,  9311,  9319,  9325,  9329,  9363,  9366,  9379,  9384,  9394,
    9422,  9424,  9423,  9460,  9461,  9465,  9466,  9467,  9485,  9486,
    9498,  9497,  9543,  9544,  9548,  9593,  9613,  9616,  9646,  9651,
    9645,  9664,  9664,  9701,  9708,  9709,  9710,  9711,  9712,  9713,
    9714,  9715,  9716,  9717,  9718,  9719,  9720,  9721,  9722,  9723,
    9724,  9725,  9726,  9727,  9728,  9729,  9730,  9731,  9732,  9733,
    9734,  9735,  9737,  9738,  9739,  9740,  9741,  9742,  9743,  9744,
    9745,  9746,  9747,  9748,  9749,  9750,  9751,  9753,  9754,  9755,
    9756,  9757,  9758,  9759,  9760,  9761,  9762,  9763,  9764,  9765,
    9766,  9767,  9768,  9769,  9770,  9771,  9772,  9773,  9788,  9800,
    9799,  9810,  9809,  9844,  9843,  9854,  9858,  9862,  9868,  9874,
    9879,  9884,  9889,  9894,  9900,  9906,  9910,  9916,  9920,  9925,
    9929,  9933,  9937,  9941,  9945,  9949,  9953,  9967,  9974,  9975,
    9982,  9982,  9994,  9998, 10002, 10009, 10013, 10017, 10024, 10025,
   10029, 10031, 10035, 10036, 10040, 10041, 10045, 10049, 10050, 10059,
   10060, 10065, 10066, 10070, 10071, 10075, 10091, 10107, 10120, 10124,
   10128, 10135, 10141, 10147, 10152, 10158, 10163, 10168, 10173, 10178,
   10184, 10190, 10196, 10203, 10207, 10211, 10215, 10219, 10230, 10235,
   10240, 10245, 10250, 10255, 10261, 10266, 10271, 10277, 10283, 10289,
   10295, 10300, 10305, 10312, 10319, 10325, 10328, 10328, 10332, 10343,
   10344, 10345, 10349, 10350, 10351, 10355, 10356, 10360, 10364, 10383,
   10382, 10391, 10395, 10402, 10406, 10414, 10415, 10419, 10423, 10434,
   10433, 10443, 10447, 10458, 10460, 10473, 10474, 10482, 10481, 10490,
   10491, 10495, 10501, 10501, 10508, 10507, 10524, 10523, 10579, 10582,
   10590, 10594, 10598, 10617, 10623, 10643, 10647, 10657, 10661, 10666,
   10670, 10669, 10686, 10687, 10692, 10700, 10734, 10736, 10740, 10749,
   10762, 10765, 10769, 10773, 10778, 10801, 10802, 10806, 10807, 10811,
   10815, 10819, 10830, 10834, 10841, 10845, 10853, 10857, 10864, 10871,
   10875, 10886, 10885, 10897, 10901, 10908, 10909, 10919, 10918, 10926,
   10927, 10931, 10936, 10944, 10945, 10946, 10947, 10948, 10953, 10952,
   10964, 10965, 10973, 10972, 10981, 10988, 10992, 11002, 11013, 11031,
   11030, 11039, 11046, 11057, 11056, 11065, 11069, 11073, 11078, 11086,
   11090, 11101, 11100, 11109, 11112, 11114, 11120, 11122, 11123, 11124,
   11125, 11133, 11132, 11144, 11148, 11152, 11156, 11160, 11161, 11162,
   11163, 11164, 11165, 11166, 11170, 11178, 11187, 11188, 11193, 11192,
   11236, 11240, 11246, 11248, 11252, 11253, 11257, 11258, 11262, 11266,
   11271, 11275, 11276, 11280, 11284, 11288, 11292, 11299, 11300, 11305,
   11304, 11321, 11328, 11328, 11340, 11344, 11352, 11353, 11354, 11365,
   11364, 11382, 11384, 11388, 11389, 11393, 11397, 11398, 11399, 11400,
   11405, 11410, 11404, 11424, 11425, 11430, 11435, 11429, 11454, 11453,
   11475, 11476, 11477, 11481, 11482, 11487, 11490, 11497, 11510, 11522,
   11529, 11530, 11536, 11537, 11541, 11542, 11543, 11544, 11545, 11546,
   11550, 11553, 11557, 11558, 11559, 11563, 11564, 11565, 11566, 11570,
   11571, 11576, 11577, 11581, 11591, 11607, 11612, 11618, 11624, 11629,
   11634, 11640, 11646, 11652, 11658, 11665, 11669, 11673, 11677, 11681,
   11686, 11691, 11696, 11701, 11707, 11712, 11717, 11724, 11734, 11738,
   11749, 11748, 11757, 11761, 11765, 11769, 11773, 11780, 11784, 11795,
   11794, 11806, 11805, 11815, 11834, 11833, 11860, 11868, 11869, 11874,
   11885, 11896, 11910, 11918, 11926, 11927, 11932, 11938, 11948, 11960,
   11966, 11976, 11988, 11993, 12002, 12003, 12008, 12083, 12084, 12085,
   12086, 12090, 12091, 12095, 12099, 12110, 12109, 12121, 12125, 12150,
   12164, 12187, 12210, 12231, 12255, 12258, 12266, 12265, 12274, 12285,
   12284, 12293, 12306, 12305, 12318, 12327, 12331, 12342, 12362, 12361,
   12370, 12374, 12380, 12387, 12390, 12397, 12403, 12409, 12414, 12426,
   12425, 12433, 12441, 12442, 12446, 12447, 12448, 12453, 12456, 12463,
   12467, 12475, 12482, 12483, 12484, 12485, 12486, 12487, 12488, 12500,
   12503, 12513, 12512, 12520, 12527, 12540, 12539, 12551, 12552, 12559,
   12558, 12567, 12571, 12572, 12573, 12577, 12578, 12579, 12580, 12587,
   12586, 12607, 12617, 12625, 12629, 12636, 12641, 12646, 12651, 12656,
   12661, 12669, 12670, 12674, 12679, 12685, 12687, 12688, 12689, 12690,
   12694, 12722, 12725, 12729, 12733, 12737, 12744, 12751, 12761, 12760,
   12774, 12779, 12772, 12791, 12794, 12801, 12802, 12806, 12814, 12818,
   12828, 12827, 12837, 12844, 12846, 12853, 12852, 12865, 12864, 12877,
   12878, 12882, 12886, 12897, 12896, 12904, 12908, 12919, 12918, 12927,
   12931, 12938, 12942, 12953, 12952, 12961, 12962, 12966, 12994, 12995,
   12999, 13000, 13001, 13002, 13006, 13007, 13011, 13012, 13013, 13017,
   13021, 13022, 13026, 13027, 13033, 13042, 13043, 13044, 13049, 13050,
   13051, 13055, 13062, 13082, 13081, 13093, 13105, 13102, 13119, 13116,
   13132, 13140, 13147, 13151, 13164, 13171, 13183, 13186, 13191, 13195,
   13208, 13215, 13216, 13220, 13221, 13224, 13225, 13230, 13273, 13277,
   13287, 13286, 13299, 13298, 13306, 13311, 13321, 13336, 13335, 13345,
   13374, 13375, 13379, 13383, 13387, 13391, 13398, 13399, 13403, 13407,
   13410, 13412, 13416, 13425, 13426, 13427, 13430, 13432, 13436, 13440,
   13444, 13452, 13453, 13457, 13458, 13462, 13466, 13476, 13487, 13486,
   13495, 13500, 13501, 13505, 13506, 13507, 13511, 13512, 13516, 13520,
   13521, 13525, 13529, 13533, 13543, 13542, 13550, 13560, 13571, 13570,
   13579, 13586, 13590, 13601, 13600, 13612, 13621, 13624, 13628, 13632,
   13639, 13643, 13653, 13665, 13664, 13673, 13677, 13686, 13687, 13692,
   13695, 13703, 13707, 13714, 13722, 13726, 13737, 13736, 13744, 13747,
   13752, 13754, 13758, 13764, 13765, 13766, 13767, 13770, 13772, 13779,
   13778, 13792, 13793, 13794, 13795, 13796, 13797, 13798, 13799, 13803,
   13804, 13808, 13809, 13815, 13824, 13831, 13832, 13836, 13840, 13844,
   13850, 13856, 13860, 13864, 13868, 13877, 13881, 13885, 13894, 13903,
   13904, 13908, 13917, 13918, 13922, 13926, 13935, 13944, 13956, 13955,
   13964, 13963, 14008, 14011, 14028, 14029, 14032, 14033, 14042, 14045,
   14050, 14055, 14065, 14082, 14087, 14097, 14115, 14114, 14124, 14137,
   14140, 14148, 14151, 14156, 14161, 14169, 14170, 14171, 14172, 14173,
   14174, 14178, 14186, 14187, 14191, 14195, 14206, 14205, 14216, 14224,
   14235, 14242, 14246, 14250, 14258, 14270, 14273, 14280, 14284, 14291,
   14292, 14293, 14294, 14301, 14300, 14309, 14316, 14316, 14326, 14327,
   14331, 14345, 14346, 14351, 14352, 14356, 14357, 14361, 14365, 14376,
   14375, 14384, 14388, 14392, 14396, 14404, 14408, 14418, 14429, 14430,
   14437, 14436, 14444, 14451, 14464, 14463, 14471, 14485, 14484, 14492,
   14509, 14508, 14518, 14526, 14527, 14532, 14533, 14538, 14545, 14546,
   14551, 14558, 14559, 14563, 14564, 14568, 14572, 14582, 14581, 14596,
   14601, 14613, 14612, 14621, 14622, 14623, 14624, 14625, 14629, 14656,
   14659, 14671, 14681, 14686, 14691, 14696, 14704, 14744, 14745, 14749,
   14807, 14817, 14845, 14846, 14847, 14848, 14852, 14861, 14868, 14879,
   14910, 14911, 14915, 14921, 14937, 14938, 14945, 14944, 14956, 14966,
   14967, 14972, 14975, 14979, 14983, 14990, 14991, 14995, 14996, 14997,
   15001, 15005, 15015, 15014, 15028, 15039, 15026, 15050, 15052, 15056,
   15057, 15061, 15065, 15077, 15086, 15096, 15099, 15109, 15112, 15120,
   15123, 15132, 15136, 15143, 15151, 15154, 15163, 15167, 15174, 15182,
   15185, 15189, 15190, 15191, 15194, 15196, 15204, 15205, 15209, 15214,
   15219, 15226, 15231, 15236, 15244, 15248, 15255, 15259, 15270, 15269,
   15289, 15282, 15331, 15332, 15333, 15343, 15347, 15354, 15362, 15363,
   15367, 15368, 15372, 15380, 15381, 15386, 15387, 15388, 15398, 15402,
   15409, 15417, 15418, 15422, 15430, 15431, 15432, 15442, 15446, 15453,
   15461, 15462, 15466, 15474, 15475, 15476, 15486, 15490, 15497, 15505,
   15506, 15510, 15520, 15521, 15522, 15532, 15536, 15543, 15551, 15552,
   15556, 15566, 15567, 15568, 15578, 15582, 15589, 15597, 15598, 15602,
   15613, 15614, 15621, 15623, 15632, 15636, 15643, 15651, 15652, 15656,
   15666, 15667, 15677, 15681, 15688, 15696, 15697, 15701, 15711, 15712,
   15716, 15717, 15727, 15731, 15738, 15746, 15747, 15751, 15762, 15765,
   15774, 15777, 15785, 15789, 15798, 15802, 15809, 15810, 15816, 15821,
   15829, 15836, 15836, 15847, 15848, 15852, 15853, 15855, 15857, 15859,
   15860, 15862, 15863, 15864, 15865, 15866, 15868, 15869, 15870, 15873,
   15875, 15879, 15882, 15884, 15885, 15886, 15887, 15888, 15889, 15891,
   15892, 15893, 15894, 15895, 15898, 15899, 15903, 15904, 15908, 15909,
   15913, 15914, 15918, 15922, 15928, 15932, 15938, 15940, 15941, 15945,
   15946, 15947, 15951, 15952, 15953, 15957, 15961, 15965, 15966, 15967,
   15970, 15971, 15981, 15993, 16002, 16018, 16027, 16043, 16058, 16059,
   16064, 16073, 16079, 16089, 16103, 16125, 16129, 16150, 16154, 16175,
   16187, 16201, 16215, 16216, 16221, 16227, 16228, 16233, 16247, 16248,
   16249, 16256, 16267, 16268, 16272, 16280, 16281, 16285, 16286, 16290,
   16300, 16304, 16311, 16320, 16321, 16327, 16336, 16347, 16364, 16368,
   16375, 16376, 16377, 16384, 16385, 16389, 16393, 16400, 16401, 16405,
   16406, 16410, 16411, 16412, 16413, 16417, 16421, 16425, 16429, 16433,
   16437, 16441, 16462, 16471, 16475, 16482, 16483, 16484, 16488, 16489,
   16490, 16491, 16492, 16496, 16500, 16507, 16508, 16509, 16510, 16514,
   16518, 16525, 16537, 16549, 16563, 16564, 16568, 16569, 16573, 16580,
   16587, 16588, 16595, 16596, 16603, 16604, 16605, 16609, 16610, 16614,
   16618, 16622, 16626, 16627, 16631, 16635, 16636, 16640, 16644, 16645,
   16654, 16658, 16663, 16664, 16670, 16674, 16678, 16682, 16683, 16689,
   16693, 16697, 16698, 16702, 16709, 16719, 16737, 16755, 16762, 16769,
   16776, 16786, 16790, 16797, 16801, 16808, 16818, 16828, 16838, 16851,
   16877, 16881, 16889, 16889, 16902, 16907, 16915, 16923, 16927, 16937,
   16952, 16976, 16998, 17011, 17018, 17034, 17035, 17036, 17037, 17038,
   17039, 17043, 17047, 17064, 17068, 17075, 17076, 17077, 17078, 17079,
   17080, 17081, 17085, 17086, 17087, 17088, 17094, 17098, 17102, 17106,
   17110, 17114, 17119, 17123, 17127, 17131, 17135, 17139, 17143, 17147,
   17154, 17155, 17159, 17160, 17161, 17162, 17166, 17167, 17168, 17169,
   17170, 17174, 17178, 17182, 17189, 17193, 17197, 17204, 17211, 17218,
   17228, 17228, 17244, 17251, 17261, 17268, 17278, 17282, 17295, 17299,
   17314, 17322, 17323, 17327, 17328, 17329, 17333, 17334, 17339, 17342,
   17350, 17353, 17360, 17362, 17363, 17367, 17368, 17372, 17373, 17374,
   17379, 17382, 17395, 17399, 17407, 17411, 17415, 17419, 17423, 17427,
   17431, 17435, 17442, 17443, 17447, 17448, 17458, 17459, 17468, 17472,
   17476, 17480, 17487, 17488, 17489, 17490, 17491, 17492, 17493, 17494,
   17495, 17496, 17497, 17498, 17499, 17500, 17501, 17502, 17503, 17504,
   17505, 17506, 17507, 17508, 17509, 17510, 17511, 17512, 17513, 17514,
   17515, 17516, 17517, 17518, 17519, 17520, 17521, 17522, 17523, 17524,
   17525, 17526, 17527, 17528, 17529, 17530, 17531, 17532, 17533, 17534,
   17535, 17536, 17537, 17541, 17542, 17543, 17544, 17545, 17546, 17547,
   17548, 17549, 17550, 17551, 17552, 17553, 17554, 17555, 17556, 17557,
   17558, 17559, 17560, 17561, 17562, 17563, 17570, 17570, 17571, 17571,
   17572, 17572, 17573, 17573, 17574, 17574, 17574, 17575, 17575, 17576,
   17576, 17577, 17577, 17578, 17578, 17579, 17579, 17580, 17580, 17581,
   17581, 17582, 17582, 17583, 17583, 17584, 17584, 17585, 17585, 17586,
   17586, 17587, 17587, 17588, 17588, 17589, 17589, 17590, 17590, 17591,
   17591, 17592, 17592, 17593, 17593, 17593, 17594, 17594, 17594, 17595,
   17595, 17596, 17596, 17597, 17597, 17598, 17598, 17599, 17599, 17600,
   17600, 17600, 17601, 17601, 17601, 17602, 17602, 17602, 17602, 17603,
   17603, 17603, 17604, 17604, 17605, 17605, 17606, 17606, 17606, 17607,
   17607, 17607, 17608, 17608, 17609, 17609, 17610, 17610, 17611, 17611,
   17612, 17612, 17613, 17613, 17614, 17614, 17614, 17615, 17615, 17615,
   17616, 17616, 17617, 17617, 17618, 17618, 17618, 17619, 17619, 17619,
   17619, 17620, 17620, 17621, 17621, 17622, 17622, 17623, 17623, 17624,
   17624, 17625, 17625, 17626, 17626, 17626, 17627, 17627, 17628, 17628,
   17629, 17629, 17630, 17630, 17630, 17631, 17631, 17632, 17632, 17633,
   17633, 17634, 17634, 17635, 17635, 17636, 17636, 17637, 17637, 17638,
   17638, 17639, 17639, 17640, 17640, 17640, 17641, 17641, 17642, 17642,
   17643, 17643, 17644, 17644, 17644, 17648, 17648, 17649, 17649, 17650,
   17650, 17651, 17651, 17651, 17651, 17652, 17652, 17653, 17653, 17654,
   17654, 17655, 17655, 17656, 17656, 17657, 17657, 17658, 17658, 17658,
   17659, 17659, 17660, 17660, 17661, 17661, 17664, 17664, 17665, 17665,
   17666, 17666, 17667, 17667, 17668, 17668, 17669, 17669, 17670, 17670,
   17671, 17671
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "$undefined", "\"3D\"", "ABSENT", "ACCEPT",
  "ACCESS", "\"ACTIVE-X\"", "ACTION", "ADD", "ADDRESS",
  "\"ADJUSTABLE-COLUMNS\"", "ADVANCING", "AFTER", "ALIGNMENT", "ALL",
  "ALLOCATE", "ALLOWING", "ALPHABET", "ALPHABETIC", "\"ALPHABETIC-LOWER\"",
  "\"ALPHABETIC-UPPER\"", "ALPHANUMERIC", "\"ALPHANUMERIC-EDITED\"",
  "ALSO", "ALTER", "ALTERNATE", "AND", "ANY", "APPLY", "ARE", "AREA",
  "AREAS", "\"ARGUMENT-NUMBER\"", "\"ARGUMENT-VALUE\"", "ARITHMETIC", "AS",
  "ASCENDING", "ASCII", "ASSIGN", "AT", "ATTRIBUTE", "ATTRIBUTES", "AUTO",
  "\"AUTO-DECIMAL\"", "\"AUTO-SPIN\"", "AUTOMATIC", "\"AWAY-FROM-ZERO\"",
  "\"BACKGROUND-COLOR\"", "\"BACKGROUND-HIGH\"", "\"BACKGROUND-LOW\"",
  "\"BACKGROUND-STANDARD\"", "BAR", "BASED", "BEFORE", "BELL", "BINARY",
  "\"BINARY-C-LONG\"", "\"BINARY-CHAR\"", "\"BINARY-DOUBLE\"",
  "\"BINARY-LONG\"", "\"BINARY-SEQUENTIAL\"", "\"BINARY-SHORT\"", "BIT",
  "BITMAP", "\"BITMAP-END\"", "\"BITMAP-HANDLE\"", "\"BITMAP-NUMBER\"",
  "\"BITMAP-START\"", "\"BITMAP-TIMER\"", "\"BITMAP-TRAILING\"",
  "\"BITMAP-TRANSPARENT-COLOR\"", "\"BITMAP-WIDTH\"", "BLANK", "BLINK",
  "BLOCK", "BOTTOM", "BOX", "BOXED", "\"BULK-ADDITION\"", "BUSY",
  "BUTTONS", "BY", "\"BYTE-LENGTH\"", "\"CALENDAR-FONT\"", "CALL",
  "CANCEL", "\"CANCEL-BUTTON\"", "CAPACITY", "\"CARD-PUNCH\"",
  "\"CARD-READER\"", "CASSETTE", "CCOL", "CD", "CELL", "\"CELL-COLOR\"",
  "\"CELL-DATA\"", "\"CELL-FONT\"", "\"CELL-PROTECTION\"", "CENTER",
  "CENTERED", "\"CENTERED-HEADINGS\"", "\"CENTURY-DATE\"", "CF", "CH",
  "CHAINING", "CHARACTER", "CHARACTERS", "\"CHECK-BOX\"", "CLASS",
  "CLASSIFICATION", "\"class-name\"", "\"CLEAR-SELECTION\"", "CLINE",
  "CLINES", "CLOSE", "COBOL", "CODE", "\"CODE-SET\"", "COLLATING", "COL",
  "COLOR", "COLORS", "COLS", "COLUMN", "\"COLUMN-COLOR\"",
  "\"COLUMN-DIVIDERS\"", "\"COLUMN-FONT\"", "\"COLUMN-HEADINGS\"",
  "\"COLUMN-PROTECTION\"", "COLUMNS", "\"COMBO-BOX\"", "COMMA",
  "\"COMMAND-LINE\"", "\"comma delimiter\"", "COMMIT", "COMMON",
  "COMMUNICATION", "COMP", "COMPUTE", "\"COMP-0\"", "\"COMP-1\"",
  "\"COMP-2\"", "\"COMP-3\"", "\"COMP-4\"", "\"COMP-5\"", "\"COMP-6\"",
  "\"COMP-N\"", "\"COMP-X\"", "\"FUNCTION CONCATENATE\"", "CONDITION",
  "CONFIGURATION", "CONSTANT", "CONTAINS", "CONTENT",
  "\"FUNCTION CONTENT-LENGTH\"", "\"FUNCTION CONTENT-OF\"", "CONTINUE",
  "CONTROL", "CONTROLS", "CONVERSION", "CONVERTING", "COPY",
  "\"COPY-SELECTION\"", "CORRESPONDING", "COUNT", "CRT", "\"CRT-UNDER\"",
  "CSIZE", "CURRENCY", "\"FUNCTION CURRENT-DATE\"", "CURSOR",
  "\"CURSOR-COL\"", "\"CURSOR-COLOR\"", "\"CURSOR-FRAME-WIDTH\"",
  "\"CURSOR-ROW\"", "\"CURSOR-X\"", "\"CURSOR-Y\"",
  "\"CUSTOM-PRINT-TEMPLATE\"", "CYCLE", "DASHED", "DATA",
  "\"DATA-COLUMNS\"", "\"DATA-TYPES\"", "DATE", "\"DATE-ENTRY\"", "DAY",
  "\"DAY-OF-WEEK\"", "DE", "DEBUGGING", "\"DECIMAL-POINT\"",
  "DECLARATIVES", "DEFAULT", "\"DEFAULT-BUTTON\"", "\"DEFAULT-FONT\"",
  "DELETE", "DELIMITED", "DELIMITER", "DEPENDING", "DESCENDING",
  "DESTINATION", "DESTROY", "DETAIL", "DISABLE", "DISC", "DISK", "DISPLAY",
  "\"DISPLAY-COLUMNS\"", "\"DISPLAY-FORMAT\"", "\"FUNCTION DISPLAY-OF\"",
  "DIVIDE", "DIVIDERS", "\"DIVIDER-COLOR\"", "DIVISION", "DOTDASH",
  "DOTTED", "\"DRAG-COLOR\"", "\"DROP-DOWN\"", "\"DROP-LIST\"", "DOWN",
  "DUPLICATES", "DYNAMIC", "EBCDIC", "EC", "ECHO", "EGI",
  "\"level-number 88\"", "ENABLE", "ELEMENT", "ELSE", "EMI", "ENCRYPTION",
  "ENCODING", "END", "\"END-ACCEPT\"", "\"END-ADD\"", "\"END-CALL\"",
  "\"END-COMPUTE\"", "\"END-COLOR\"", "\"END-DELETE\"", "\"END-DISPLAY\"",
  "\"END-DIVIDE\"", "\"END-EVALUATE\"", "\"END FUNCTION\"", "\"END-IF\"",
  "\"END-JSON\"", "\"END-MODIFY\"", "\"END-MULTIPLY\"", "\"END-PERFORM\"",
  "\"END PROGRAM\"", "\"END-READ\"", "\"END-RECEIVE\"", "\"END-RETURN\"",
  "\"END-REWRITE\"", "\"END-SEARCH\"", "\"END-START\"", "\"END-STRING\"",
  "\"END-SUBTRACT\"", "\"END-UNSTRING\"", "\"END-WRITE\"", "\"END-XML\"",
  "ENGRAVED", "\"ENSURE-VISIBLE\"", "ENTRY", "\"ENTRY-CONVENTION\"",
  "\"ENTRY-FIELD\"", "\"ENTRY-REASON\"", "ENVIRONMENT",
  "\"ENVIRONMENT-NAME\"", "\"ENVIRONMENT-VALUE\"", "EOL", "EOP", "EOS",
  "EQUAL", "ERASE", "ERROR", "ESCAPE", "\"ESCAPE-BUTTON\"", "ESI",
  "EVALUATE", "EVENT", "\"EVENT-LIST\"", "\"EVENT STATUS\"", "EVERY",
  "EXCEPTION", "\"EXCEPTION CONDITION\"", "\"EXCEPTION-VALUE\"", "EXPAND",
  "EXCLUSIVE", "EXIT", "\"exponentiation operator\"", "EXTEND", "EXTERNAL",
  "\"EXTERNAL-FORM\"", "F", "FD", "\"FH--FCD\"", "\"FH--KEYDEF\"",
  "\"FILE-CONTROL\"", "\"FILE-ID\"", "\"FILE-NAME\"", "\"FILE-POS\"",
  "\"FILL-COLOR\"", "\"FILL-COLOR2\"", "\"FILL-PERCENT\"", "FILLER",
  "FINAL", "\"FINISH-REASON\"", "FIRST", "FIXED", "\"FIXED-FONT\"",
  "\"FIXED-WIDTH\"", "FLAT", "\"FLAT-BUTTONS\"", "\"FLOAT-BINARY-128\"",
  "\"FLOAT-BINARY-32\"", "\"FLOAT-BINARY-64\"", "\"FLOAT-DECIMAL-16\"",
  "\"FLOAT-DECIMAL-34\"", "\"FLOAT-DECIMAL-7\"", "\"FLOAT-EXTENDED\"",
  "\"FLOAT-LONG\"", "\"FLOAT-SHORT\"", "FLOATING", "FONT", "FOOTING",
  "FOR", "\"FOREGROUND-COLOR\"", "FOREVER", "\"FUNCTION FORMATTED-DATE\"",
  "\"FUNCTION FORMATTED-DATETIME\"", "\"FUNCTION FORMATTED-TIME\"",
  "FRAME", "FRAMED", "FREE", "FROM", "\"FROM CRT\"", "FULL",
  "\"FULL-HEIGHT\"", "FUNCTION", "\"FUNCTION-ID\"",
  "\"intrinsic function name\"", "GENERATE", "GIVING", "GLOBAL", "GO",
  "\"GO-BACK\"", "\"GO-FORWARD\"", "\"GO-HOME\"", "\"GO-SEARCH\"",
  "GOBACK", "GRAPHICAL", "GREATER", "\"GREATER OR EQUAL\"", "GRID",
  "GROUP", "\"GROUP-VALUE\"", "HANDLE", "\"HAS-CHILDREN\"", "HEADING",
  "\"HEADING-COLOR\"", "\"HEADING-DIVIDER-COLOR\"", "\"HEADING-FONT\"",
  "HEAVY", "\"HEIGHT-IN-CELLS\"", "\"HIDDEN-DATA\"", "HIGHLIGHT",
  "\"HIGH-COLOR\"", "\"HIGH-VALUE\"", "\"HOT-TRACK\"", "HSCROLL",
  "\"HSCROLL-POS\"", "ICON", "ID", "IDENTIFIED", "IDENTIFICATION", "IF",
  "IGNORE", "IGNORING", "IN", "INDEPENDENT", "INDEX", "INDEXED",
  "INDICATE", "INITIALIZE", "INITIALIZED", "INITIATE", "INPUT",
  "\"INPUT-OUTPUT\"", "INQUIRE", "\"INSERTION-INDEX\"", "\"INSERT-ROWS\"",
  "INSPECT", "INTERMEDIATE", "INTO", "INTRINSIC", "INVALID",
  "\"INVALID KEY\"", "IS", "ITEM", "\"ITEM-TEXT\"", "\"ITEM-TO_ADD\"",
  "\"ITEM-TO_DELETE\"", "\"ITEM-TO_EMPTY\"", "\"ITEM-VALUE\"", "\"I-O\"",
  "\"I-O-CONTROL\"", "JSON", "JUSTIFIED", "KEPT", "KEY", "KEYBOARD",
  "LABEL", "\"LABEL-OFFSET\"", "\"LARGE-FONT\"", "\"LARGE-OFFSET\"",
  "LAST", "\"LAST-ROW\"", "\"LAYOUT-DATA\"", "\"LAYOUT-MANAGER\"",
  "LEADING", "\"LEADING-SHIFT\"", "LEFT", "LEFTLINE", "\"LEFT-TEXT\"",
  "LENGTH", "\"FUNCTION LENGTH/BYTE-LENGTH\"", "LESS", "\"LESS OR EQUAL\"",
  "\"level-number\"", "LIMIT", "LIMITS", "LINAGE", "\"LINAGE-COUNTER\"",
  "LINE", "\"LINE-COUNTER\"", "\"LINE LIMIT\"", "\"LINE-SEQUENTIAL\"",
  "LINES", "\"LINES-AT-ROOT\"", "LINKAGE", "\"LIST-BOX\"", "\"Literal\"",
  "\"LM-RESIZE\"", "LOC", "LOCALE", "\"FUNCTION LOCALE-DATE\"",
  "\"FUNCTION LOCALE-TIME\"", "\"FUNCTION LOCALE-TIME-FROM-SECONDS\"",
  "\"LOCAL-STORAGE\"", "LOCK", "\"LONG-DATE\"", "LOWER", "LOWERED",
  "\"FUNCTION LOWER-CASE\"", "LOWLIGHT", "\"LOW-COLOR\"", "\"LOW-VALUE\"",
  "\"MAGNETIC-TAPE\"", "MANUAL", "\"MASS-UPDATE\"", "\"MAX-LINES\"",
  "\"MAX-PROGRESS\"", "\"MAX-TEXT\"", "\"MAX-VAL\"", "MEMORY",
  "\"MEDIUM-FONT\"", "MENU", "MERGE", "MESSAGE", "MINUS", "\"MIN-VAL\"",
  "\"Mnemonic name\"", "MODE", "MODIFY", "MODULES", "MOVE", "MULTILINE",
  "MULTIPLE", "MULTIPLY", "NAME", "NAMESPACE", "\"NAMESPACE-PREFIX\"",
  "NATIONAL", "\"NATIONAL-EDITED\"", "\"FUNCTION NATIONAL-OF\"", "NATIVE",
  "\"NAVIGATE-URL\"", "\"NEAREST-AWAY-FROM-ZERO\"", "\"NEAREST-EVEN\"",
  "\"NEAREST-TOWARD-ZERO\"", "NEGATIVE", "NESTED", "NEW", "NEXT",
  "\"NEXT-ITEM\"", "\"NEXT GROUP\"", "\"NEXT PAGE\"", "NO",
  "\"NO ADVANCING\"", "\"NO-AUTOSEL\"", "\"NO-AUTO-DEFAULT\"",
  "\"NO-BOX\"", "\"NO DATA\"", "\"NO-DIVIDERS\"", "\"NO-ECHO\"",
  "\"NO-F4\"", "\"NO-FOCUS\"", "\"NO-GROUP-TAB\"", "\"NO-KEY-LETTER\"",
  "\"NO-SEARCH\"", "\"NO-UPDOWN\"", "NONNUMERIC", "NORMAL", "NOT", "NOTAB",
  "NOTHING", "NOTIFY", "\"NOTIFY-CHANGE\"", "\"NOTIFY-DBLCLICK\"",
  "\"NOTIFY-SELCHANGE\"", "\"NOT END\"", "\"NOT EOP\"", "\"NOT ESCAPE\"",
  "\"NOT EQUAL\"", "\"NOT EXCEPTION\"", "\"NOT INVALID KEY\"",
  "\"NOT OVERFLOW\"", "\"NOT SIZE ERROR\"", "\"NUM-COL-HEADINGS\"",
  "\"NUM-ROWS\"", "NUMBER", "NUMBERS", "NUMERIC", "\"NUMERIC-EDITED\"",
  "\"FUNCTION NUMVAL-C\"", "OBJECT", "\"OBJECT-COMPUTER\"", "OCCURS", "OF",
  "OFF", "\"OK-BUTTON\"", "OMITTED", "ON", "ONLY", "OPEN", "OPTIONAL",
  "OPTIONS", "OR", "ORDER", "ORGANIZATION", "OTHER", "OTHERS", "OUTPUT",
  "\"OVERLAP-LEFT\"", "\"OVERLAP-TOP\"", "OVERLINE", "\"PACKED-DECIMAL\"",
  "PADDING", "PAGE", "\"PAGE-COUNTER\"", "\"PAGE-SETUP\"", "PAGED",
  "PARAGRAPH", "PARENT", "PARSE", "PASSWORD", "PERFORM", "PERMANENT", "PH",
  "PF", "PHYSICAL", "PICTURE", "\"PICTURE SYMBOL\"", "PIXEL", "PLACEMENT",
  "PLUS", "POINTER", "\"POP-UP\"", "POS", "POSITION", "\"POSITION-SHIFT\"",
  "POSITIVE", "PRESENT", "PREVIOUS", "PRINT", "\"PRINT-NO-PROMPT\"",
  "\"PRINT-PREVIEW\"", "PRINTER", "\"PRINTER-1\"", "PRINTING", "PRIORITY",
  "PROCEDURE", "PROCEDURES", "PROCEED", "PROCESSING", "PROGRAM",
  "\"PROGRAM-ID\"", "\"program name\"", "\"PROGRAM-POINTER\"", "PROGRESS",
  "PROHIBITED", "PROMPT", "PROPERTIES", "PROPERTY", "PROTECTED", "PURGE",
  "\"PUSH-BUTTON\"", "\"QUERY-INDEX\"", "QUEUE", "QUOTE",
  "\"RADIO-BUTTON\"", "RAISE", "RAISED", "RANDOM", "RD", "READ", "READERS",
  "\"READ-ONLY\"", "\"READY TRACE\"", "RECEIVE", "RECORD",
  "\"RECORD-DATA\"", "\"RECORD-TO-ADD\"", "\"RECORD-TO-DELETE\"",
  "RECORDING", "RECORDS", "RECURSIVE", "REDEFINES", "REEL", "REFERENCE",
  "REFERENCES", "REFRESH", "\"REGION-COLOR\"", "RELATIVE", "RELEASE",
  "REMAINDER", "REMOVAL", "RENAMES", "REPLACE", "REPLACING", "REPORT",
  "REPORTING", "REPORTS", "REPOSITORY", "REQUIRED", "RESERVE", "RESET",
  "\"RESET TRACE\"", "\"RESET-GRID\"", "\"RESET-LIST\"", "\"RESET-TABS\"",
  "RETRY", "RETURN", "RETURNING", "REVERSE", "\"FUNCTION REVERSE\"",
  "\"REVERSE-VIDEO\"", "REVERSED", "REWIND", "REWRITE", "RF", "RH",
  "RIGHT", "\"RIGHT-ALIGN\"", "RIMMED", "ROLLBACK", "ROUNDED", "ROUNDING",
  "\"ROW-COLOR\"", "\"ROW-COLOR-PATTERN\"", "\"ROW-DIVIDERS\"",
  "\"ROW-FONT\"", "\"ROW-HEADINGS\"", "\"ROW-PROTECTION\"", "RUN", "S",
  "SAME", "\"SAVE-AS\"", "\"SAVE-AS-NO-PROMPT\"", "SCREEN",
  "\"SCREEN CONTROL\"", "SCROLL", "\"SCROLL-BAR\"", "SD", "SEARCH",
  "\"SEARCH-OPTIONS\"", "\"SEARCH-TEXT\"", "SECONDS", "SECTION", "SECURE",
  "SEGMENT", "\"SEGMENT-LIMIT\"", "SELECT", "\"SELECTION-INDEX\"",
  "\"SELECTION-TEXT\"", "\"SELECTION-ALL\"", "\"SELF-ACT\"",
  "\"semi-colon\"", "SEND", "SENTENCE", "SEPARATE", "SEPARATION",
  "SEQUENCE", "SEQUENTIAL", "SET", "\"level-number 78\"", "SHADING",
  "SHADOW", "SHARING", "\"SHORT-DATE\"", "\"SHOW-LINES\"", "\"SHOW-NONE\"",
  "\"SHOW-SEL-ALWAYS\"", "SIGN", "SIGNED", "\"SIGNED-INT\"",
  "\"SIGNED-LONG\"", "\"SIGNED-SHORT\"", "\"level-number 66\"", "SIZE",
  "\"SIZE ERROR\"", "\"SMALL-FONT\"", "SORT", "\"SORT-MERGE\"",
  "\"SORT-ORDER\"", "SOURCE", "\"SOURCE-COMPUTER\"", "SPACE",
  "\"SPECIAL-NAMES\"", "SPINNER", "SQUARE", "STANDARD", "\"STANDARD-1\"",
  "\"STANDARD-2\"", "\"STANDARD-BINARY\"", "\"STANDARD-DECIMAL\"", "START",
  "\"START-X\"", "\"START-Y\"", "STATIC", "\"STATIC-LIST\"", "STATUS",
  "\"STATUS-BAR\"", "\"STATUS-TEXT\"", "STDCALL", "STEP", "STOP", "STRING",
  "STYLE", "\"SUB-QUEUE-1\"", "\"SUB-QUEUE-2\"", "\"SUB-QUEUE-3\"",
  "\"FUNCTION SUBSTITUTE\"", "\"FUNCTION SUBSTITUTE-CASE\"", "SUBTRACT",
  "SUBWINDOW", "SUM", "SUPPRESS", "\"SUPPRESS\"", "SYMBOLIC",
  "SYNCHRONIZED", "\"SYSTEM-DEFAULT\"", "\"SYSTEM-INFO\"",
  "\"SYSTEM-OFFSET\"", "TAB", "\"TAB-TO-ADD\"", "\"TAB-TO-DELETE\"",
  "TABLE", "TALLYING", "TEMPORARY", "TAPE", "TERMINAL", "TERMINATE",
  "\"TERMINAL-INFO\"", "\"TERMINATION-VALUE\"", "TEST", "TEXT", "THAN",
  "THEN", "THREAD", "THREADS", "THRU", "\"THUMB-POSITION\"",
  "\"TILED-HEADINGS\"", "TIME", "\"TIME-OUT\"", "TIMES", "TITLE",
  "\"TITLE-POSITION\"", "TO", "\"&\"", "\")\"", "\":\"", "\"/\"", "\".\"",
  "\"=\"", "\"EXTERN\"", "\"FALSE\"", "\"FILE\"", "\">\"", "\"INITIAL\"",
  "\"<\"", "\"-\"", "\"*\"", "\"NULL\"", "\"OVERFLOW\"", "\"(\"", "\"+\"",
  "\"TRUE\"", "TOP", "\"TOWARD-GREATER\"", "\"TOWARD-LESSER\"",
  "\"TRADITIONAL-FONT\"", "TRAILING", "\"TRAILING-SHIFT\"", "TRANSFORM",
  "TRANSPARENT", "\"TREE-VIEW\"", "\"FUNCTION TRIM\"", "TRUNCATION",
  "TYPE", "U", "\"UCS-4\"", "UNBOUNDED", "UNDERLINE", "UNFRAMED", "UNIT",
  "UNLOCK", "UNSIGNED", "\"UNSIGNED-INT\"", "\"UNSIGNED-LONG\"",
  "\"UNSIGNED-SHORT\"", "UNSORTED", "UNSTRING", "UNTIL", "UP", "UPDATE",
  "UPDATERS", "UPON", "\"UPON ARGUMENT-NUMBER\"", "\"UPON COMMAND-LINE\"",
  "\"UPON ENVIRONMENT-NAME\"", "\"UPON ENVIRONMENT-VALUE\"", "UPPER",
  "\"FUNCTION UPPER-CASE\"", "USAGE", "USE", "\"USE-ALT\"",
  "\"USE-RETURN\"", "\"USE-TAB\"", "USER", "\"USER-DEFAULT\"",
  "\"user function name\"", "USING", "\"UTF-8\"", "\"UTF-16\"", "V",
  "VALIDATE", "VALIDATING", "VALUE", "\"VALUE-FORMAT\"", "VARIABLE",
  "VARIANT", "VARYING", "VERTICAL", "\"VERY-HEAVY\"", "\"VIRTUAL-WIDTH\"",
  "VOLATILE", "VPADDING", "VSCROLL", "\"VSCROLL-BAR\"", "\"VSCROLL-POS\"",
  "VTOP", "WAIT", "\"WEB-BROWSER\"", "WHEN", "\"FUNCTION WHEN-COMPILED\"",
  "\"WHEN\"", "WIDTH", "\"WIDTH-IN-CELLS\"", "WINDOW", "WITH",
  "\"Identifier\"", "WORDS", "\"WORKING-STORAGE\"", "WRAP", "WRITE",
  "WRITERS", "X", "XML", "\"XML-DECLARATION\"", "Y", "YYYYDDD", "YYYYMMDD",
  "ZERO", "SHIFT_PREFER", "$accept", "start", "$@1", "compilation_group",
  "nested_list", "$@2", "source_element_list", "source_element",
  "simple_prog", "$@3", "program_definition", "function_definition",
  "_end_program_list", "end_program_list", "end_program", "$@4",
  "end_function", "$@5", "_program_body", "$@6", "$@7",
  "_identification_header", "identification_or_id", "program_id_paragraph",
  "$@8", "$@9", "function_id_paragraph", "$@10", "program_id_name",
  "end_program_name", "_as_literal", "_program_type",
  "program_type_clause", "init_or_recurse_and_common", "init_or_recurse",
  "_options_paragraph", "_options_clauses", "_arithmetic_clause",
  "arithmetic_choice", "_default_rounded_clause",
  "_entry_convention_clause", "convention_type",
  "_intermediate_rounding_clause", "intermediate_rounding_choice",
  "_environment_division", "_environment_header", "_configuration_section",
  "_configuration_header", "_configuration_paragraphs",
  "configuration_paragraphs", "configuration_paragraph",
  "source_computer_paragraph", "$@11", "_source_computer_entry",
  "_with_debugging_mode", "object_computer_paragraph", "$@12",
  "_object_computer_entry", "object_clauses_list", "object_clauses",
  "object_computer_memory", "object_computer_sequence",
  "program_collating_sequence", "$@13", "program_coll_sequence_values",
  "object_computer_segment", "object_computer_class", "locale_class",
  "computer_words", "repository_paragraph", "$@14", "_repository_entry",
  "repository_list", "repository_name", "repository_name_list",
  "special_names_header", "special_names_sentence", "special_name_list",
  "special_name", "mnemonic_name_clause", "$@15", "mnemonic_choices",
  "_special_name_mnemonic_on_off", "on_off_clauses", "on_off_clauses_1",
  "alphabet_name_clause", "@16", "alphabet_definition", "@17", "@18",
  "alphabet_target_alphanumeric", "alphabet_target_national",
  "alphabet_type_alphanumeric", "alphabet_type_national",
  "alphabet_type_common", "alphabet_literal_list", "alphabet_literal",
  "@19", "alphabet_also_sequence", "alphabet_lits", "space_or_zero",
  "symbolic_characters_clause", "_sym_in_word", "symbolic_collection",
  "symbolic_chars_list", "symbolic_chars_phrase", "char_list",
  "integer_list", "symbolic_constant_clause", "symbolic_constant_list",
  "symbolic_constant", "class_name_clause", "class_item_list",
  "class_item", "_class_type", "_in_alphabet", "locale_clause",
  "currency_sign_clause", "_with_pic_symbol", "decimal_point_clause",
  "numeric_sign_clause", "cursor_clause", "crt_status_clause",
  "screen_control", "event_status", "_input_output_section",
  "_input_output_header", "_file_control_header", "_file_control_sequence",
  "file_control_entry", "$@20", "_select_clauses_or_error",
  "_select_clause_sequence", "select_clause", "assign_clause",
  "printer_name", "general_device_name", "line_seq_device_name",
  "_line_adv_file", "_ext_clause", "assignment_name", "_assignment_name",
  "access_mode_clause", "access_mode", "alternative_record_key_clause",
  "_password_clause", "password_clause", "$@21", "_suppress_clause",
  "collating_sequence_clause", "collating_sequence", "$@22",
  "coll_sequence_values", "collating_sequence_clause_key", "alphabet_name",
  "file_status_clause", "_file_or_sort", "lock_mode_clause", "$@23",
  "lock_mode", "_lock_with", "_with_rollback", "with_rollback",
  "_with_mass_update", "organization_clause", "organization",
  "padding_character_clause", "record_delimiter_clause", "$@24",
  "record_delimiter_option", "record_key_clause", "_split_keys",
  "source_is", "split_key_list", "$@25", "split_key",
  "relative_key_clause", "reserve_clause", "no_or_integer",
  "sharing_clause", "sharing_option", "_i_o_control", "i_o_control_header",
  "_i_o_control_entries", "i_o_control_list", "i_o_control_clause",
  "same_clause", "_same_option", "apply_commit_clause",
  "multiple_file_tape_clause", "$@26", "multiple_file_list",
  "multiple_file", "_multiple_file_position", "_data_division", "$@27",
  "_data_division_header", "_file_section_header",
  "_file_description_sequence", "file_description",
  "file_description_entry", "$@28", "file_type",
  "_file_description_clause_sequence", "file_description_clause",
  "block_contains_clause", "_records_or_characters", "record_clause",
  "_record_depending", "_from_integer", "_to_integer",
  "label_records_clause", "value_of_clause", "file_id", "valueof_name",
  "data_records_clause", "linage_clause", "_linage_sequence",
  "linage_lines", "linage_footing", "linage_top", "linage_bottom",
  "recording_mode_clause", "recording_mode", "u_or_s", "code_set_clause",
  "_for_sub_records_clause", "report_clause", "report_keyword",
  "rep_name_list", "_communication_section", "$@29",
  "_communication_description_sequence", "communication_description",
  "communication_description_entry", "$@30",
  "_communication_description_clause_sequence",
  "communication_description_clause", "_input_cd_clauses",
  "named_input_cd_clauses", "named_input_cd_clause",
  "unnamed_input_cd_clauses", "_output_cd_clauses", "output_cd_clauses",
  "output_cd_clause", "_i_o_cd_clauses", "named_i_o_cd_clauses",
  "named_i_o_cd_clause", "unnamed_i_o_cd_clauses",
  "_working_storage_section", "$@31", "_record_description_list", "$@32",
  "record_description_list", "data_description", "$@33", "level_number",
  "_filler", "_entry_name", "user_entry_name", "_const_global",
  "lit_or_length", "con_source", "fp32_usage", "fp64_usage", "fp128_usage",
  "pointer_len", "renames_entry", "_renames_thru", "condition_name_entry",
  "$@34", "constant_entry", "$@35", "constant_source",
  "constant_78_source", "constant_expression_list", "constant_expression",
  "_data_description_clause_sequence", "data_description_clause_sequence",
  "data_description_clause", "redefines_clause", "external_clause",
  "_as_extname", "_global_clause", "global_clause", "volatile_clause",
  "picture_clause", "_pic_locale_format", "_is_locale_name", "locale_name",
  "usage_clause", "usage", "double_usage", "_font_name", "_layout_name",
  "sign_clause", "report_occurs_clause", "_occurs_step", "occurs_clause",
  "_occurs_to_integer", "_occurs_from_integer", "_occurs_integer_to",
  "_occurs_depending", "_capacity_in", "_occurs_initialized",
  "_occurs_keys_and_indexed", "$@36", "occurs_keys", "occurs_key_list",
  "occurs_key_field", "ascending_or_descending", "_occurs_indexed",
  "occurs_indexed", "occurs_index_list", "occurs_index",
  "justified_clause", "synchronized_clause", "_left_or_right",
  "blank_clause", "based_clause", "value_clause", "$@37",
  "value_item_list", "value_item", "_false_is", "any_length_clause",
  "external_form_clause", "identified_by_clause", "_local_storage_section",
  "$@38", "_linkage_section", "$@39", "_report_section", "$@40",
  "_report_description_sequence", "report_description", "$@41",
  "_report_description_options", "report_description_option",
  "control_clause", "control_field_list", "control_final_tag",
  "control_identifier_list", "control_identifier", "page_limit_clause",
  "page_line_column", "page_limit_cols", "report_int_ident",
  "_page_heading_list", "page_detail", "heading_clause", "first_detail",
  "last_heading", "last_detail", "footing_clause",
  "_report_group_description_list", "report_group_description_entry",
  "$@42", "_report_group_options", "report_group_option", "type_clause",
  "type_option", "_control_heading_final", "_or_page",
  "_control_footing_final", "next_group_clause", "next_group_plus",
  "next_page", "sum_clause_list", "_reset_clause", "data_or_final",
  "present_when_condition", "present_absent", "_page_or_id", "page_or_ids",
  "report_varying_clause", "line_clause", "line_keyword_clause",
  "_line_clause_options", "line_clause_option", "line_clause_integer",
  "column_clause", "col_keyword_clause", "_orientation",
  "_left_right_center", "col_or_plus", "column_integer_list",
  "column_integer", "source_clause", "group_indicate_clause",
  "_screen_section", "$@43", "_screen_description_list",
  "screen_description_list", "screen_description", "$@44", "$@45", "$@46",
  "_screen_options", "screen_option", "control_definition",
  "control_type_name", "control_type", "control_item",
  "_control_attributes", "control_attributes", "control_attribute",
  "control_style", "control_property", "control_style_name",
  "control_property_name", "control_style_name_generic",
  "control_property_name_generic", "control_style_name_label",
  "control_property_name_label", "control_style_name_entry_field",
  "control_property_name_entry_field", "control_style_name_push_button",
  "control_property_name_push_button", "control_style_name_check_box",
  "control_property_name_radio_button", "control_style_name_list_box",
  "control_property_name_list_box", "control_style_name_combo_box",
  "control_style_name_frame", "control_property_name_frame",
  "control_style_name_tab_control", "control_property_name_tab_control",
  "control_style_name_bar", "control_property_name_bar",
  "control_property_name_bitmap", "control_style_name_grid",
  "control_property_name_grid", "control_style_name_tree_view",
  "control_property_name_tree_view", "control_property_name_web_browser",
  "control_style_name_activex", "control_property_name_activex",
  "control_style_name_date_entry", "control_property_name_date_entry",
  "control_style_type", "control_property_type",
  "changeable_control_properties", "changeable_control_property",
  "changeable_window_properties", "changeable_window_property", "eol",
  "eos", "plus_plus", "minus_minus", "control_size", "control_size_unit",
  "_cell", "screen_line_number", "_screen_line_plus_minus",
  "screen_col_number", "_screen_col_plus_minus", "screen_occurs_clause",
  "screen_global_clause", "_procedure_division", "$@47", "$@48", "$@49",
  "$@50", "_procedure_using_chaining", "$@51", "$@52",
  "procedure_param_list", "procedure_param", "_procedure_type",
  "_size_optional", "size_is_integer", "_procedure_optional",
  "_procedure_returning", "_procedure_declaratives", "$@53",
  "_procedure_list", "procedure", "section_header", "$@54",
  "_use_statement", "paragraph_header", "invalid_statement", "_segment",
  "statement_list", "@55", "@56", "statements", "$@57", "statement",
  "accept_statement", "$@58", "accept_body", "$@59", "$@60",
  "accp_identifier", "field_with_pos_specifier", "$@61", "pos_specifier",
  "pos_specifier_value", "identifier_or_numeric_literal",
  "_accept_clauses", "accept_clauses", "accept_clause",
  "accept_from_screen_clauses", "accept_from_screen_clause",
  "lines_or_number", "at_line_column", "line_number", "column_number",
  "mode_is_block", "accp_attr", "_key_dest", "key_dest", "no_echo",
  "reverse_video", "update_default", "_end_accept", "add_statement",
  "$@62", "add_body", "_add_to", "_end_add", "allocate_statement", "$@63",
  "allocate_body", "_loc", "allocate_returning", "alter_statement", "$@64",
  "alter_body", "alter_entry", "_proceed_to", "call_statement", "$@65",
  "call_body", "$@66", "_mnemonic_conv", "program_or_prototype",
  "_id_or_lit_or_func_as", "nested_or_prototype", "call_using", "$@67",
  "call_param_list", "call_param", "_call_type", "call_returning",
  "return_give", "null_or_omitted", "call_exception_phrases",
  "_call_on_exception", "call_on_exception", "_call_not_on_exception",
  "call_not_on_exception", "_end_call", "cancel_statement", "$@68",
  "cancel_body", "id_or_lit_or_program_name", "close_statement", "$@69",
  "close_body", "close_files", "_close_option", "close_window", "$@70",
  "_close_display_option", "compute_statement", "$@71", "compute_body",
  "_end_compute", "commit_statement", "continue_statement",
  "destroy_statement", "$@72", "destroy_body", "delete_statement", "$@73",
  "delete_body", "delete_file_list", "_end_delete", "disable_statement",
  "$@74", "enable_disable_handling", "_enable_disable_key",
  "communication_mode", "display_statement", "$@75", "display_body",
  "screen_or_device_display", "display_list", "display_atom", "$@76",
  "disp_list", "_with_display_attr", "display_attrs", "display_clauses",
  "display_clause", "display_upon", "crt_under", "display_erase", "$@77",
  "display_pos_specifier", "field_or_literal_or_erase_with_pos_specifier",
  "$@78", "field_or_literal_or_erase_list", "field_or_literal_or_erase",
  "display_message_box", "$@79", "_display_message_clauses",
  "display_message_clauses", "display_message_clause", "display_window",
  "$@80", "$@81", "sub_or_window", "display_floating_window", "$@82",
  "$@83", "display_initial_window", "$@84", "initial_type", "_graphical",
  "_upon_window_handle", "window_handle", "display_window_clauses",
  "display_window_clause", "shadow", "boxed", "_top_or_bottom",
  "_left_or_centered_or_right", "no_scroll_wrap", "pop_up_or_handle",
  "pop_up_area", "handle_is_in", "disp_attr", "_end_display",
  "divide_statement", "$@85", "divide_body", "_end_divide",
  "enable_statement", "$@86", "entry_statement", "$@87", "entry_body",
  "evaluate_statement", "$@88", "evaluate_body", "evaluate_subject_list",
  "evaluate_subject", "evaluate_condition_list", "evaluate_case_list",
  "evaluate_case", "evaluate_other", "evaluate_when_list",
  "evaluate_object_list", "evaluate_object", "_evaluate_thru_expr",
  "_end_evaluate", "exit_statement", "$@89", "exit_body",
  "exit_program_returning", "free_statement", "$@90", "free_body",
  "generate_statement", "$@91", "generate_body", "goto_statement", "$@92",
  "go_body", "goto_depending", "goback_statement", "if_statement", "$@93",
  "if_else_statements", "_if_then", "if_true", "if_false", "_end_if",
  "initialize_statement", "$@94", "initialize_body", "_initialize_filler",
  "_initialize_value", "_initialize_replacing",
  "initialize_replacing_list", "initialize_replacing_item",
  "initialize_category", "_initialize_default", "initiate_statement",
  "$@95", "initiate_body", "inquire_statement", "$@96", "inquire_body",
  "inspect_statement", "$@97", "inspect_body", "send_identifier",
  "inspect_list", "inspect_tallying", "$@98", "inspect_replacing",
  "inspect_converting", "tallying_list", "tallying_item", "replacing_list",
  "replacing_item", "rep_keyword", "replacing_region", "inspect_region",
  "inspect_before", "inspect_after", "json_generate_statement", "$@99",
  "json_generate_body", "$@100", "$@101", "_json_suppress",
  "json_suppress_list", "json_suppress_entry", "_end_json",
  "json_parse_statement", "$@102", "json_parse_body", "_with_detail",
  "merge_statement", "$@103", "modify_statement", "$@104", "modify_body",
  "_end_modify", "move_statement", "$@105", "move_body",
  "multiply_statement", "$@106", "multiply_body", "_end_multiply",
  "open_statement", "$@107", "open_body", "open_file_entry",
  "_open_exclusive", "open_mode", "_open_sharing", "_open_option",
  "lock_allowing", "open_lock_option", "allowing_option", "allowing_all",
  "open_option_sequential", "perform_statement", "$@108", "perform_body",
  "$@109", "$@110", "_end_perform", "end_perform_or_dot",
  "perform_procedure", "_perform_option", "perform_test", "cond_or_exit",
  "perform_varying_list", "perform_varying", "_by_phrase",
  "purge_statement", "$@111", "raise_statement", "$@112", "raise_body",
  "exception_name", "read_statement", "$@113", "read_body", "_read_into",
  "_lock_phrases", "ignoring_lock", "advancing_lock_or_retry",
  "_retry_phrase", "retry_phrase", "retry_options", "_extended_with_lock",
  "extended_with_lock", "_read_key", "read_handler", "_end_read",
  "ready_statement", "receive_statement", "$@114", "receive_body",
  "message_or_segment", "_data_sentence_phrases", "_no_data_sentence",
  "no_data_sentence", "_with_data_sentence", "with_data_sentence",
  "_end_receive", "release_statement", "$@115", "release_body",
  "reset_statement", "return_statement", "$@116", "return_body",
  "_end_return", "rewrite_statement", "$@117", "rewrite_body",
  "_with_lock", "with_lock", "_end_rewrite", "rollback_statement",
  "search_statement", "$@118", "search_body", "search_varying",
  "search_at_end", "search_whens", "search_when", "_end_search",
  "send_statement", "$@119", "send_body", "_from_identifier",
  "from_identifier", "with_indicator", "_replacing_line", "set_statement",
  "$@120", "set_body", "on_or_off", "up_or_down", "set_environment",
  "set_attr", "set_attr_clause", "set_attr_one", "set_to", "set_up_down",
  "set_to_on_off_sequence", "set_to_on_off", "set_to_true_false_sequence",
  "set_to_true_false", "set_last_exception_to_off", "set_thread_priority",
  "sort_statement", "$@121", "sort_body", "@122", "sort_key_list",
  "_key_sort_list", "_sort_duplicates", "_sort_collating", "sort_input",
  "sort_output", "start_statement", "$@123", "start_body",
  "_sizelen_clause", "_start_key", "start_op", "disallowed_op",
  "not_equal_op", "_end_start", "stop_statement", "$@124",
  "stop_returning", "_status_x", "stop_argument", "stop_literal",
  "string_statement", "$@125", "string_body", "string_items", "$@126",
  "string_item_list", "string_item", "_string_delimited",
  "string_delimiter", "_with_pointer", "_end_string", "subtract_statement",
  "$@127", "subtract_body", "_end_subtract", "suppress_statement",
  "_printing", "terminate_statement", "$@128", "terminate_body",
  "transform_statement", "$@129", "transform_body", "unlock_statement",
  "$@130", "unlock_body", "unstring_statement", "$@131", "unstring_body",
  "_unstring_delimited", "unstring_delimited_list",
  "unstring_delimited_item", "unstring_into", "unstring_into_item",
  "_unstring_into_delimiter", "_unstring_tallying", "_end_unstring",
  "validate_statement", "$@132", "validate_fields", "use_statement",
  "$@133", "use_phrase", "use_file_exception", "use_global",
  "use_file_exception_target", "use_debugging", "debugging_list",
  "debugging_target", "_all_refs", "use_start_end", "program_start_end",
  "use_reporting", "use_exception_list", "use_exception", "use_ex_keyw",
  "write_statement", "$@134", "write_body", "from_option", "write_option",
  "before_or_after", "write_handler", "_end_write",
  "xml_generate_statement", "$@135", "xml_generate_body", "$@136", "$@137",
  "_with_encoding_xml_dec_and_attrs", "with_encoding_xml_dec_and_attrs",
  "with_encoding_xml_dec_and_attr", "encoding_xml_dec_and_attr",
  "_xml_gen_namespace", "_xml_gen_namespace_prefix", "_name_of",
  "identifier_name_list", "identifier_is_name", "_type_of",
  "identifier_type_list", "identifier_is_type", "_xml_type", "ml_type",
  "_xml_gen_suppress", "xml_suppress_list", "xml_suppress_entry",
  "xml_suppress_generic_opt", "xml_suppress_when_list", "_end_xml",
  "xml_parse_statement", "$@138", "xml_parse_body", "$@139",
  "_accept_exception_phrases", "_accp_on_exception", "accp_on_exception",
  "escape_or_exception", "_accp_not_on_exception", "accp_not_on_exception",
  "not_escape_or_not_exception", "_display_exception_phrases",
  "_disp_on_exception", "disp_on_exception", "_disp_not_on_exception",
  "disp_not_on_exception", "_xml_exception_phrases", "_xml_on_exception",
  "xml_on_exception", "_xml_not_on_exception", "xml_not_on_exception",
  "_json_exception_phrases", "_json_on_exception", "json_on_exception",
  "_json_not_on_exception", "json_not_on_exception",
  "on_size_error_phrases", "_on_size_error", "on_size_error",
  "_not_on_size_error", "not_on_size_error", "_on_overflow_phrases",
  "_on_overflow", "on_overflow", "_not_on_overflow", "not_on_overflow",
  "return_at_end", "at_end", "_at_end_clause", "at_end_clause",
  "_not_at_end_clause", "not_at_end_clause", "at_eop_clauses",
  "_at_eop_clause", "at_eop_clause", "_not_at_eop_clause",
  "not_at_eop_clause", "_invalid_key_phrases", "invalid_key_phrases",
  "_invalid_key_sentence", "invalid_key_sentence",
  "_not_invalid_key_sentence", "not_invalid_key_sentence", "_thread_start",
  "_thread_handle", "thread_reference_optional", "_scroll_lines",
  "_count_in", "condition", "expr", "partial_expr", "$@140", "expr_tokens",
  "expr_token", "_not_expr", "not_expr", "condition_or_class", "eq", "gt",
  "lt", "ge", "le", "exp_list", "_e_sep", "exp", "exp_term", "exp_factor",
  "exp_unary", "exp_atom", "line_linage_page_counter", "arithmetic_x_list",
  "arithmetic_x", "record_name", "file_or_record_name", "table_name",
  "file_name_list", "file_file_name_list", "file_name", "cd_name",
  "report_name", "mnemonic_name_list", "mnemonic_name",
  "procedure_name_list", "procedure_name", "label", "integer_label",
  "reference_list", "reference", "_reference", "single_reference_list",
  "single_reference", "optional_reference_list", "optional_reference",
  "reference_or_literal", "undefined_word", "unique_word", "target_x_list",
  "target_x", "_x_list", "x_list", "x", "call_x", "x_common",
  "length_of_register", "report_x_list", "expr_x", "arith_x",
  "arith_nonzero_x", "numeric_literal", "non_numeric_literal",
  "nonzero_numeric_literal", "prog_or_entry", "alnum_or_id",
  "simple_display_value", "simple_display_all_value", "inspect_from",
  "inspect_to", "simple_value", "simple_all_value", "id_or_lit",
  "id_or_lit_or_func", "id_or_lit_or_length_or_func", "num_id_or_lit",
  "positive_id_or_lit", "pos_num_id_or_lit_or_zero", "pos_num_id_or_lit",
  "from_parameter", "sub_identifier", "table_identifier",
  "sub_identifier_1", "display_identifier", "numeric_identifier",
  "identifier_or_file_name", "identifier", "identifier_1",
  "identifier_list", "target_identifier", "target_identifier_1",
  "display_identifier_or_alphabet_name", "qualified_word",
  "unqualified_word", "$@141", "unqualified_word_check", "subref",
  "refmod", "integer", "symbolic_integer", "unsigned_pos_integer",
  "report_integer", "class_value", "literal", "basic_literal",
  "basic_value", "zero_spaces_high_low_values", "function", "func_no_parm",
  "func_one_parm", "func_multi_parm", "func_refmod", "func_args",
  "trim_args", "length_arg", "$@142", "numvalc_args", "locale_dt_args",
  "formatted_datetime_args", "formatted_time_args", "not_const_word",
  "flag_all", "flag_duplicates", "flag_initialized", "flag_initialized_to",
  "to_init_val", "_flag_next", "_flag_not", "flag_optional",
  "flag_rounded", "round_mode", "round_choice", "flag_separate",
  "_from_idx_to_idx", "_dest_index", "error_stmt_recover", "verb",
  "scope_terminator", "_advancing", "_after", "_are", "_area", "_areas",
  "_as", "_at", "_before", "_binary", "_box", "_by", "_character",
  "_characters", "_collating", "_contains", "_controls", "_control",
  "_data", "_end_of", "_file", "_for", "_from", "_in", "_in_equal",
  "_in_order", "_index", "_indicate", "_initial", "_into", "_is",
  "_is_equal", "_is_are", "_is_are_equal", "_is_in", "_key", "_line",
  "_line_or_lines", "_limits", "_lines", "_lock", "_message", "_mode",
  "_new", "_number", "_number_is", "_numbers", "_of", "_on", "_on_for",
  "_onoff_status", "_other", "_others", "_procedure", "_program",
  "_protected", "_record", "_records", "_right", "_sign", "_signed",
  "_sign_is", "_size", "_standard", "_status", "_symbolic", "_tape",
  "_terminal", "_then", "_times", "_to", "_to_using", "_when",
  "_when_set_to", "_with", "_with_for", "column_or_col", "columns_or_cols",
  "column_or_cols", "column_or_col_or_position_or_pos", "comp_equal",
  "exception_or_error", "in_of", "label_option", "line_or_lines",
  "lock_records", "object_char_or_word_or_modules", "records",
  "reel_or_unit", "size_or_length", "detail_keyword", "ch_keyword",
  "cf_keyword", "ph_keyword", "pf_keyword", "rh_keyword", "rf_keyword",
  "control_keyword", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   570,   571,   572,   573,   574,
     575,   576,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   588,   589,   590,   591,   592,   593,   594,
     595,   596,   597,   598,   599,   600,   601,   602,   603,   604,
     605,   606,   607,   608,   609,   610,   611,   612,   613,   614,
     615,   616,   617,   618,   619,   620,   621,   622,   623,   624,
     625,   626,   627,   628,   629,   630,   631,   632,   633,   634,
     635,   636,   637,   638,   639,   640,   641,   642,   643,   644,
     645,   646,   647,   648,   649,   650,   651,   652,   653,   654,
     655,   656,   657,   658,   659,   660,   661,   662,   663,   664,
     665,   666,   667,   668,   669,   670,   671,   672,   673,   674,
     675,   676,   677,   678,   679,   680,   681,   682,   683,   684,
     685,   686,   687,   688,   689,   690,   691,   692,   693,   694,
     695,   696,   697,   698,   699,   700,   701,   702,   703,   704,
     705,   706,   707,   708,   709,   710,   711,   712,   713,   714,
     715,   716,   717,   718,   719,   720,   721,   722,   723,   724,
     725,   726,   727,   728,   729,   730,   731,   732,   733,   734,
     735,   736,   737,   738,   739,   740,   741,   742,   743,   744,
     745,   746,   747,   748,   749,   750,   751,   752,   753,   754,
     755,   756,   757,   758,   759,   760,   761,   762,   763,   764,
     765,   766,   767,   768,   769,   770,   771,   772,   773,   774,
     775,   776,   777,   778,   779,   780,   781,   782,   783,   784,
     785,   786,   787,   788,   789,   790,   791,   792,   793,   794,
     795,   796,   797,   798,   799,   800,   801,   802,   803,   804,
     805,   806,   807,   808,   809,   810,   811,   812,   813,   814,
     815,   816,   817,   818,   819,   820,   821,   822,   823,   824,
     825,   826,   827,   828,   829,   830,   831,   832,   833,   834,
     835,   836,   837,   838,   839,   840,   841,   842,   843,   844,
     845,   846,   847,   848,   849,   850,   851,   852,   853,   854,
     855,   856,   857,   858,   859,   860,   861,   862,   863,   864,
     865,   866,   867,   868,   869,   870,   871,   872,   873,   874,
     875,   876,   877,   878,   879,   880,   881,   882,   883,   884,
     885,   886,   887,   888,   889,   890,   891,   892,   893,   894,
     895,   896,   897,   898,   899,   900,   901,   902,   903,   904,
     905,   906,   907,   908,   909,   910,   911,   912,   913,   914,
     915,   916,   917,   918,   919,   920,   921,   922,   923,   924,
     925,   926,   927,   928,   929,   930,   931,   932,   933,   934,
     935,   936,   937,   938,   939,   940,   941,   942,   943,   944,
     945,   946,   947,   948,   949,   950,   951,   952,   953,   954,
     955,   956,   957,   958,   959,   960,   961,   962,   963,   964,
     965,   966,   967,   968,   969,   970,   971,   972,   973,   974,
     975,   976,   977,   978,   979,   980,   981,   982,   983,   984,
     985,   986,   987,   988,   989,   990,   991,   992,   993,   994,
     995,   996,   997,   998,   999,  1000,  1001,  1002,  1003,  1004,
    1005,  1006,  1007,  1008,  1009,  1010,  1011,  1012,  1013,  1014,
    1015,  1016,  1017,  1018,  1019,  1020,  1021,  1022,  1023,  1024,
    1025,  1026,  1027,  1028,  1029,  1030,  1031,  1032,  1033,  1034,
    1035,  1036,  1037,  1038,  1039,  1040,  1041,  1042,  1043,  1044,
    1045,  1046,  1047,  1048,  1049,  1050,  1051,  1052,  1053,  1054,
    1055,  1056,  1057,  1058,  1059,  1060,  1061,  1062,  1063,  1064,
    1065,  1066,  1067,  1068,  1069,  1070,  1071,  1072,  1073,  1074,
    1075,  1076,  1077,  1078,  1079,  1080,  1081,  1082,  1083,  1084,
    1085,  1086,  1087,  1088,  1089,  1090,  1091,  1092,  1093,  1094,
    1095,  1096,  1097,  1098,  1099,  1100,  1101,  1102,  1103,  1104,
    1105,  1106,  1107,  1108,  1109,  1110,  1111,  1112,  1113,  1114,
    1115,  1116,  1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  1129,  1130,  1131,  1132,  1133,  1134,
    1135,  1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,  1144,
    1145,  1146,  1147,  1148,  1149,  1150,  1151
};
# endif

#define YYPACT_NINF -3695

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-3695)))

#define YYTABLE_NINF -3035

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -3695,   416,   -28, -3695, -3695, -3695,  1480, -3695,   157, -3695,
   -3695,  1197, -3695, -3695, -3695,    -2,   220,  1326, -3695, -3695,
    1518, -3695, -3695, -3695,  1101,  1101,   924,  1116,  1725,  1233,
    1554,  1236,  1165,  1192,  1216,   157,   157, -3695, -3695,  1810,
   -3695,  1245,  1235,  1370, -3695,  1796,    92,    92,  1369,  1383,
    1732,  1732,  1732,    92,  1403,  1348,  1354,  1732,  1359,  1367,
    1141, -3695, -3695,  1236, -3695, -3695, -3695, -3695, -3695, -3695,
    1576, -3695, -3695, -3695, -3695,  1790, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,   -54,   -54,  2142,  1958,
    1968,  1421,  1111,  1548, -3695, -3695,  1445,  1446, -3695, -3695,
   -3695, -3695,   264,  1732, -3695,  1732, -3695,  1362,  2126,  1362,
    1732,  1732, -3695, -3695,  1362, -3695, -3695, -3695,  1388,  1389,
    1372, -3695, -3695, -3695,  1395, -3695, -3695, -3695,  2243,  2243,
    1732, -3695,  2091, -3695, -3695,  1958, -3695, -3695, -3695, -3695,
    2072, -3695, -3695,  1493,   837, -3695, -3695,   -99,   264, -3695,
    1732,   231,  1362,  1847,    -8, -3695, -3695, -3695, -3695,  1849,
    1478,    85,   877, -3695,  1411, -3695,  1388, -3695,  1732, -3695,
    1389, -3695,    81, -3695, -3695, -3695, -3695, -3695, -3695,   840,
    -101,  1732,    94, -3695,  1860,  1508, -3695,   906,  1642,  2050,
     -51, -3695,   -51, -3695,  7747,  8306, -3695, -3695, -3695,  1434,
   -3695,   849,    95,  1523,  -114, -3695, -3695,   256, -3695, -3695,
   -3695,   446,  1281, -3695, -3695, -3695,   568, -3695, -3695,  1362,
   -3695,  1619, -3695,  1603, -3695,  1732, -3695, -3695,   248, -3695,
   -3695, -3695, -3695, -3695,    43,  2215,  2207,   104,  1447, -3695,
     108, -3695, -3695,   -43, -3695,    97, -3695, -3695, -3695, -3695,
    1892,  -101, -3695,  1943,    92,    92, -3695,   840, -3695, -3695,
     754, -3695, -3695, -3695, -3695, -3695,  1870,  1732,  1960, -3695,
   -3695,  1559,  1561,  -192,  8608, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695,   -68, -3695, -3695,
   -3695, -3695, -3695,   -13, -3695, -3695, -3695, -3695,  1658, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,  1432, -3695, -3695,  1769,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695,    -7, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,  1672,  2227, -3695,    36,  1571, -3695, -3695,
   -3695,  1826, -3695,    92,  2238, -3695,   670, -3695,   150, -3695,
   -3695, -3695, -3695,  1732,  1732, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,  1528, -3695,  1592, -3695, -3695,  1802,
   -3695, -3695, -3695,  1732,  1935, -3695, -3695, -3695, -3695,   650,
    1732, -3695, -3695,  1679,  1989, -3695,  2243,  1285,  2243,  1590,
   -3695, -3695,  1593,  2198,  1433, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,  1601, -3695, -3695,  1892, -3695,    92, -3695,
   -3695, -3695, -3695, -3695,  1600,    35, -3695,  1732,    54,  1731,
    1605, -3695, -3695, -3695, -3695, -3695, -3695,    13, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695,  -243,  9674, 16547,
    -183,  -192,   691,   989,   296,   448,    76,   799,  9045, 10674,
     799,  -192,  -386,  1423,   296,  1362,  1611, -3695, -3695, 10674,
   -3695,  5980,   296,  1520,   -15,  7319, -3695, -3695,  1362,   -15,
   10042, 10674, -3695,  2116,  -130,  1524,  -103,  1532,  1524,  1362,
    1532,   538,   100,  1524,   400,  1362,  1532, -3695, -3695, -3695,
   -3695,  1362, -3695, -3695, -3695, -3695, -3695, -3695,  1612, -3695,
    9885, -3695, -3695,  1520,   101,  1362,  1532,  4438,  1362,   538,
   -3695, -3695,  1629,  1738,  1984,  1388,  1388,  1388,   874,  1641,
   13370, -3695, -3695, -3695,  2009, -3695, -3695, -3695, -3695,  1896,
    1643, -3695, -3695, -3695,  2410,  1649, -3695, -3695,   724,  1949,
    1566, -3695, -3695,  1281,  2010,  1935, -3695, -3695,  -219, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  1978, -3695,
    1433, -3695, -3695, -3695,  -281, -3695, -3695, -3695,  1859, -3695,
    2328,   608, -3695, -3695, -3695, -3695,  1732, -3695, -3695, -3695,
    1812, -3695,  1298,  2233, -3695, 14280,  1662,  2004,  2140,  1939,
   -3695, -3695, -3695,  1362, -3695, -3695,  1673,  1675,  1676,  1939,
    1677,    -8,    -8,  1681,  1683,  1684, -3695, -3695,  1686,    -8,
   -3695, -3695, -3695,  1362,  1692, -3695,  1676, -3695,  2265, -3695,
   10229, -3695, -3695, 12140, -3695, -3695, -3695,  1696,  1697,  1698,
   15407, 16547, 15407, -3695,    52,  1115, -3695,  2216, -3695, -3695,
   12140, -3695,   356,  1601, -3695, -3695,  -183, -3695,  1717, -3695,
      -8, -3695,  2272,  -130, -3695, -3695,   691, -3695, -3695, -3695,
   -3695, -3695,  1532, -3695,  1173,  1939,  2274, -3695,   221, -3695,
    1845, -3695, -3695,  1612,  1601,  1532,  2275,  1895,  2358, -3695,
   -3695,  1362,  1746,  1747, -3695, -3695, -3695,  1524, -3695,  2173,
   -3695,  1409,  2449, -3695, -3695, -3695, -3695, -3695,  2287,    31,
   10299, -3695, -3695, -3695, -3695,  1646,  1662, -3695, -3695, -3695,
   -3695, -3695,  2173,  8975,  1424,  1497,  2290,   228, -3695,  2090,
   -3695, -3695, -3695,  2293,    74, -3695, -3695, -3695,  4104, -3695,
   -3695,  2359,   -68, -3695, -3695, -3695,   296, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695,  1758, -3695, -3695,   495, -3695,
    1520, -3695, -3695,  1362, 12097,   907, -3695,   968, -3695,   167,
   -3695, -3695, -3695,  1362,  1362, -3695, -3695, -3695,  1733, 11370,
     907,  2298, 10674, -3695,  1749,  2299,  2463, -3695,  1022, -3695,
    1441, -3695, -3695,  5648,  1767, -3695, -3695,  1668, -3695, -3695,
    2303,   748,  2307,  -136, -3695,  2214, -3695,  2302,  1895,  1678,
    2317, -3695,  2214,  1362,  2322,  1707, -3695, -3695,  2244, 12140,
    2294, -3695, -3695, -3695, -3695, -3695, -3695,  2110, -3695,   296,
   -3695, -3695, -3695,  1992,  -188, -3695,   429,  2543, -3695,    88,
   -3695,  2330,    75,  9440, -3695, 16547,  1779, -3695,  2333,  2196,
   10674,  1362,  1362,  2334, 10487,  1520, -3695, -3695,   555, -3695,
   -3695, -3695, -3695,  7905, -3695,  2257, -3695, -3695,   774, -3695,
    2337,  2400, -3695, -3695,  1362, -3695,  2339,  2214,  1362,  1362,
   -3695,  1804,  1911,  2163, -3695, -3695,  1970,  1811, -3695,  1815,
   -3695, -3695, -3695,  2459, -3695,  1222,  3018, -3695,  1362, -3695,
    1844, -3695,  2291, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  1565, -3695,
     106, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
      38, -3695, -3695, -3695, -3695, -3695, -3695,  1735,  -125, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,   769,    79,    79,   609,  1821, -3695, -3695,
   14280,  -230, -3695, -3695,  1732, -3695, 14280, -3695, -3695, -3695,
    1076,  1834, 15765,    22,  1362, -3695,  2455,  2679, -3695,   -80,
    1833, 16547, 16547, 15853, -3695, -3695, -3695,  1744,  1745, 16547,
   16547, 16547, 12140,  1748,  1840, 12140, -3695, -3695, -3695, 10861,
    2289, -3695, -3695,  1601, -3695, 16547, -3695, 12140, 16547, -3695,
   -3695,  1159, -3695,  2248, 16547, 16547, 16547, 16547, 16547, -3695,
    1601, -3695, -3695,  2190, -3695,  2041,  2194, -3695, -3695,  4438,
   -3695,  1362,  1173, -3695, -3695, -3695,  1296,   -22,  1362, -3695,
   -3695, -3695, -3695, -3695, 16547,  2165, -3695,  1779, -3695,  1532,
   -3695, -3695, -3695, -3695,  1991, -3695, -3695, -3695, -3695, -3695,
   -3695,  -159,  1646, -3695,  1760, -3695, 10674, -3695, -3695, -3695,
   -3695, -3695,  2119,  2361, -3695, -3695,  8975,   249,  4003, -3695,
    1033,  1807,  1772,    31,    31,    31,    31, -3695, -3695, 10674,
   10861,  1794, -3695, -3695,  -386,  5041, -3695,  1778, -3695,   -70,
   -3695, -3695,   625, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695,  3871, -3695, -3695, -3695,  1952, -3695, -3695, -3695,   -40,
   -3695,  2427,  2374,  2354, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
     727, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,  1935, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,   727, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, 12097, -3695,   727,   727,   727,   907, -3695,   905,   118,
   -3695, -3695,  2020, -3695, -3695,  2416,  2329,  2416,  2269,    71,
   16547, -3695, -3695,   728,  6629, -3695, -3695,   117, 13756,   907,
   -3695, -3695,  1876,   296, -3695, -3695, 10861, -3695, -3695, -3695,
   -3695, -3695,  1959,  1939, -3695,  1939,  1127, -3695,  2312,  2312,
    -429,  1888,  1885, -3695,   922, -3695, -3695,  1898, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695,  1895, -3695, -3695, -3695, -3695,
    2281,  7319, -3695, -3695, -3695,  2282, -3695, -3695, -3695,  1991,
    2446, -3695, -3695,  1362,  2446,  1362,  1803,    -6,  1894, -3695,
   -3695,  1601, -3695,  1897, -3695, -3695,   757,  1902,   840, -3695,
   -3695,  9641, -3695,  2604,   266,   124, -3695, -3695, -3695,  1732,
   -3695,   654, 10674, -3695, -3695,    55,    56,   940, 16547, -3695,
   -3695, -3695,  1362, 10674, -3695,  2498,  2362,  2363, -3695, -3695,
   10861, -3695, -3695, -3695, -3695, 12140, -3695, -3695, -3695, -3695,
   -3695,  2617,  2306, -3695, -3695, -3695,   390,  2441,  2367,  2441,
    2108,   849, -3695,  1909,  2016,  2069,  1855,  1500,  1362, -3695,
   -3695, 14626,  1500,  2561,  1732,  1318,  1318,  1732,     1,  1870,
    1732,  2686, -3695,  2178, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,    92,   133,  1923, -3695,  3352,  1362,
   -3695, -3695,  2561,  1532, -3695, -3695, -3695,  1565, -3695, -3695,
   -3695, -3695,    -8, -3695, -3695,   474, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,    39, -3695,  -345,  -287,   161, -3695, -3695,
   -3695,  1546, -3695, -3695, -3695, -3695,  2644, -3695, -3695, -3695,
    2189, -3695, -3695, -3695, -3695,  2189,    86, -3695,  1732, -3695,
   -3695, -3695, -3695, -3695,  1732, -3695, -3695, -3695,  1732, -3695,
   -3695, -3695, -3695, -3695,    87, -3695, -3695, -3695,  2615, -3695,
   -3695, -3695, -3695, -3695, -3695,   -41, -3695, -3695, -3695,  2709,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695,  2316,  2007,   183,
   -3695,  2594,   928, -3695, -3695, -3695, -3695, -3695,  1546, -3695,
   -3695, -3695,  1835,  1837, -3695, 12140,  1546,  2323,  1990,  1993,
    2205, -3695, -3695, -3695, -3695, -3695,  2258, -3695, -3695, -3695,
   -3695, -3695,  1939,  1939, -3695,   665, -3695,  1362,   122,  1180,
    1945,   149,  1947, -3695,   227,   651, 12140, -3695, -3695,   335,
    1948,  1951,  1957,   337, -3695,  1601, -3695,  1961, -3695,  1362,
     344,  1963,  1939,  2398,  1054, -3695,   140,   544,   296,  1385,
    1965,   361, -3695,  1954,  2190,  1115,  1115, -3695, -3695, -3695,
    1935,  2097,  1969,  -183, -3695, -3695,   778,  2722,   623, -3695,
   -3695,  2103,  2128, -3695,   914,  1732, -3695, -3695, -3695,  1719,
     -25, -3695, -3695, -3695,  2353, -3695, -3695, 10674, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695,   175, -3695, -3695,  5489, -3695,
   -3695,  5383,   728, -3695, -3695, -3695, -3695,  -100, -3695,  1732,
   -3695,   -26,   728, -3695, -3695, -3695, -3695,   -34,  1732, -3695,
   -3695, -3695,  4003, -3695, -3695,  1033, -3695, -3695,  1601,  1362,
   -3695, -3695, -3695, -3695, -3695, -3695,  2424,  1054,  2425, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,  2745, -3695,  1988,  4970,
   -3695, -3695, -3695, -3695,  5041, -3695,  1879,  1952, -3695, -3695,
   -3695, -3695, -3695,  1611, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,  1896, -3695, -3695, -3695,  2527, -3695,  1611, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,  2135,  1611, -3695, -3695,
   -3695,  1362, -3695, -3695,  1362, -3695,  1362,  1362,  1362, -3695,
    1986, -3695, -3695,  1601, -3695,  2696, -3695, -3695, -3695,   551,
   -3695,   905,  4368, -3695, -3695, -3695,  1362, -3695,  1362,    63,
     311,  2560, -3695, -3695,   680, -3695, -3695, -3695, -3695, 10674,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,   296,   296,  1054,  2438,  1803,  1991,  2030,
    2030,  2406, 11479,    16,  5585,  1362,  -183, -3695,   390,  2282,
    1362, -3695, -3695, -3695, -3695,  1362,   943,   723, -3695,  1910,
   -3695,  1913, -3695,   390,   -33, 12140,  2247,  1070,   185, -3695,
     665,  2253, -3695, -3695, -3695, 10674,   840,   840,   840,   840,
     840,   840,   840,   840,   266, -3695,   -52,   -25,   -94, -3695,
    2053,  2053, -3695, -3695, -3695, 16547, 15915,   940,  -269, -3695,
    2617, -3695,  1362,  1362,  1054,  2450,  1935,  2006,  2783,  1362,
     703, -3695, -3695,  1991,  2787, -3695, -3695,  1362, -3695,  2208,
   -3695, -3695, -3695,  2008,  2112,  2127,  1145, -3695,  1950, -3695,
    2464,  2031,     2, -3695, -3695,  -339,  -326,   528,   638,   648,
   -3695,  1938, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
    3528, -3695,  2152, -3695,  1030, -3695,  2371, -3695, -3695,  1362,
    2419, -3695, -3695, -3695,   785, -3695, -3695, -3695,  1732, -3695,
   -3695, 14833, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695,  1806,  -157,  1148,  2474, -3695,  1935,  1566,  1145,  1145,
    1936,   676,   681,  1935,  1956,  1732, -3695, -3695, -3695,   -97,
    1900, -3695, -3695, -3695, -3695,  1870,  1895,  -398, -3695,  2117,
    1732,  2717,   234,   -93,  1407,  1803, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695,  2083, -3695,  2379,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  2130,  2775,
   -3695,  1532,  1532, -3695,  1565, -3695,  1735,  1732,  1732,  1953,
   -3695,  1732,  2114,  2292, -3695, -3695, -3695,  2651, -3695, -3695,
   -3695, -3695, -3695,  1666,  1362,  1149, -3695, -3695, -3695,  -230,
    -230, -3695, -3695, -3695, -3695,  -230,  -230,  -230, -3695, -3695,
   -3695,  1732,   680,   680, -3695,    86,  2406,  1732,  2054,   667,
    2401,  2401, -3695, -3695, -3695,  1546, -3695, -3695, -3695, -3695,
   -3695, -3695,   110, -3695,  1532,  1532, -3695, -3695, -3695,  1845,
   16053,  1696, 16347,  1696, -3695,  2055, -3695, -3695,  1362,  1696,
    1696,  1696, 12140, -3695,  1845,   742,  1696,   -80, -3695, -3695,
   -3695,  2326,  2124,   199,  2508,  1054, 16485,  1696,  1696,    84,
   -3695,  2097, -3695,   296, -3695, -3695, -3695,  2312, -3695, -3695,
   -3695, -3695, -3695,  2366, -3695, -3695, -3695,   608, -3695, 16547,
   -3695, -3695, -3695, -3695,  2335,  2470,   698,  1807,   752, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  -230,
   -3695, -3695,  -230, -3695, -3695, -3695, -3695,   819,  2627,  -230,
     680,   680,  -230, -3695, -3695, -3695,  2706,  2706,   296, -3695,
     296,    57,  5041, -3695, -3695,   -46,  2745, -3695, -3695, -3695,
    1362, -3695, -3695, -3695,  2639,  2011,  1244,    -5,  2012, -3695,
   -3695, -3695, -3695, -3695,    84, 12140, -3695, -3695,  2788, -3695,
    1659, -3695, -3695,  4368, -3695,  1659,  2547,  2548,  2711,   -50,
   -3695, -3695,  2174, -3695, -3695,  2323,   932, -3695, -3695, -3695,
   10674,   296, -3695,   296,   145,  1532,  2084, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,  1362, -3695, -3695,    26, -3695, -3695, -3695,
    2865, -3695,  2545, -3695, -3695,   120,  1001, -3695, -3695, -3695,
   -3695,  2360,  2649,   -25, -3695,  1353, -3695, -3695,  5980, -3695,
    1913,  2245, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, 10674, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695,   -44, -3695,  1362, -3695, -3695,
   -3695,  1164, -3695, -3695, -3695, 16547, -3695, 10674, 10674,  1180,
   -3695,  1205,  -218,  2309, 10523,  1845,  1845, -3695,   296,  2088,
   -3695,    84, -3695,  2336, -3695, 12140, -3695,  2690,  2121, -3695,
     723, -3695,   876,  2711,  1732,  2798,   849, -3695,  2099,  2203,
   -3695,  2045,  1732, -3695,  1362, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
    2000,  3580, -3695, -3695,  2808,  2074,  2111,   -66, -3695, -3695,
   -3695, -3695, -3695, -3695,  5908, -3695,  2864, -3695,  2617,  2201,
    2201, -3695,  2021, -3695,  2045, -3695,  2115,  2583, -3695, -3695,
   -3695,  1936, -3695, -3695, -3695, -3695, -3695, -3695,  2472,    59,
    2406,   442,  1732, -3695, -3695,  1732, -3695,  1732,  2323, -3695,
   -3695,   163, -3695,  1355,  1732,  1732,  1732,  1732, -3695,  2005,
   -3695,   145,  1732,  1870,  2372, -3695,  2210,  1532, -3695,  2338,
   -3695,  2891, -3695, -3695, -3695, -3695, -3695,   244,  1732, -3695,
   -3695,  2033,  2122, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,   698, -3695,
    1543, -3695, -3695,  1362,   185, -3695, -3695,  2132, -3695, -3695,
   -3695,  1732,   659, -3695, -3695, -3695, -3695,   708, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  2585, -3695,
   -3695, -3695,  2579, -3695, -3695, -3695, -3695, -3695, -3695,  2582,
   -3695, -3695,  1262, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
    1794,  2721, -3695,  1061, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,   728,   728,   728,   728, 10674, -3695,   752, -3695,
    7346, -3695, -3695, -3695, -3695,  1939, -3695, -3695, -3695, -3695,
   -3695, -3695,  1456,  -230,  2410, -3695, -3695, -3695, 12272, -3695,
   -3695, -3695,   216, -3695, -3695, -3695, -3695,  5630, 12272,  1054,
    2296,  1054,  2308,  9307, -3695,  -250,   -14, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,  1244, -3695,  2747, -3695,
   -3695,  1611, -3695,  1659, -3695, -3695,  1659,    84,  2129,  2129,
   -3695,  2916,  2881, -3695, -3695, -3695,  2406, -3695,  2462,  2741,
     -67, -3695, -3695,  2396, -3695, -3695,  1054,  2399,  2399,  2403,
   -3695,   131, -3695, -3695,  2703, -3695, -3695, -3695,  1362, 10674,
    1896,  2504,  2549, -3695,   763, -3695, -3695, -3695,   733, -3695,
   -3695, -3695,  2776,  2457, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695,  2512, -3695, -3695, -3695,  2531, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,  1180, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,  2439,  2161,  1732, -3695, -3695, -3695,   708,  2585,
    1054,  2123, -3695, -3695,  2783, -3695,  2406,  2711,  2406,  -218,
    1242, -3695, -3695,  1670, -3695,  -183,    92, -3695,   849, -3695,
     849, -3695,  2176, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,  2192, -3695,  2045, -3695,   947, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,   952, -3695, -3695,  2532,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,  2406,  2643,  2193,  1935,  2193,  2259,
    2098, -3695, -3695, -3695,  2541, -3695,   698,  2717, -3695, -3695,
   -3695,  2045,  1935,   257,  1362, -3695, -3695, -3695, -3695,  1935,
   -3695,  1603, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
     707,   707,   464,  1732, -3695, -3695,  1347, -3695, -3695,   681,
   -3695,  1362,  1362, -3695, -3695, -3695, -3695,  1362,  1732,  1362,
    -125, -3695, -3695,  1935, -3695,  1565,    51,   304, -3695, -3695,
   -3695,   189, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,   185, -3695, -3695,  2606,  2785, -3695,   -68,
   -3695, -3695, -3695,  2706, 10674, 10674, 10674, 10674, -3695, -3695,
   -3695, -3695, -3695,  1362, -3695,   728, -3695, -3695, -3695, -3695,
    2202,   593, -3695, -3695,   296, -3695,   296, -3695, -3695, 10674,
   -3695, -3695, -3695, -3695, -3695, -3695,  2909,  2800, -3695, -3695,
    1659, -3695, 10674, 10674, -3695, -3695,  1362,  2462,  1939,  2234,
   -3695,  2402,  1532,   902,  1362, -3695, -3695, -3695, -3695, -3695,
   -3695,    73, -3695, -3695,  2497, -3695,   619, -3695,  2503,  1161,
   -3695, -3695, -3695,  2914,  2552, -3695,  1732,  1308, -3695, -3695,
      80,  2553,  2554, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695,  1362, -3695,  2785, -3695, -3695, -3695,  2217, -3695,  1362,
   -3695,  1362, -3695, -3695, -3695, -3695, -3695,  2487,  2742, -3695,
   -3695, -3695, -3695,   125, -3695, -3695, -3695, -3695, -3695, -3695,
     235,  -416, -3695, -3695,  1939, -3695, -3695,  2045, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  2131,  1935,
    2225, -3695,  2818, -3695,  2822,  1732, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695,  1362,    27,  2915,    58, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695,  1362, -3695, -3695,
   -3695, -3695, -3695,   713, -3695, -3695,   713,  3009, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695,   713,   713,   713,   721, -3695,
      65,   660, -3695,  1362,  1110,  1732, -3695,   539,  1566, -3695,
   -3695,  2544,  3004, -3695, -3695,   113, -3695, -3695,   185,  2606,
     708,    67,   229, 12272, -3695, -3695, -3695, -3695, -3695,  1362,
     728, -3695, -3695,   708,   708, -3695, -3695, -3695, -3695, -3695,
   10674, -3695, -3695, -3695, -3695, -3695,  2234,  1362,  1362,  1035,
    1732,  1532,  1532,  2436, -3695, -3695, -3695,  2477, -3695, -3695,
   -3695, -3695, -3695, -3695,  2376, -3695, -3695, -3695, -3695,  7901,
    2195, -3695,  1362, -3695, -3695,  2360,  2649, -3695, -3695, -3695,
   -3695,   708,  1604, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,  2556,   125, -3695,   127,  1128, -3695,  2418,  -106,
   -3695,  2340, -3695,  2240,  1362, -3695, -3695, -3695,  1935,  2654,
    1896,  1896,   230,  1935,   681,   681, -3695, -3695, -3695, -3695,
    2715, -3695, -3695,  2115,  1935,   660, -3695, -3695, -3695, -3695,
    1643, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,  1732, -3695,  2476, -3695, -3695, -3695,   558,   616,
     558, -3695,  1566,  1732,  1732,  1566,  1732,  1732,   -65,  2256,
     185, -3695, -3695, -3695, -3695, -3695,  2522,   -78,  1939, -3695,
   -3695, -3695, -3695, -3695,  1362, -3695, 10674, -3695, -3695, -3695,
   -3695,  1362, -3695,  1732,  1362, -3695, -3695, -3695, -3695, -3695,
    2523,  2766,  -183,  1532,  1732, -3695, -3695, -3695, -3695, 12140,
   -3695, -3695, -3695, -3695,  5980, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,  1848, -3695, -3695, -3695,
    1435, -3695,  1668,  2456,  2866,  1732,  2462, -3695, -3695, 12140,
   -3695, -3695, -3695, -3695, -3695,  2528,  2770,  -123,  1520, -3695,
   12757, -3695, -3695,  -106,  2261,  2268,  1732, -3695, -3695,   230,
    1362,  1362,  2617, -3695,  2677, -3695,   311,  2323,   312, -3695,
   -3695, -3695,  3021,  2915, -3695,   -58, -3695, -3695, -3695,   -58,
   -3695,  1362,  1362,   805, -3695, -3695,  2404, -3695,  2605, -3695,
   -3695,  1566,  1566, -3695, -3695, -3695,  2877,   185, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,  1362, -3695, -3695,  1035,
   -3695,    97, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
    -183, -3695,  1601, -3695, -3695,  2429,  2341, -3695,  2270,   -35,
    1896, 12140,  2254, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,  -265, -3695,  2283,  2051,  2284, -3695, -3695,   235,
   -3695,   230, -3695, -3695, -3695, -3695,  1732,   311,   681,  2878,
    2476,  1103,  1732, -3695, -3695, -3695, -3695, -3695,  2534, -3695,
   -3695,    51,   304, -3695, -3695,  2285, -3695, -3695, -3695, -3695,
   -3695,  1362, -3695,  1730,  1532,  2288, -3695, -3695, -3695,   116,
    2599,  1939,  2324,   473,   903,   123, -3695, -3695,  2557, -3695,
   -3695, -3695,  1953, -3695, -3695, -3695, -3695, -3695,  1896, -3695,
    2331, -3695,  2868,  1362,  1052,  2601,  3069, -3695, -3695, -3695,
   -3695,  2502, -3695,  1532,  1240,   116, -3695, -3695,  1732, -3695,
    1362,   137, -3695,   174,  2687,  2688,  2896,  2674, -3695,   473,
   -3695,  1862,   823,  2305,    83, 15189,   680, -3695, -3695, -3695,
    1953, -3695,  1362,  2226, -3695, -3695, -3695,  1399, -3695, -3695,
     788,  1732,  1732, -3695,  1896, -3695, -3695, -3695,  2562,  1362,
   -3695, 12140,  1362, -3695,  1732,   206,   663,   137, -3695, -3695,
    1128,  1732,  2565,  1362,  1732,  1732,  1732,  1732, -3695,  2697,
      24,  2698, -3695,  2681, -3695,  1660, -3695, -3695,  1362,  2944,
    1195,  2700,    25,  2701,  2689, -3695,   944, -3695, -3695,  1362,
    2343, -3695,  1145,  1145,  1637, -3695, -3695, -3695, -3695,  2768,
    3001,  -118, -3695,  1732, -3695, -3695, -3695, -3695,   669, -3695,
    2189,  2189,  1732,   728,   -26,  1732,  7319, -3695, -3695, -3695,
   -3695,  2189,   728, -3695,  2895,  1935, -3695,  3014, -3695, -3695,
     728, -3695, -3695,  1362, -3695, -3695,  1362, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  2777,  2189,
   -3695, -3695, -3695,  6629, -3695, -3695,    91,   788, -3695, -3695,
    1566,  1566,   783, -3695, -3695, -3695, -3695,   295, -3695, -3695,
   -3695,   295,   295, -3695, -3695, -3695, -3695, -3695, -3695,  2574,
   -3695, -3695, -3695,  1362,  1935, -3695,  1362,  1362,  1362,  1362,
    1732,  1732,  1732,  1732,  1732, -3695,  1362,  1732,  1732,  1732,
    1732,  1732,  1732,  1732,  1732,  1732,  1732,  1732, -3695,  1362,
    1732, -3695, -3695,  1145,  1145,   194,  2694, -3695,  1732, -3695,
   -3695,  1362,  1362, -3695, -3695,  -230, -3695, -3695, -3695,  1732,
   -3695,  1732,  -230,   680, -3695, -3695,  -230, -3695, -3695,  -230,
    2193,  1732,   680, -3695, -3695, -3695, -3695, -3695,  6629,  1433,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695,  1532, -3695, -3695, -3695, -3695,   663, -3695,  2193, -3695,
   -3695, -3695, -3695,  1362,  1362,  1362,  1362,  1362,  1362,  1362,
    1362,  1362,  1362,  1362,  1362,  1362,  1362,  1362,  1362,  1362,
    1362,  1362, -3695, -3695, -3695, -3695, -3695, -3695,  1346, -3695,
   -3695, 13321, -3695,   698,  1362, -3695, -3695,  1231,  1231, -3695,
   -3695, -3695, -3695, -3695,   129, -3695,   698, -3695, -3695, 15659,
   -3695, -3695,  2677, -3695, -3695, -3695, -3695, -3695,  1362, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
    1362, -3695,  1768,   207,   456, -3695, -3695,   194,  2332, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  -230, -3695,
   -3695,  -230, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  1362,
    1362,  1118,  1732,  1732,  1908,  1732, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,  1762, -3695, -3695, -3695, -3695,  1362, -3695,
   -3695, -3695,  1732,   194,   194, -3695,  2765,  1732,  1732,   194,
   14242,  1362,   194, -3695, -3695, -3695,   194,   194, -3695, -3695,
   -3695, -3695,  2743,  2602,  1732,  1935, -3695,  1732,  1939, -3695,
    1732,  1362, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,   109, -3695, -3695,   692, -3695,   427,
   -3695, -3695, -3695, -3695,  1542,  1362, -3695, -3695, -3695, -3695,
   -3695,  2727, -3695,  1721,  2225, 16630, 16630,  1156,  2795,  2641,
    2641,  1854,  5980, -3695,    40,    40, -3695,   692, -3695, -3695,
   -3695, -3695,    40,    40, -3695, -3695, -3695,   115,  1362, -3695,
   -3695, -3695, -3695,  1935,  1935, -3695, -3695, -3695,  2193,  1845,
   16692, -3695, -3695,   946,   969, -3695, -3695,  1071, -3695, -3695,
   -3695, -3695,   999,   999, -3695, -3695, -3695, -3695, 16630, -3695,
     779,   779,  2641,  2641, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,  1145, -3695,  1362, -3695, -3695,  2822, -3695,
    1896,  1362, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695,  -113,    68,  3056, -3695, -3695, -3695,   779, -3695, -3695,
    2580,  2586, -3695, -3695,  2395,  -107, -3695,  2598, -3695,  2598,
   -3695,  2598, -3695,  2598, 16630, -3695, -3695, -3695,  1935, -3695,
   -3695, -3695, -3695,  2588, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,    12,     1,     3,     5,    27,     4,    75,    30,
      29,    27,     8,    10,    11,     0,     0,     0,    13,    24,
      78,     9,    34,    31,    52,    52,     0,     0,   393,     0,
     240,    80,     0,     0,     0,    75,    75,    28,    76,     0,
      25,   395,     0,     0,    74,   242,     0,     0,     0,  2978,
    2907,  2907,  2907,     0,     0,     0,     0,  2907,     0,     0,
    2871,   156,    77,    81,    82,    84,    85,    88,    86,    87,
       0,   141,   143,   144,   145,   205,   146,   148,   147,   149,
     150,   151,   152,   153,   154,   155,     0,     0,    55,    16,
       0,     0,  1275,     0,   397,    79,     0,     0,   244,  2546,
    2545,   167,   224,  2907,  2979,  2907,  2908,     0,     0,     0,
    2907,  2907,    95,   125,     0,    89,   139,  2872,     0,     0,
    2907,    83,   140,   142,     0,   204,    37,    36,    40,    40,
    2907,    53,    61,    20,    14,    17,    18,    22,    15,   394,
       0,    26,  1321,     0,   391,   241,   243,   367,   173,  2888,
    2907,     0,     0,     0,  2650,   235,  2534,   233,   238,     0,
       0,    97,   127,   237,    91,   536,   215,   216,  2907,  2547,
     207,   208,  2912,   211,  2659,  2100,  2099,   157,   161,   164,
    2957,  2907,     0,   206,     0,     0,    32,     0,     0,    63,
       0,    19,     0,  1271,     0,     0,   396,   403,   404,   517,
     398,   520,     0,     0,  2747,   245,   239,   370,   168,   169,
     171,     0,     0,   225,   226,   236,   231,  3029,  3030,     0,
     229,     0,  2870,  2985,  2968,  2907,   123,    96,  2967,   101,
     103,   104,   105,   106,  2967,     0,  2873,     0,     0,   126,
       0,   130,    90,    93,   217,     0,   209,  2914,  2913,   212,
       0,  2957,  2960,  2959,     0,     0,   158,   162,    41,    35,
    2907,    57,    58,    59,    60,    56,  2938,  2907,    68,    39,
      38,     0,     0,  1528,     0,  1389,  1499,  1509,  1517,  1524,
    1571,  1577,  1597,  1592,  1598,  1603,  1599,  1611,  1621,  1750,
    1759,  1761,  1764,  1795,  1806,  1809,  1812,  1804,  1818,  1829,
    1851,  1855,  1859,     0,  1915,  1917,  1923,  1927,     0,  1933,
    1963,  1990,  1992,  1997,  2027,  2028,  2044,  2047,  2048,  2053,
    2062,  2063,  2076,  2089,  2128,  2146,     0,  2183,  2199,  2208,
    2210,  1303,  2214,  2217,  2220,  2237,  2276,     0,  1323,  1324,
    1325,  1326,  1327,  1328,  1329,  1330,  1332,  1331,  1333,  1335,
    1334,  1336,  1337,  1338,  1339,  1340,  1341,  1342,  1343,  1344,
    1345,  1346,  1347,  1348,  1349,  1350,  1351,  1352,  1353,  1354,
    1355,  1356,  1357,  1358,  1359,  1360,  1361,  1362,  1363,  1364,
    1365,  1366,  1367,  1368,  1369,  1370,  1371,  1372,  1373,  1374,
    1375,  1376,  1377,  1378,  1379,  1380,  1381,  1382,  1383,  1384,
    1385,  1386,  1322,     0,   464,   399,  2730,     0,  2519,   400,
     369,     0,  2748,     0,     0,   384,   379,   368,     0,   373,
     375,   376,   377,  2907,  2907,   174,   175,  2668,  2664,  2669,
    2667,  2665,  2670,  2666,   227,   220,   222,  3011,   230,     0,
    2651,   234,  2986,  2907,     0,   100,   102,    98,   124,  2967,
    2907,  2874,   108,     0,     0,   137,    40,     0,    40,     0,
     128,   131,     0,     0,     0,  2679,  2675,  2680,  2678,  2676,
    2681,  2677,   218,  2671,  2673,  2660,   210,   213,     0,  2958,
     165,   159,   160,   163,     0,     0,  2939,  2907,     0,     0,
       0,    21,    23,  1532,  1529,  1530,  1531,  1277,  2772,  2773,
    2774,  2775,  2776,  2777,  2778,  2779,  2780,  2781,  2782,  2783,
    2784,  2770,  2823,  2824,  2825,  2826,  2827,  2828,  2829,  2830,
    2831,  2832,  2833,  2834,  2835,  2836,  2837,  2838,  2839,  2840,
    2841,  2842,  2843,  2844,  2845,  2785,  2786,  2787,  2788,  2789,
    2790,  2791,  2792,  2793,  2794,  2796,  2795,  2797,  2798,  2799,
    2800,  2801,  2802,  2803,  2804,  2805,  2806,  2807,  2808,  2809,
    2810,  2811,  2812,  2813,  2814,  2815,  2816,  2817,  2768,  2818,
    2819,  2820,  2821,  2822,  1388,  2769,  2771,  1420,     0,     0,
       0,  1528,     0,     0,     0,     0,     0,  1616,  1662,     0,
    1616,  1528,  2441,  1797,     0,     0,  3001,  1556,  1555,     0,
    1817,     0,     0,     0,     0,     0,  1898,  1910,     0,     0,
       0,     0,  1387,  1938,  2428,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2179,  2182,  2166,
    2180,  2433,  2181,  2168,  2177,  2169,  2178,  2636,  2640,  2186,
       0,  2209,  2207,     0,  1321,     0,     0,     0,     0,     0,
    2292,  2338,     0,     0,   747,     0,     0,     0,   522,     0,
       0,   526,   527,   525,     0,   402,   405,  2749,   246,  2952,
    2885,   380,   381,   382,  2852,     0,   371,   374,     0,     0,
       0,   221,   219,     0,     0,     0,   117,    99,  2652,   109,
     132,   133,   136,   138,   134,   135,   129,    92,     0,  2672,
       0,   214,   166,    33,    44,    47,    51,    50,  2967,    45,
      46,     0,    65,    67,    66,    64,  2907,    54,  1280,  1278,
    1297,  1419,  2650,  1497,  1391,  1430,     0,  2936,  1418,  2950,
    2706,  2704,  2707,     0,  2700,  2708,     0,     0,  2714,  2950,
       0,  2502,  2504,     0,     0,     0,  2703,  2571,     0,  2506,
    2705,  2709,  2710,     0,     0,  2702,  2714,  2701,  1507,  2563,
    1505,  2555,  2558,     0,  2557,  2561,  2562,  2711,     0,     0,
       0,     0,     0,  1510,     0,  2491,  2494,  2496,  2499,  2581,
       0,  2501,  2736,  2579,  2580,  2531,  1518,  1519,     0,  2527,
    2529,  2528,  1569,  2428,  2610,  1576,  1572,  1573,  1575,  2609,
    1588,  1578,  1579,  1580,  1583,  2950,  1595,  2644,     0,  2508,
    2750,  2550,  2643,  2648,  2551,     0,  1609,  2971,  2877,  1600,
    2641,  1602,  2995,     0,  1618,  1620,  1612,     0,  1659,  1693,
    1692,  2675,  2865,  1641,  1691,  1684,  1690,  1683,  1748,  2355,
    2553,  1636,  1638,  1628,  1629,  1642,     0,  1630,  1631,  1680,
    1632,  1633,  1693,  1635,     0,  2557,  1757,     0,  1760,     0,
    1762,  1771,  1770,  1793,     0,  1767,  1769,  2440,  2907,  1799,
    1803,  1801,  1804,  1802,  1796,  1807,  1808,  2548,  1810,  1811,
    3002,  1813,  2525,  1805,  2439,  1823,  2438,  1830,  1832,  2521,
    1852,  1853,   971,  1699,     0,     0,  1856,   970,  1860,     0,
    1862,  1863,  1864,     0,     0,  1916,  2132,  2630,  2631,  2745,
       0,  1921,     0,  1924,     0,  1931,     0,  1939,  1934,  1935,
       0,  2892,  1964,  1976,     0,  2520,  1991,     0,  1993,  1995,
    2025,  2742,  2042,     0,  2045,  2279,  2511,  2051,  2971,     0,
    2060,  2512,  2279,     0,  2074,  2067,  2514,  2077,  2080,     0,
       0,  2524,  2090,  2091,  2092,  2093,  2094,  2095,  2119,  2096,
    2122,  2097,  2098,     0,     0,  2522,     0,     0,  2629,  2648,
    2129,  2164,  2151,  2170,  2432,     0,  2638,  2639,  2197,     0,
       0,     0,     0,  2205,     0,  2211,  2212,  1309,  1315,  1304,
    1305,  1306,  1308,     0,  2215,     0,  2633,  2218,  2973,  2612,
    2235,  2223,  2611,  2613,  2238,  2239,  2290,  2279,     0,     0,
     518,     0,     0,   750,   569,   572,     0,     0,   523,     0,
     533,   534,   528,   535,   531,  2907,     0,  2953,     0,  2886,
    2993,  2853,  2887,   372,   180,   179,   200,   196,  2652,   201,
     185,   199,   197,   177,   178,   198,   170,   176,   187,   188,
     190,   182,   183,   184,   172,   181,   318,   228,   223,   232,
       0,   120,   122,   121,   118,   119,  2539,     0,  2887,    94,
    2674,    49,    43,    48,  2754,  2755,  2756,  2757,  2758,  2759,
    2760,  2761,    62,     0,  1285,  1285,     0,     0,  1498,  1390,
    1430,  2860,  2862,  1435,  2907,  1416,  1431,  1432,  1434,  1436,
       0,     0,  2879,     0,     0,  2937,     0,     0,  2951,     0,
       0,     0,     0,     0,  2698,  2572,  2720,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2699,  1508,  1500,     0,
       0,  2556,  2564,  2565,  2566,     0,  2686,     0,     0,  2498,
    2578,     0,  2497,  2738,     0,     0,     0,     0,     0,  2582,
    2583,  2584,  2737,  1513,  1520,  1522,     0,  1570,  1525,  1535,
    1574,     0,  1583,  3042,  3043,  1581,     0,  1584,     0,  1596,
    1593,  3026,  3025,  2509,     0,  2752,  2510,  2646,  2647,  1606,
    1607,  1610,  1604,  2972,  2010,  2878,  1601,  2642,  2996,  1617,
    1619,  1614,  1642,  1694,     0,  2866,     0,  1749,  1622,  1318,
    1318,  1627,  2361,  2358,  1637,  1634,  2554,  3010,     0,  1661,
       0,  1695,     0,  2355,  2355,  2355,  2355,  1758,  1751,     0,
       0,  1539,  1794,  1765,  2441,     0,  1766,  1773,  1774,  1318,
    2457,  2455,  2908,  2461,  2458,  2450,  2454,  2452,  2453,  2449,
    2451,  2442,  2443,  2456,  2445,     0,  1800,  1798,  2549,  1815,
    1824,  1825,  1834,     0,  1854,  1698,  1051,  1080,  1048,  1130,
    1065,  1064,  1129,  1131,  1153,  1132,  1116,  1199,  1233,  1149,
    1178,  1152,  1175,  1221,  1124,  1147,  1143,  1150,  1173,  1219,
    1050,  1053,  1160,  1157,  1049,  1156,  1155,  1205,  1077,  1159,
    1078,  1234,  1082,  1142,  1171,  1168,  1195,  1186,  1223,  1022,
    1196,  1206,  1169,  1104,  1106,  1105,  1172,  1207,  1208,  1209,
    1210,  1068,  1069,  1198,  1161,  1163,  1162,  1167,  1102,  1183,
    1076,  1185,  1192,  1193,  1084,  1086,  1197,  1089,  1028,  1181,
    2893,  1127,  1103,  1075,  1045,  1204,  1044,  1047,  1046,  1202,
    1194,  1170,  1154,  1215,  1190,  1191,  1126,  1212,  1213,  1214,
    1203,  1218,     0,  1079,  1180,  1176,  1179,  1211,  1166,  1177,
    1085,  1118,  1148,  1144,  1140,  1151,  1174,  1216,  1217,  1184,
    1087,  1088,  1052,  1220,  1081,  1125,  1083,  1164,  1165,  1201,
    1117,  1119,  1021,  1090,  1107,  1128,  1200,  1232,  1158,  1141,
    1182,  1123,  1146,  1145,  2893,   981,   996,   997,   998,   999,
    1000,  1001,  1002,  1003,  1004,  1005,  1006,  1007,  1008,  1009,
    1010,  1857,  1238,  2893,  2893,  2893,  1858,  1242,     0,  1885,
    1869,  1861,  1866,  1867,  1868,  1908,     0,  1908,     0,  2136,
       0,  2632,  2746,  2909,  1919,   974,   976,  2915,     0,  1920,
    1922,  1918,     0,     0,  1932,  1928,     0,  1936,  1943,  1940,
    1942,  1941,  1944,  2950,  1978,  2950,  2531,  2615,  2430,  2430,
       0,  1974,     0,  2614,  2528,   539,  2616,     0,  2429,  1996,
    1994,  2026,  1998,  2743,  2744,  2971,  2043,  2029,  2031,  2032,
       0,     0,  2046,  2052,  2049,  2000,  2513,  2061,  2054,  2010,
    2069,  2075,  2064,     0,  2069,     0,  3010,  2078,     0,  2598,
    2604,  2605,  2606,     0,  2120,  2123,     0,     0,     0,  2523,
    2102,     0,  2101,     0,     0,  2646,  2165,  2147,  2153,  2907,
    2154,  2149,     0,  2167,  2172,     0,  2486,  2484,     0,  2637,
    2198,  2184,     0,  2187,  2188,  2191,     0,     0,  2206,  2200,
       0,  2213,  1310,  1314,  1307,     0,  2974,  2975,  2219,  2236,
    2221,  2867,     0,  2240,  2291,  2277,  2281,  2336,     0,  2336,
       0,   520,   465,     0,     0,   753,     0,   615,     0,   524,
     530,  2907,   537,  2875,  2907,     0,     0,  2907,  2875,  2938,
    2907,  2850,   401,     0,   406,   409,   410,   411,   412,   413,
     414,   415,   416,   417,     0,     0,     0,   247,  2971,   383,
    2532,  2994,  2875,     0,   186,   189,   192,     0,  3037,  3039,
    3038,   107,  2654,  2653,   110,     0,  2652,    70,    71,    72,
      73,    69,  2868,  1285,  1282,  1288,     0,  1285,  1298,  1299,
    1272,  2342,  2619,  1447,  2617,  2618,     0,  1433,  3022,  3021,
    2942,  3024,  3023,  1445,  1446,  2942,     0,  1451,  2907,  1464,
    1465,  1466,  1453,  1455,  2907,  2880,  1456,  1496,  2907,  1457,
    1460,  1458,  1459,  1461,     0,  1490,  1491,  1468,  1470,  2970,
    1471,  1494,  1492,  1493,  1462,     0,  1473,  1463,  1452,  2848,
    1475,  1495,  1478,  1437,  1467,  1472,  1477,     0,     0,     0,
    2591,     0,  1425,  1429,  1428,  1421,  1417,  1412,  2342,  3018,
    3017,  1409,  1400,  1402,  1403,     0,  2342,  2922,     0,     0,
       0,  1443,  1393,  1398,  1397,  1407,     0,  1415,  1395,  1414,
    1396,  2595,  2950,  2950,  2594,     0,  2568,     0,  2486,  2484,
       0,  2486,     0,  2716,  2486,     0,     0,  2503,  2505,  2486,
       0,     0,     0,  2486,  2575,  2576,  2577,     0,  2507,     0,
    2486,     0,  2950,  2644,  2382,  1506,  2648,  2551,     0,     0,
       0,  2486,  2500,  2740,  1513,  2490,  2489,  2493,  2492,  2495,
       0,  1515,     0,     0,  2530,  1526,     0,  1533,  1590,  1582,
    1587,     0,     0,  2552,  2382,  2907,  2751,  2645,  1608,  2887,
    2418,  2011,  2012,  1613,     0,  1660,  1685,  1669,  2360,  1319,
    2363,  2356,  2362,  2357,  2359,     0,  1651,  1650,  1639,  1646,
    1648,     0,  2909,  1736,  1737,  1738,  1725,     0,  1728,  2907,
    1729,  2883,  2909,  1732,  1733,  1739,  1734,     0,  2907,  1735,
    1742,  1740,  1643,  1644,  1668,  1663,  1664,  1666,  1667,     0,
    1681,  1688,  1625,  1626,  1623,  1624,     0,  2382,     0,  1540,
    1763,  1768,  1790,  1787,  1789,  1788,  1782,  1784,  1791,     0,
    1775,  1772,  1777,  1778,     0,  1776,     0,  2460,  2444,  2471,
    2472,  2473,  2462,  3001,  2479,  2482,  2481,  2483,  2475,  2468,
    2470,  2469,  2474,  2476,  2478,  2480,  2446,  2463,  2464,  2465,
    2466,  2467,  2952,  1814,  2526,  1826,  1827,  1318,  3001,  1842,
    1843,  1845,  1847,  1848,  1844,  1846,  1837,  3001,  1833,  2894,
    2895,     0,   980,  1237,     0,  1239,     0,     0,     0,  1243,
       0,  2649,  2600,  2601,  1886,     0,  1888,  1887,  1889,  1871,
    1881,     0,     0,  1865,  1909,  1899,     0,  1911,     0,  2138,
       0,     0,  2910,  2911,     0,   975,  2917,  2916,  2918,     0,
    1016,  1139,  1100,  1043,  1059,  1110,  1029,  1133,  1108,  1057,
    1025,  1138,  1225,  1135,  1121,  1054,  1122,  1120,  1091,  1093,
    1096,  1055,  1109,  1063,  1112,  1061,  1101,  1098,  1014,  1113,
    1134,  1023,  1067,  1188,  1226,  1037,  1095,  1031,  1038,  1058,
    1030,  1114,  1228,  1115,  1026,  1074,  1229,  1013,  1020,  1040,
    1071,  1072,  1041,  1056,  1017,  1018,  1073,  1011,  1094,  1039,
    1024,  1230,  1097,  1136,  1019,  1224,  1187,  1231,  1189,  1042,
    1060,  1092,  1012,  1137,  1227,  1027,  1062,  1070,  1036,  1222,
    1034,  1035,  1111,  1099,  1032,  1033,  1066,  1015,   979,   982,
     983,   984,   985,   986,   987,   988,   989,   990,   991,   992,
     993,   994,   995,     0,  1925,  2382,     0,  3010,  2010,     0,
       0,  2891,  1976,  1966,     0,     0,     0,  1977,     0,  2000,
       0,  2280,  2626,  2627,  2628,     0,     0,  2056,  1318,     0,
    2068,     0,  2082,  2281,     0,     0,     0,     0,     0,  2121,
       0,     0,  2125,  2124,  2116,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2104,  2105,  2745,  2418,     0,  2171,
    2989,  2989,  2487,  2488,  2656,     0,     0,     0,  2195,  2189,
    2867,  2190,     0,     0,  2382,     0,  1316,     0,  2731,     0,
    2195,  2286,  2285,  2010,  2846,  2337,  2293,     0,  2339,     0,
     519,   467,   748,     0,     0,   885,  2912,   570,     0,   616,
       0,   567,     0,   734,   629,  2980,  2980,  2980,  2980,  2980,
     630,  3006,   631,   632,   633,   679,   635,   636,   637,   638,
     640,   639,   642,   675,   673,   674,   676,   677,   680,   641,
     647,   643,  2976,   678,   702,   644,   620,   645,   646,     0,
    2983,   658,   659,   657,   730,   661,   662,   660,  2907,   618,
     529,  2907,   593,   595,   596,   597,   610,   598,   599,   625,
     634,   600,   601,   602,   603,   604,   605,   606,   607,   608,
     609,     0,     0,  2857,     0,  2876,     0,     0,  2912,  2912,
       0,     0,     0,     0,     0,  2907,   460,  2851,   461,     0,
       0,   462,   407,   408,   249,  2938,  2971,  3003,   339,     0,
    2907,  2869,  2922,   341,     0,  3010,   322,   248,   321,   251,
     252,   257,   262,   259,   308,   260,   263,     0,   264,     0,
     254,   338,   255,   256,   261,   258,   253,   265,     0,  2863,
    2533,     0,   378,  2515,     0,   191,     0,  2907,  2907,   111,
    1283,  2907,     0,  1295,  1293,  1286,  1287,  1300,  2348,  2349,
    2353,  2354,  1392,  2350,  1486,  2345,  1318,  1450,  2943,     0,
       0,  2661,  1438,  2620,  2621,     0,     0,     0,  1454,  1489,
    1476,  2907,  2434,  2434,  2849,     0,  2891,  2907,     0,     0,
       0,     0,  1413,  1399,  1401,  2342,  1410,  2923,  1404,  1405,
    1406,  1444,  2859,  1408,     0,     0,  2597,  2567,  2596,  2750,
       0,  2711,     0,  2711,  2715,     0,  2690,  2721,     0,  2711,
    2711,  2711,     0,  2692,  2750,     0,  2711,     0,  1318,  1318,
    1501,  2388,  2385,  2646,  2647,  2382,     0,  2711,  2711,     0,
    2739,  1515,  1514,     0,  1511,  1523,  1521,  2430,  1537,  1538,
    1534,  1536,  1589,     0,  1586,  1585,  1594,     0,  2015,     0,
    1318,  1318,  1605,  2419,  2425,  2422,     0,  1695,  1671,  1321,
    1657,  1658,  1655,  1654,  1656,  1653,  1647,  1649,  1652,     0,
    1726,  1727,     0,  1247,  1249,  1730,  1731,     0,     0,     0,
    2434,  2434,     0,  1645,  1665,  1696,  3010,  3010,     0,  1752,
       0,  1546,     0,  2441,  1786,  1318,  1783,  2448,  2447,  2477,
       0,  1318,  1828,  1819,  1822,     0,     0,  1849,     0,  1241,
    1240,  1246,  1245,  1244,     0,     0,  1882,  1884,     0,  1877,
    1891,  1878,  1879,  1870,  1873,  1891,     0,  2604,  2436,  1913,
    2139,  2130,     0,   720,   721,  2922,  2896,   978,  1236,  1235,
     977,  1926,  1929,     0,     0,     0,     0,   548,   544,   547,
     546,   545,   550,   561,   557,   559,   560,   562,   558,   563,
     551,  2592,   564,   565,   541,   553,   554,   555,   549,   552,
     543,   542,   540,     0,  1965,  1318,     0,  1983,  1979,  1984,
    1980,  1985,     0,  1975,  1982,  2002,  2033,  2001,  1318,  1318,
    2050,  2407,     0,  2418,  2057,     0,  2070,  2441,     0,  2065,
    2071,  2087,  2086,  2085,  2084,  2083,  2103,  2126,  2623,  2127,
    2622,  2624,  2625,  2115,     0,  2118,  2107,  2108,  2109,  2113,
    2110,  2114,  2111,  2112,  2106,  2746,  2163,     0,  2160,  2161,
    2155,     0,  2148,  3045,  3044,     0,  2990,  2175,  2175,  2485,
    2657,     0,  2391,     0,     0,  2750,  2750,  2201,     0,     0,
    1317,     0,  2732,  2224,  2225,     0,  2228,  2231,  2233,  2229,
    2056,  2847,     0,  2436,  2907,   466,   520,   751,     0,     0,
     392,     0,  2907,   617,     0,   566,   743,   744,  2981,   672,
     671,   664,   663,   670,   669,   668,   667,   666,   665,  3007,
       0,     0,  2977,   728,   706,     0,   698,   622,   619,   611,
    2984,   731,   732,   729,     0,   594,   613,   745,  2867,  2762,
    2762,  2858,     0,   571,     0,   538,   429,   457,  3040,  3041,
    2542,   438,  2540,  3032,  3031,   431,  2544,  2543,  2932,  2871,
    2891,     0,  2907,   435,   434,  2907,   463,  2907,  2922,  3004,
    3005,   288,   342,  2971,  2907,  2907,  2907,  2907,   361,  2854,
     362,     0,  2907,  2938,   309,  2864,     0,   385,   386,   389,
    2516,   193,   194,  2655,  2652,  2652,   112,     0,  2907,  1292,
    1296,     0,     0,  1273,  2343,  2351,  1318,  1487,  1488,  2634,
    2344,  2346,  2352,  1448,  1449,  1481,  1479,  1480,     0,  1483,
       0,  1482,  1484,     0,     0,  1424,  1423,     0,  1427,  1426,
    1411,  2907,  1394,  1439,  1441,  2569,  2570,  2382,  2727,  2696,
    2729,  2697,  2691,  2725,  2693,  2694,  2695,  2723,  2764,  2718,
    2719,  2689,  2552,  2390,  2387,  2383,  2389,  2384,  2386,  2645,
    1502,  2712,     0,  2687,  2688,  2741,  2607,  2608,  1512,  1516,
    1539,     0,  2753,     0,  2424,  2427,  2420,  2426,  2421,  2423,
    1615,  1686,  2909,  2909,  2909,  2909,     0,  1670,  1672,  1673,
       0,  1745,  1743,  1248,  1250,  2950,  1744,  1747,  1746,  1741,
    1714,  1711,  2919,     0,  2852,  1710,  1713,  1704,  1682,  1700,
    1706,  1707,  1717,  1708,  1702,  1721,  1722,     0,  1689,  2382,
    2508,  2382,  2508,  1541,  1542,  1288,     0,  1785,  1792,  1780,
    1781,  1779,  1816,  1821,  1826,  1835,  1838,  1839,  2881,  2998,
    1831,  3001,  1836,  1891,  2602,  2603,  1891,     0,  2903,  2903,
    1876,  1892,  1893,  1874,  1880,  1875,  2891,  1900,  2309,     0,
    2140,   309,  2134,     0,  2897,  2137,  2382,  2961,  2961,     0,
    1945,  1946,   556,  2431,  1970,  1972,  1973,  1969,     0,     0,
    2952,     0,  2021,  2003,  2016,  2009,  2005,  2018,     0,  1318,
    1318,  2030,  2039,  2036,  2406,  2409,  2400,  2408,  2401,  2055,
    2058,     0,  1318,  1318,  2072,  2924,  2079,  2117,  2162,  2152,
    2156,  2157,  2158,  2159,  2150,  2173,  2176,  2174,  2658,  1318,
    1318,  2185,  2397,  2394,  2907,  2193,  2192,  2194,  2382,  2764,
    2382,  1312,  2216,  2599,  2731,  2227,  2891,  2436,  2891,  2391,
    2287,  2284,  2283,  2926,  2294,     0,     0,   468,   520,   749,
     520,   754,     0,   588,   590,   589,   583,   587,   585,   586,
     582,   584,   581,   740,   735,   737,     0,   568,   733,   965,
     961,   962,   955,   959,   967,   953,   681,   960,   952,   688,
     958,   652,   954,   956,   957,   966,   649,   651,   963,   653,
     964,   648,   656,   655,  2891,   700,  2999,   703,  2999,     0,
       0,   628,   627,   626,     0,   612,     0,  2869,   690,   691,
     575,   574,     0,   419,     0,   456,  2541,  2933,   440,     0,
     422,  2985,   449,   451,   455,   454,   450,   452,   448,   453,
       0,     0,     0,  2907,   290,   289,   286,   341,   337,     0,
     344,     0,     0,  2855,  2856,   360,   363,     0,  2907,     0,
    2887,   340,   387,     0,   388,     0,   113,   114,  1289,  1290,
    1294,     0,  1284,  1301,  1303,  2347,  1469,  3033,  3034,  2435,
    1485,  1474,  1422,     0,  1440,  1503,  2899,  2766,  2713,  1550,
    1591,  2014,  2013,  3010,     0,     0,     0,     0,  1679,  1674,
    2884,  2921,  2920,     0,  1703,  2909,  1701,  1716,  1715,  1718,
       0,     0,  1709,  1754,     0,  1753,     0,  1543,  1544,     0,
    1548,  1547,  1549,  1318,  1840,  2882,     0,     0,  1872,  1883,
    1891,  2904,     0,     0,  1894,  1895,     0,  2309,  2950,  1903,
    1914,     0,     0,  2143,  2133,  2898,  1930,  2962,   364,   365,
     366,     0,  3014,  1962,  3013,  1937,  1947,  1948,     0,     0,
    1971,  1967,  1986,  1988,     0,  2006,  2907,  2418,  2004,  2017,
       0,     0,     0,  2020,  2041,  2038,  2034,  2040,  2035,  2037,
    2059,  2066,  2073,  2925,  2088,  2399,  2396,  2392,  2398,  2393,
    2395,     0,  2203,  2766,  2202,  2241,  1311,     0,  2226,     0,
    2230,     0,  2222,  1318,  1318,  2278,  2289,  2415,  2412,  2288,
    2927,  2928,  2282,  2297,  2340,   470,   469,   752,   756,   886,
       0,   741,   738,   578,  2950,   581,   573,   576,   579,   682,
     683,   687,   686,   685,   684,   650,   689,   654,     0,     0,
     698,  3000,     0,   699,   704,  2907,   624,   623,   614,   746,
    2763,   430,   421,   420,   418,   458,   439,  2871,   427,   436,
     433,   437,   432,   298,   299,   297,   296,     0,   279,   280,
     281,   275,   276,   293,   282,   283,   293,     0,   284,   285,
     274,   272,   273,   278,   277,   293,   293,   293,     0,   343,
       0,   351,   359,  2535,     0,  2907,   310,     0,     0,   390,
     195,     0,     0,  1291,  1303,  1321,  1442,  2900,     0,  2899,
    2382,  1559,  2905,  1687,  1678,  1677,  1675,  1676,  1724,     0,
    2909,  1719,  1720,  2382,  2382,  1545,  2560,  2559,  2635,  1820,
       0,  1850,  1890,  1897,  1896,  2437,  1903,     0,     0,  2373,
    2907,  2141,     0,     0,  2131,  2135,  1960,  2963,  1957,  1959,
    1958,  1951,  1956,  1949,     0,  1954,  1952,  1953,  1950,     0,
       0,  2008,     0,  1999,  2024,  2407,  2404,  2023,  2007,  2019,
    2196,  2382,  2249,  1313,  2232,  2234,  2414,  2417,  2410,  2416,
    2411,  2413,  2305,  2298,  2299,     0,  2364,   472,   755,   888,
     739,     0,   736,     0,     0,   580,   707,   701,     0,   708,
    2952,  2952,   710,     0,     0,     0,   441,   442,   443,   444,
       0,   423,  2890,   429,     0,   351,   294,   269,   295,   270,
    2885,   271,   267,   268,   291,   266,   292,   348,   347,   346,
     349,   345,  2907,   353,   301,   355,  2536,   319,   328,   335,
     328,   324,     0,  2907,  2907,   311,  2907,  2907,  1321,     0,
       0,  1504,  1318,  1318,  1318,  1527,  1566,  1562,  2950,  2906,
    1553,  1558,  1557,  1552,     0,  1723,     0,  1756,  1755,  1841,
    1901,  2310,  2311,  2907,  1904,  1905,  1907,  1318,  1318,  1912,
    2379,  2376,     0,  2144,  2907,  2964,  1955,  1961,  2593,     0,
    1989,  2586,  2585,  2587,     0,  2022,  2402,  2403,  2405,  2204,
    2860,  2275,  2274,  2250,  2242,  2243,  2848,  2244,  2245,  2246,
    2247,  2270,     0,     0,     0,  2907,  2309,  2300,  2304,     0,
    2303,  2301,  1318,  1318,  2341,  2370,  2367,  2887,     0,   757,
       0,   892,   887,   889,     0,     0,  2907,   577,   709,   710,
       0,     0,  2867,   695,   715,   716,   717,  2922,   714,   621,
     447,   446,  2859,  2871,   428,  2733,   287,   354,   303,  2733,
     302,   352,     0,  2934,   326,   331,     0,   327,     0,   325,
     317,     0,     0,   312,  2652,  2652,     0,     0,  2767,  1564,
    1568,  1565,  1560,  1567,  1561,  1563,     0,  1551,  1705,  2373,
    2312,     0,  1906,  2378,  2381,  2374,  2380,  2375,  2377,  2142,
       0,  2588,  2589,  2590,  1987,     0,  2987,  2271,  2272,     0,
    2952,     0,  2314,  2302,  2369,  2372,  2365,  2371,  2366,  2368,
     471,   473,  2903,   758,     0,   893,     0,   890,  3009,     0,
     697,   710,   705,  2652,   711,   718,  2907,     0,     0,   425,
     301,     0,  2907,   350,   357,   358,   356,  2935,     0,   334,
     336,   313,   314,   115,   116,     0,  2765,  1554,  1902,  2313,
    2145,     0,  2988,     0,     0,  2273,  2268,  2267,  2266,     0,
    2307,  2950,  2324,   494,     0,     0,   898,   899,     0,   891,
     742,   696,   724,   725,   727,  2652,   713,   445,  2952,   424,
     305,  2735,     0,     0,     0,     0,     0,  1302,  2269,  3028,
    3027,  2965,  2517,     0,  2262,  2256,  2257,  2259,  2907,  2306,
       0,     0,  2295,  2907,     0,     0,     0,     0,   475,   495,
     496,   477,   505,     0,  2907,  2982,     0,   896,   950,   726,
     719,  2537,     0,     0,   300,  2734,   304,     0,  3035,  3036,
     332,  2907,  2907,  2966,  2952,  2518,  2265,  2260,  2263,     0,
    2258,     0,  2315,  2316,  2907,     0,     0,  2325,  2326,  2328,
    2364,  2907,     0,     0,  2907,  2907,  2907,  2907,   497,     0,
    2937,     0,  2992,     0,   474,   478,   480,   479,     0,     0,
       0,     0,     0,     0,     0,   476,   506,   508,   507,     0,
       0,   762,  2912,  2912,  2929,   795,   761,   765,   766,     0,
       0,     0,   922,  2907,   910,   911,   912,   903,  3006,   904,
    2942,  2942,  2907,  2909,  2883,  2907,     0,   927,   920,   907,
     921,  2942,  2909,   908,     0,     0,   919,   929,   926,   924,
    2909,   909,   923,     0,   930,   918,     0,   945,   939,   943,
     942,   940,   944,   900,   946,   941,   925,   913,     0,  2942,
     951,   969,   968,   972,  2538,   426,     0,   332,   329,   333,
       0,     0,     0,  2264,  2261,  2308,  2317,     0,  2321,  2323,
    2322,  2319,  2319,  2333,  2329,  2684,  2685,  2682,  2683,  2330,
    2334,  2327,  2296,     0,     0,   503,     0,     0,     0,     0,
    2907,  2907,  2907,  2907,  2907,   481,     0,  2907,  2907,  2907,
    2907,  2907,  2907,  2907,  2907,  2907,  2907,  2907,   509,     0,
    2907,  3060,  3061,  2912,  2912,     0,   759,   763,  2907,   771,
     767,   769,   770,   772,   774,     0,   901,   902,   935,  2907,
     933,  2907,     0,     0,   905,   906,     0,   948,   931,     0,
    2999,  2907,     0,   949,   947,  1269,   934,   899,   973,     0,
     202,   203,   307,   330,   315,   316,  2255,  2252,  2254,  2253,
    2248,  2251,  2318,  2332,  2320,  2331,     0,   498,  2999,   502,
     500,   504,   499,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2930,  2931,  2662,  2663,   782,   777,  2926,   781,
     780,     0,   796,     0,   768,   773,   938,  1265,  1261,   936,
     916,   917,   937,   932,  1258,  1268,     0,   915,   914,  2982,
     306,  2335,   722,   490,   486,   487,   491,   489,     0,   492,
     482,   488,   483,   484,   485,   514,   510,   511,   515,   513,
       0,   512,   775,  2927,  2928,   776,   779,     0,     0,   797,
     535,   764,  1253,  1251,  1254,  1252,  1266,  1267,     0,  1262,
    1263,     0,  1259,  1257,  1255,  1256,   928,   501,   723,     0,
       0,     0,  2907,  2907,     0,  2907,   783,   784,   785,   786,
     787,   788,   778,     0,   799,   800,  1264,  1260,     0,   516,
    3047,  3046,  2907,     0,     0,  3049,     0,  2907,  2907,     0,
    2982,     0,     0,   794,   790,  3048,     0,     0,   789,   855,
    3016,  3015,  2901,  2944,  2907,     0,   854,  2907,  2950,   798,
    2907,     0,   805,   806,   807,   816,   808,   810,   813,   801,
     802,   803,   812,   814,     0,   817,   804,   864,   809,     0,
     811,   815,  3019,  3020,  2947,     0,   791,   793,   792,  2902,
     884,  2946,   863,     0,   698,     0,     0,     0,     0,  2940,
    2940,     0,     0,   867,     0,     0,   862,   864,   866,   870,
     871,   879,   880,     0,   882,  2948,  2949,   873,     0,  2945,
     842,   840,   841,     0,     0,   835,   839,   836,  2999,  2750,
     844,  2573,  3051,     0,     0,  3053,  3055,     0,  3059,  3057,
     818,   823,  2954,  2954,   820,   824,   819,   825,     0,  2941,
     856,   856,  2940,  2940,   849,   868,   869,   865,   881,   878,
     877,   875,   876,  2912,   874,     0,   837,   838,   704,   883,
    2952,     0,   843,  2574,  3050,  3054,  3052,  3058,  3056,  2956,
    2955,   826,   831,     0,   860,   858,   850,   856,   859,   852,
       0,     0,   872,   493,   693,     0,   846,   829,   821,   829,
     834,   829,   822,   829,     0,   857,   851,   853,     0,   692,
     848,   845,   847,     0,   828,   827,   833,   832,   861,   694,
     830
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -3695, -3695, -3695, -3695, -3695, -3695, -3695,  3137, -3695, -3695,
   -3695, -3695, -3695, -3695,  3017, -3695, -3695, -3695,  2044, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  3068,  2964,
    1256, -3695, -3695, -3695,  2454,  3134, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
    3097, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  2928,  1059,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  2999, -3695,
   -3695, -3695, -3695,  2925, -3695, -3695, -3695, -3695,  3096, -3695,
   -3695, -3695, -3695,  2910, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,  2490, -3695,  2125, -3695, -3695, -1568,
   -3695, -3695, -3695, -3695, -3695,  3000, -3695, -3695, -3695, -3695,
    3005, -3695, -3695,  2738, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -1195, -3695, -3695, -3695,  -476, -3695, -3695, -3695, -3695,
    1237, -3695, -3695, -3695, -2171, -3695, -3695, -3695, -3695, -3695,
    -275,  -690, -3380, -3695, -3695,   485, -3695, -3695, -3695, -3695,
   -3695,  -246, -3695, -3695, -3695,  -391, -3695, -3695, -3695, -3695,
     481, -3695, -3695, -3695, -3695,  2767, -3695, -3695, -3695, -3695,
   -3695, -3695,   476, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
    -239, -3695, -3695, -3695,   135, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
    -598, -3695, -3695, -3695,  -541, -3695, -3695,  -607, -3695, -3695,
   -3695, -1502, -3695, -3695,  2533, -3695, -3130, -3695, -3279,  -647,
   -3695,  -908,  1130, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3036, -3695, -3695, -3695, -3695, -2673, -3695, -3695,   992,
   -3695, -3695, -3695, -3695,  1627, -3695, -2453, -3695, -3695, -3695,
   -2366,   541,    66, -3695, -3695, -2353, -3695, -3695, -3695, -3163,
   -3695, -3695, -1042, -3695, -3695, -3183, -3695,  -450,  -357, -3695,
    1260, -3695, -3306, -3695,  -491, -2347, -3695, -3695, -2311, -3695,
   -1550, -3695,   540, -2588, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,  -729, -2599, -3695, -3695,  -852, -1695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -2463, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -2412, -3695, -3695, -3695, -3695,  -970, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,  -973, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,  -332, -3695, -3695, -3695,  -745, -3695,
   -3695,  -474, -3695,  2607, -3695,  -646, -1416, -3695,  -642, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,  1808,  2310,   471,  -602,  -601, -3650,  -793,
   -3695, -3695, -3695,  -605, -3695,  -631, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695,  2144,   493, -3695,
     382,   938, -3695, -3695, -3695, -3695, -2729, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,  -907, -3695, -3695,  -119, -3695,  3036,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  2386, -1525,
   -3695,  2147, -3695,  2143, -3695,   486, -3695,  -695, -3695, -3695,
    -950, -3695, -3695,   499,  2141, -1088,  1580, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,  1481,   865, -3695, -3695,
   -3695,  2461, -3695, -3695, -3695, -3695, -3695,  1393, -3695, -3695,
   -3695,   458, -3695, -3695,   406, -3695, -3695,  -948, -3695, -3695,
   -3695,  -217, -3695,  -215, -3695, -3695, -3695, -3695,  2458, -3695,
   -3695, -3695, -3695,  2093, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695,  2662, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695,  2413, -3695, -3695,  2065, -3695, -3695,  1450, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695,  1426, -3695, -3695, -3695,
   -3695,   451, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,  2411,   855,  2655, -2262, -2573, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -1725, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
    2042, -3695, -3695,  2038, -3695, -3695,  1408,   825, -3695, -3695,
   -3695, -3695, -3695,  2405, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
     417, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,   418,
    2023, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695,  1851, -3695, -3695,   803, -3695,
    1349, -3695, -3695, -2270,   407,   409, -3695, -3695, -3695, -3695,
   -3695,   -64, -3695,  -203,  1856, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695,  2369, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,   126, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -2735,  1227, -3695, -3695, -3695,   392, -3695, -3695,
   -3695, -3695, -3695, -3695,  -231, -3695, -3695, -3695,  1225, -3695,
   -3695, -3695, -1253,   761, -3695, -3695,   393, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695,   385, -3695,   387,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695,   693, -1327, -3695, -3695, -3695, -3695, -3695, -3695,
    1813,   751, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,  -144, -3695, -3695, -3695, -3695,  1198,
   -3695, -3695, -3695,  2349, -3695,  2350, -3695, -3695, -3695, -3695,
    2683, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
     722, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,  1780,
   -3695, -3695,  1181, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,   358, -3695,  1184, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695, -3695,
   -3695,  -400, -3695, -3695, -3695, -3695, -3695,  -204, -3695, -3695,
   -3695, -3695,  -548,  1234,  1250, -3695, -3695, -3695, -3695, -3695,
   -3695, -3695, -3695, -3695,   -74, -3695, -3695, -3695, -2881, -3695,
    -161, -3695, -3695,  -441,  -560, -3385, -3695, -3695,  -442, -3695,
   -3695,  1770, -3695, -3695, -3695, -3695, -1512, -3695,  1021, -3695,
   -3695,  1024, -3695,  1232, -3695,  2133, -3695,  2136,  -440, -3695,
    -208, -3695,  -202,  -268, -3695,  -156, -3695,  -158, -1674, -3695,
     962, -3695,   966,   380, -3695,   398, -3695,   401, -3695, -3695,
   -3695, -2404,   -31, -1999, -3695, -3695,   128, -3695,   130, -2029,
     388, -3695,   937, -3695,   941,  2558, -1375,  2726,  -547, -1466,
   -2024,  -565, -1144, -3695, -3695,  2113, -3695,  2134,  1488, -1838,
     775,   776,   780,   781,   939,   531,  -500,  1029,  1081, -3695,
    1429,    82, -1079,  -552,  2744,  2716,  2417, -2440, -3695,  -198,
    1208,  -632, -3695,  -923, -3695,  -532, -3695,  2211,   334,   -85,
   -3695, -3695, -1017, -3695,   700, -2763,   -27,  3201,  -522,  -551,
   -3695,  -526,  1621, -3695,   245,  -537, -3695, -1061, -1979, -3695,
    -135, -3695, -3695, -3695,  1286,  -911, -3695,  1448,   510, -3210,
     782,  -586,  2221, -3695, -2015,  1056, -2526,  -536,  -439,  -527,
    -684,  -564, -3695, -2250, -3695,  1020,  -643, -3695, -3695, -3695,
   -1370,   -62, -3246, -3695,  1098,  -731,  -756,  -120,  2912, -3695,
   -1721,  2707,  -196,   -75,  -463,  -590,  2843, -3695, -3695, -3695,
     330,  2636, -3695, -3695, -3695, -3695,  1102, -3695, -3695, -3695,
   -3695,  -176, -3695, -3695, -3695, -3695,  1288, -3695, -2318, -3695,
     998,   741,   453,   200, -3695, -3695, -3695, -3695,  -112, -3695,
     582, -3695, -3695, -2113, -3695, -3695, -3695, -1514, -2126, -2587,
    -373, -1148, -3695, -3695, -3695, -3695,   -23,   -59, -3695, -1961,
     820, -3695,    96, -3695, -2725, -3695,   -45, -1718, -2112, -3695,
   -3695, -2182, -3695,  -595, -3695, -3695, -3695,  2693, -1505, -2297,
   -1612, -3695, -3695,  -671, -1412,  -803,  3165,   529, -3695, -3695,
    2713, -3695,  -764, -3695, -3695, -3695,   339, -3695,   384, -3695,
    1312, -2423, -3695, -3695, -3695, -2946,  -577, -3695, -3695, -3695,
    -180, -3695, -3695, -1104, -3695,  2344, -3695, -3695,  -614, -3695,
    -580,  -318, -3695,  1857, -3695, -3695, -3694,  -736, -3695, -3695,
   -3695, -3695, -3695, -3695
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     4,     5,     6,    11,    12,     7,     8,
      13,    14,   134,   135,   136,   190,   138,   192,    18,    28,
      92,    15,    16,    24,    33,   260,    25,    32,   128,   271,
     185,   484,   708,   709,   710,    35,   131,   132,   265,   189,
     268,   715,   490,  1621,    19,    20,    30,    31,    62,    63,
      64,    65,   164,   242,   462,    66,   161,   227,   228,   229,
     230,   231,   452,  1068,  1614,   232,   233,  1064,   234,    67,
     162,   239,   240,   241,   457,    68,    69,    70,    71,    72,
     120,   177,   482,   178,   179,    73,   148,   208,   423,   424,
     209,   210,  1046,  1054,  1047,  1048,  1049,  2284,  2711,  1050,
    3962,    74,   125,    75,   170,   171,   172,   476,    76,   166,
     167,    77,   434,   435,   150,   682,    78,    79,   438,    80,
      81,    82,    83,    84,    85,    44,    45,    98,   147,   205,
    1026,  1597,  1598,  2259,  2260,  3295,  3296,  3297,  3298,  3056,
    3435,  3427,  2261,  3276,  2262,  3569,  3570,  3652,  3744,  2263,
    2264,  3070,  3306,  2265,  1057,  2266,  2267,  2268,  2269,  3451,
    3574,  3868,  3575,  3577,  2270,  2271,  2272,  2273,  3300,  3441,
    2274,  3444,  3445,  3571,  3572,  3654,  2275,  2276,  2699,  2277,
    2890,   206,   207,   417,   418,   419,   420,   674,   421,   422,
     670,  2707,  2708,  3074,    40,   199,    41,    94,   144,   200,
     201,   666,   202,  1025,  1584,  1585,  3264,  1586,  3699,  3423,
    3033,  1587,  1588,  2685,  3270,  1589,  1590,  3266,  3416,  3417,
    3418,  3419,  1591,  3048,  3049,  1592,  3035,  1593,  1594,  2240,
     654,  2141,  2615,  2967,  2968,  3397,  3537,  3631,  3784,  3785,
    3786,  3787,  3728,  3729,  3730,  3795,  3796,  3797,  3798,   404,
    1561,   405,   406,   658,   659,  1571,   660,  1021,  1022,   168,
    2223,  2982,  2514,  2515,  2516,  2517,  2518,   661,  2625,   662,
    1566,   663,  1567,  2663,  3236,  3237,  2983,  2200,  2201,  2202,
    2203,  2204,  3025,  2148,  2205,  2206,  2207,  2648,  3019,  3257,
    2208,  2209,  2210,  3245,  3247,  2211,  4135,  4279,  2212,  3018,
    3250,  2645,  3412,  3015,  3549,  3553,  3647,  3554,  3555,  3556,
    3557,  4077,  3558,  3692,  3693,  2213,  2214,  2653,  2215,  2216,
    2217,  3231,  2984,  2985,  3402,  2218,  2219,  2220,  1013,  2616,
    1565,  2970,  2145,  3228,  3398,  3539,  3685,  3734,  3806,  3807,
    3930,  3931,  3932,  3933,  3808,  4006,  4007,  4008,  4052,  4086,
    4087,  4088,  4089,  4090,  4091,  3926,  4012,  4095,  4110,  4139,
    4140,  4210,  4268,  4284,  4272,  4141,  4195,  4196,  4142,  4242,
    4281,  4143,  4144,  4256,  4257,  4145,  4146,  4147,  4176,  4177,
    4178,  4148,  4149,  4233,  4234,  4180,  4181,  4182,  4150,  4151,
    2620,  3399,  3542,  3543,  3544,  3687,  3688,  3863,  3735,  3853,
    3737,  3013,  3860,   894,  3957,  1434,  1435,  1436,  1437,  2038,
    1395,  2039,  1396,  2040,  1397,  2041,  1398,  2042,  1399,  2043,
    1400,  2044,  1401,  2045,  2046,  1402,  2047,  1403,  2048,  1404,
    1405,  2049,  1406,  2050,  1407,  1408,  2051,  1409,  2052,  1410,
    2487,  1912,  1411,  1412,  1416,  1417,  2425,  2426,  4066,  4067,
    4023,  4074,  4075,  3940,  4071,  3938,  4068,  3854,  3855,   141,
     273,  2297,  3084,   142,   720,  1085,  1084,  1623,  1624,  1625,
    2293,  2294,  2721,  1087,  2723,  3314,   644,   989,   990,  2126,
    3206,   991,   992,  2599,  1798,  1799,  2409,   993,   195,   338,
     339,   577,   723,  1090,  2342,   724,   725,   726,  1104,  1691,
    1692,  1095,  1096,  1097,  2752,  2753,  1718,  2827,  1643,  1644,
    1099,  1683,  2726,  2727,  3856,  1831,  1686,  1089,   340,   578,
     758,  1130,  1128,   341,   579,   773,  1771,  2384,   342,   580,
     786,   787,  1773,   343,   581,   792,  2387,   497,  1775,  1776,
    2390,  1850,  2441,  2843,  2844,  2845,  3321,   599,  3473,  3465,
    3594,  3466,  3592,  3467,  1158,   344,   582,   796,   797,   345,
     583,   801,   802,  1165,   803,  1161,  2392,   346,   584,   806,
    1170,   347,   348,   349,   586,   819,   350,   585,   816,  1179,
    1182,   351,   587,   826,  1793,   827,   352,   588,   838,   839,
     840,   841,  1207,   842,  1209,  1832,  1808,  1809,  1810,  2414,
     843,  1192,   844,   845,   846,  1835,  1836,   847,  2408,  2807,
    2808,  2809,   848,  1211,  2436,   849,   850,  2407,  3103,   851,
    2437,   852,  1194,  1840,   895,  2828,  2829,  2830,  2831,  2832,
    3120,  2833,  2834,  2835,  2836,  1833,  1198,   353,   589,   856,
    1218,   354,   590,   355,   591,   860,   356,   592,   863,   864,
     865,  1226,  1227,  1228,  1861,  1229,  1856,  1857,  2444,  1223,
     357,   593,   874,   600,   358,   594,   875,   359,   595,   878,
     360,   596,   881,  1893,   361,   362,   601,  1896,  1251,  1897,
    2451,  2453,   363,   602,   887,  1252,  1906,  2457,  2856,  2857,
    2858,  2860,   364,   603,   890,   365,   604,   896,   366,   605,
     898,   899,  1421,  1422,  1932,  1423,  1424,  2473,  2474,  1929,
    1930,  1931,  2467,  2870,  2871,  2872,   367,   903,  1425,  3147,
    3599,  3349,  3484,  3485,  1935,   368,   904,  1427,  2878,   369,
     608,   370,   609,   911,  1441,   371,   610,   913,   372,   611,
     915,  1445,   373,   613,   918,   919,   920,  1452,  2058,  3165,
    3166,  3368,  3361,  3362,  3167,   374,   614,   922,  2525,  2526,
    3171,  2897,  1458,  1459,  1460,  2528,  2530,  2531,  3370,   375,
     615,   376,   616,   928,  1470,   377,   617,   930,  2076,  2902,
    2903,  2904,  1790,  1791,  1792,  3178,  2906,  3177,  3373,  1472,
     378,   379,   618,   932,  1480,  2911,  3188,  2912,  3186,  2913,
    1477,   380,   619,   934,   381,   382,   620,   937,  1484,   383,
     621,   940,  2543,  2544,  1488,   384,   385,   622,   944,  1494,
    2079,  2549,  2550,  1492,   386,   623,   947,  1496,  1497,  2083,
    2926,   387,   624,   952,   180,  1513,   953,   954,  2104,  2105,
     955,   956,   957,   958,   959,   960,   961,   962,   388,   625,
     905,  2880,  1429,  3154,  1939,  2481,  3153,  3354,   389,   626,
     971,  2107,  1521,  2577,  2578,  2579,  1517,   390,   973,  1523,
    2935,   633,   634,   391,   639,   978,   979,   980,  1533,  1534,
    2121,  2946,  2592,  1531,   392,   640,   983,  1539,   393,   642,
     394,   643,   985,   395,   645,   994,   396,   646,   997,   397,
     647,  1000,  1552,  2603,  2604,  2130,  2606,  2957,  2959,  1550,
     398,   648,  1004,  3207,  3382,  3514,  3515,  3516,  3970,  3517,
    3715,  3716,  3759,  3518,  3678,  3519,  3520,  3521,  3522,   399,
     649,  1006,  1482,  2133,  2134,  3215,  1555,   400,  1008,  1557,
    3223,  3770,  3392,  3393,  3394,  3531,  3526,  3719,  3149,  3481,
    3482,  3682,  3762,  3763,  3973,  3974,  3722,  3767,  3768,  3884,
    3889,  2136,   401,  1009,  1559,  3396,  2302,  2730,  2303,  2304,
    2724,  2305,  2306,  1201,  1803,  1202,  1801,  1203,  3534,  3628,
    3535,  3626,  3536,  3489,  3607,  3490,  3605,  3491,  2370,  2777,
    2371,  2775,  2372,  2941,  3199,  2942,  3197,  2943,  2540,  3374,
    3507,  2541,  2916,  2917,  3216,  3390,  3217,  3388,  3218,  2402,
    2403,  2798,  2404,  2796,  2405,   923,  2062,   635,  2739,  2877,
     885,   886,   867,   868,  1241,  1242,  1866,  1243,  1886,  1887,
    1888,  1889,  1890,  1891,  1526,  2115,  1729,   775,   776,   777,
     778,   759,   808,  1173,   941,   942,   945,  2282,  3675,  2283,
     727,   891,   964,   965,  1249,  1461,   789,   790,  1599,  2677,
    3447,  3740,  1065,  2671,  2672,  2678,   101,   173,   876,   810,
    1205,   760,   761,  3335,   762,   763,  4200,  1743,   781,  3500,
    1693,  2520,  3501,  1725,  2347,  2475,  2952,  1920,  2863,  1499,
    2785,   798,  1001,  1462,  1633,  2312,  2559,  2560,  2071,  1634,
     906,   968,   995,  2476,  3337,   764,   637,   821,   811,   812,
    1922,   638,  1066,  1067,  1613,   976,   977,  1635,   477,  2314,
    4010,   436,   765,   473,   474,  3890,   766,   767,   768,   769,
    1136,  1114,  1751,  1735,  1736,  1747,  1740,  1730,  1732,   664,
    2605,  3650,  1153,  1764,  2380,  1475,  1438,   413,  1176,  1786,
    1082,  3028,  3097,  3320,   574,   575,   576,  2612,  2325,  2238,
    1032,  3065,  2664,  1100,  1101,  2706,  1196,  1626,   235,   119,
     453,  2226,  1186,  1687,  3136,  2427,  1030,   151,  3424,   924,
    1911,  2885,  3318,  4160,  3142,  3474,  1245,  1944,   250,  1949,
    3113,  2338,  3194,  3222,  3925,  3038,  3658,  3789,   487,  4220,
    3939,  4162,  4187,  1109,  1028,  4251,   254,  3158,  3496,  3754,
     236,  1688,  2279,  1548,  2643,   105,  2630,  2222,   443,  3673,
    2587,  3790,  1602,  1189,  2861,  3252,  2428,  2691,  2640,  3403,
    2837,  3169,  4152,  4056,  4154,  3859,  1174,  3711,   219,  2675,
    3089,  3750,  1611,  2230,  1167,  2585,  4102,  4108,  4213,  4214,
    4215,  4216,  4217,  3811
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     181,   699,   854,  1720,   409,   107,   108,   109,  1014,  1015,
    1016,   986,   114,  1023,  1685,  1457,  2147,  1940,  1945,   882,
     102,  1604,   155,   194,   158,  1522,   110,   866,  2309,   163,
    1098,  2757,   809,  2310,  2621,   251,   439,  2128,  1498,  2285,
    2529,  1509,   780,   877,   907,   156,  2768,   156,   788,   472,
    1754,   877,   156,  1184,  2728,  2891,  2667,  1178,   152,  2140,
     153,   907,   853,   463,  1750,   159,   160,   215,  1115,  1124,
    2696,  2697,  3254,   877,  2235,   182,  1760,  2542,  2582,   774,
     888,  1858,  1177,  4270,  2063,   187,  2418,  3409,  3356,   211,
     156,   818,  3040,    99,  2419,    99,   407,   967,  1224,   464,
    2523, -1276,   966,  3414,  2429,   212,  3959,  2433, -2952,   459,
    2396,   247,   464, -1274,   984,   943,  2668,  2669,   718,   454,
    1132,  1622,  4169,   245,  3733,  2694,  3437,  1117,  1118, -2631,
    1606,  3714,  2900,  1924,  1168,  1123,   255,  1149,  2918,  1622,
    3130,  1847,  2318,   892,  3143,  1608, -3010,  1946,  3161,   222,
    1091,   675, -3010, -2869,  2225,   257,  1689,   440,  1892,  1143,
    2887,  1622, -3010,  4170,  2328, -2632,   117, -3010,  3586,  3528,
     712,   704,  1862,  2439,  1485,  2838,  1156,  1431,  2322,   414,
     444,   927,   451,  1721,  1719,  2430,  2332, -2997,  3929,   907,
    2112,   222,  2552,  4267,  2336, -2869,  2849,  2553,  3676,  4280,
   -3010,   149,  2682,   149, -2967,  2874,  3462, -3001,  3901,  3914,
   -3012, -2867,   174,  1178,  4230,   485,  1722,  1723,  3091, -2081,
    1529,  1873,   488,  4072, -2081,  1925,  1061,   480,   481,  1873,
    3224,   805,  3313,   780,   780,   780,  2077,  1070,  1515,  3468,
    -760,  3802,  3803,  1622,  2423,  2554,  2424,  3878,  3644,   921,
     464, -3010,  1394,   213,   788,  3116,  2112,  1807,  1254,   785,
     256,  3635,   655,  2659, -1968,  3116,  3346,  2483,  2580,  3540,
   -2952,  1141, -2081,   106,  2895,   597,  2683,  2948,  2949,  1657,
     493,  -760,  -760,  2112,   951,   414, -2887,  3078,  3683,  1091,
   -2972, -3010,  3128,  1800,  2733,  2734,  3299,   252,  1537,   721,
    2735,  2736,  2737,  2882,   149, -2922,   805,   203, -3010,  2939,
    1219,  2319,    -6,  3238,  1206,  1199,  3117,  2337,  1453,  3623,
    3151,  2096,  1865, -2730,   686,  1248, -2887,   705,  1418,   606,
    2110,  2583,   106,  1478,  2420,   650,  3878,  1248,    22,  3771,
    2097,  2410,  2411,  2295, -1276,    -6,   706,    -6,  2483,  -712,
   -1276,  3462, -3010,  1541,   222,  3315, -1274, -3010, -2869,  3529,
    3879,  2112, -1274,  3541,  3262,  2743,  3640,  2055,  4069, -2859,
    3869,   217,  2400, -2859,  4271,  2291,  2628,   149,   678,   679,
    3883,  2492, -3008,  1518,  3054,   804,   668,   817,   126,  2628,
    4107,   269,  2689,  3422,  1489,  1098,  3232,  3694,   685,   106,
     411,  1098,   785,  2131,  2811,   688,  2064,  2812,   877,   901,
     805,  3680,   861,  3540,  2816,  1248,     3,  2819,   907,   931,
    3765,  1787,   938,   862,    17,  2242,  1926,  2626,   972,  2484,
    2233,  2695,   412,    26,  3880,  2065,  3079,  1506,   780,   805,
   -3010, -1276,   711,  3232,  2132,   455,  3694,   237,   998,  3879,
    2597,   702,   465, -1274,  2281,  3055,  2690,  3162,  3691,  1556,
    3401,  2124,  2575, -2578,  1690,   465,  1726,  -760,   425,  2112,
    2291,  2112,  2576,  4211, -1276,  1527, -1276,  2243,  2112,   248,
    2450,   106,  4004, -2640,  2292,  1519, -1274,  3869, -1274,  3182,
    2629,  1520,  3972,  2728,  1171,  2112,  2287, -3010,  2901,  4183,
   -2889,  2401,  3438,  2631,   783,   805,  3053,  3541,   223,   814,
    2484,  -712,  1806,  1609,  2919,  1947,  1724,  4171,   791,   814,
     707,  -760,   813,  3880,  2923,  1783,   466,   814,  2311, -3010,
    2920,   218,   813,   879, -2887,  2627,  3141,  4231,  1927,   466,
     813,   467, -2638,  1062,   127,  1102,   908,   270,   494,   814,
     223,  3875,   607,   495,   467,  1479,  1200,   936,   651,   936,
     946,  3453,   969,   908,  3405,  1060,  1924,  3316,  2111,    -6,
   -3010,  3357,   106,  2296,   780,   780,   780,   809,  2921,  2292,
    2421, -3012,   780,   780,   780,  3458,   598,   936,  2940,   465,
     149, -3012,  3463,  3138,   204,    23,  3139,  2963,   780,  2289,
     656,   780,  1508,   465,  1162,   496,  2594,   780,   780,   780,
     780,   780,  3552,   437,  1027, -2887,  3131,  1180, -3010,  1739,
    1739,  1739,  1220,  3469,  1166,  1690,  2584,   174,   415, -3010,
     149,   174,  2098,  1063,  2127,  1759,  4004,   780,  3118,   224,
     722,   253,  3804,  2888,  2337,  3622, -2578,   951,  1510, -3033,
   -2891,  3560,  3561,   154,   741,  1074,   742,  2660,  1925,   866,
    1455,   779,   706,   466,   465,  1632,   807,   949,   809, -2867,
    1797,  1083,  3630,  3723,  1784,  2357,   807,   466,   467,  2375,
    3421,   224,  -760, -2859,   807,  3273,   174, -2867,  1133,  2099,
    3358,   908,   467, -1281,   468,   783,   783,   783, -1276,  1091,
     154,  2780,  3677,  4073,   238,  1150,   807,   468,  1253, -2867,
   -1274,  2069,  3351,   223,   214,  1858,  2712,  1894,  3881,  3041,
    1858,  2100,  1094,   437,   791,  1863,  4078,  1465,  3464,   741,
     987,   742,  4059,   814,   415,   225,  3042,  2928,   466,  3882,
    2687,  1152,   656,  1467,  1883,  3470,   813, -2887,  3724,  2850,
    3116,  3043,  1883,   467,  2113,   657,   222,  3609, -2907,  1787,
   -2869,  2889,  1034,  2412,   880,   154, -2873,   805,   465,  1394,
     154,  3471,  1789,  3375,  2859,  1921,   154,   225,  4232, -2859,
     154,   749,  2059, -2859,  2060, -2997,  2684,  3152, -2967,  3163,
   -2922, -2867,  3459,  1525,  2335,  2323,  4011,   465,  3439,   713,
   -3010,   814,  2431,   154,  2747,  2495,  1864,  1419,  3114,  3902,
    3915, -1968,   469,   814,   813, -1279,   950, -2907,   988,  3960,
    2113,  2896,  2101,  2750,   437,   469,   813,   416, -2859, -2859,
    2751,   468,   437,   741, -1281,   742,   707,  3415,   447,   437,
     448,  3323,   466,   154,   224,   468,  3132,  2113,  2114,  3039,
     154,   714,   779,   779,   779,  1144,   749,   467,  1807,  1926,
    1145,  1464,   741,   465,   742, -2907,   893,   719,   154,  4004,
    3342,   466,   951,  3464,  1501,  3670, -2081, -2081,  3805,  3119,
    2610,   946,  2415,  3263,   814,  3697,   467, -2671,  1607,   470,
     807,   814,   877,   780,   809,   975,   987,   813, -3034, -2867,
     783,  3260,   470,   460,   813,   154,   468,  3684,   987,   437,
    1102,  3359,  3493,  1948, -2726,  3146,  1102, -2867,  -760, -1885,
     908,  2054,  1610,  2102,  1681,  2113,   448,  2846,   741,   426,
     742,  1528,  1928,   416,  3588,  4005,  1420,   466, -2972, -2867,
     225, -2728,  3772,  1600,  2802,   676,  1035,   975,  3440,   469,
    1225,   154,   467,   437,  3183,  1248, -1279,  2288,   807,   154,
     749,  3360,   464,   469,   169,   437,   156,   154,   226,   154,
     807,  1927,  3508,  1603,  2973,   100,  3649,   100,   408,   471,
    1595,  1788,  1166,   154,   988,  4172,  3961,   456,   809,   749,
    2454,   780,   471, -1885, -3010,  3209,   988,  3211,  2286,   154,
    2374,  4198,   437,  4063,  4025,  -760,  1528,   437, -1885,  1615,
     468,  1794,  2790,  3164,   408,  3766, -3010,  1172,  3530,  2354,
     154, -2867,  3454,  1616,   469,  2373,   470,  1811,  2117,   908,
    1453,  3885,  4032,  2113,  3472,  2113,   783,   783,   783,   468,
     470,   807,  2113,   445,   783,   783,   783,  1745,   807,  1636,
    1745,  2344,  2345,  3248,  1757,   749,  3791,   779,  2413,  2113,
     783,  3666,  1745,   783,   805,  2364,   154,  1756,   154,   783,
     783,   783,   783,   783,  3966,  -521, -3010,   154,  3274, -3010,
    2558,  2367,  2348,  3095,  3104,  3105,  3106,  3107,  1805,  4005,
   -3033,  2103,  1036, -2859,  2864,   597,  1075,  1076,  1077,   783,
     655,   470, -3033,  3936,   154,   468,  1909,  2346,  1782,  1690,
    3181, -1270,  3122,  1806,  2969,   223,   471, -3010,  3044,  2313,
    3886, -3010,   174,  2676,  2803,   465,  1942, -2724,   469, -2722,
     471,   437,   197,   794,  4004,  1838, -2717,  3455,  1144,  1233,
     794, -3010,  3182,  1145,  3026,  1757,  3239,  2964,  3377,  3269,
   -2578,  1628,   106,  2378,  1457,  3426,  3448,   469,  1756, -2578,
   -2578, -1885,  2769,  3434, -2578,  3123,  1037,  3125,  3275,  1038,
    3967,  2546, -3010, -3010,  2556,   247,  2538,  3791,  3376,   154,
     631,  1039,  2909,  2920,  2661,   907,   822,   791,  3968,  4173,
     741,   471,   742,   779,   779,   779,  1638,    -7,  3068,   466,
    1639,   779,   779,   779,   823,   470,   809,  2651,  2907,  1078,
    1040,  1753,  3156, -3010,   467,  3725,   237,   779,  2673,  1511,
     779, -3010,  2355,   469, -3010,  2278,   779,   779,   779,   779,
     779,  2921,  1913,  3726,   470,  4065,  2368,  2974, -2859, -2859,
    3365,  2386,  1473,  2628,  1542,  3352,   224,   815,   117,  1941,
    3727,  2644,  4238,  2813,    46,  3657,   779,  1617,  1618,  4202,
    4105, -3010,  3240,  1899,  3045,  1512,  1900,  1901,  4174,  3523,
    2388,  4244,  2716,   465,  3202,  3331,  3204,  3163,  -521, -1885,
   -3010,  3580,  3847,   154,  3583,  2073,   692,   446,  1921,   795,
    3731,   671,  3792,   118,  4245,   471,  2449,  1573,  1858,  3046,
     470,  2767,  1753, -2730,  3047,  4115,  4100,  1834,  3732,  3800,
     917,  2883,   154, -1938,  4203,   465,  2084,   749,   174,  3487,
    4101,  2455,  4005,  3701,   471,  4249,  1413,  4254,  4246,  2846,
    2458,   408,  1474,  4015,  1041,   700,  3969,   939,  4255, -3034,
    1574,  2108,   225,  1923,  4100,    47,   636,   466,   951,  3937,
    1543, -3034,   824,  2628, -1270,   783, -1885,  2633,  4101,  2839,
   -1270,  2841,   467,  2628,  2089,  1928,  3241,   468,   814,  3848,
    1619,  1757,  1455,  3800, -3010,  3646,  2389,   437,   154,  2751,
     175,   813,  3849,   176,  1756,   186,  3442, -1697,  3850,   466,
     471,  3887,   261,   672,   673,  1546,  4247,  3329,  3449,  2732,
    1547,   465,    48,  1575,   467,    49,   598,    50,  2674, -1938,
    3661,  3662,  3532,  3792,  2886,  4015,  2521,  2521,  3242,  1079,
    1080,   154,   465,  2490,  3851,  2298,    51, -1938,  1081,  2369,
    4248,   814,  3971,  2299, -1885,  2961,  3278,  3279,  3280,  2596,
    3573,  3233,  2368,  2356,   813,   687, -1885,  2348,  2652,  3648,
     437, -1270,  1042,   783, -3010,  3353,  3443,  1043,  1044,  2746,
    2806,  2773,  2774, -2520,  2539,  1757,  3226,  2635,  3227,  1699,
    1501, -3011,   174,   238,  2106,   466,  1700,  2637,  1756,  3332,
    3614,  2884,  2688,  2662, -1270,   469, -1270,   741,  3174,   742,
     467,  3210,  1629,  2794,  2795,  2975,   466,  2864,   437,  2814,
    4175,   437,   877,  1248,  2910,   437,  2151,  3310,   446,  2641,
    1640,   467,   779,  3213,  2280,   468,  1874,  1875,    52,  2950,
   -2459,   198,  2150,  1910,  1943,   807,  2221,  2224,  1753,  2227,
    3747,  2491,  2232,  2234,  2533,  2236,   437,   156,  2851,    27,
    2804,  2538,  4250,   248,  2853,  2639,  2649,  2087,   154,   -42,
     154,  3281,  3282,  3283,  2866,  -521,  2770,   468,  3888,   907,
    3488,  2561,   470,   154,   154,  3793,  2482,  2241,  -521,  3450,
       9,   825,    10,  2805,   154, -1938,  3847,  2907,   780,   780,
     656,   154,  2368,  3782,   908,   437,   154,  4005,   807,  1620,
     154,  1876,  1877,   657,    46, -2991,   154,   728,  2393,   782,
    3794,  3702,   799,  2315,   154,   437,   820,  3183,   855,  2316,
     779,  3366,  3476,  2317,   749,  2589,  2591,  2779,  2894,  1045,
     106,  3367,  1753, -3010,   897,   900,   693,  1414,  1811,   897,
    1501,  2914,  2915,   469,  1576,  2369,   929, -3010,   262,  2400,
     154,   263,   264,   468,  3510,   437,  3461,  2754,    34,   427,
    2382,   974,   471,  3533,  1577,  1641,  1642,  4132,  3511,  3477,
    3478,  1745,   154,  3848,   468,   996,   408,  1002,  1005,    29,
     437, -3010,  2868,  3748,  3243,   469,  3849,   217,  3749,   814,
     694,    53,  3850,   814,  3234,    47,  3793,   908, -1697,  2962,
    3921,  3922,   813,  4020,  2955,  1415,   813,  3076,  3077,  3964,
    3965,  4062,  4027,   140,  3782,  2400,   154,  3509, -1270, -2640,
     470,   791,   691,  2869,   695,  4204, -2991,  1144,  3851,    37,
    3512,  3794,  1145,   428,  2772,  4205,  4206,  1902,  1903,   975,
    2399,  2330,  1448,  2116,  3284,  3285,  2331,  2248,   429,  2976,
    2397,  2977,    48,  1144,  4133,    49,  1780,    50,  1145,  3101,
    2978,  2979,   470,  1110,  2980,  2981, -1697,  4134,  3286,   154,
    1838,   469,   869,  4136,  3214,  3244,    51, -2520,  2401,    54,
     106,  1632,   814,    55,  2422,  2369,  2741,  1904,  1905,  3756,
    4053,  3287,   469,  2432,  4054,   813,  2740,  2740,   154,  2249,
    1140,  1140,  1140, -2887,  1781,  2561,  2561,  4207,  2313,  4137,
     471,   465,  1163,  2920,  3908,  3288,  4285,  4063,  4286,  4259,
    4287,  4002,  4003,   780,   174,   780,   799,  4208,  4209,  3085,
    2800,  1921,  2298,   926,  2489, -2936,   933,  3511,  1449,  2539,
    2299,   948,  2789,  3757,  2401,  3111,   807,   218,   470,   780,
     807,  1187,   471,  1578, -2907,  4275,  1450,  1579,  3102,   174,
    2589,  2921,  2589,     9,  3112,    10,  1923,  1501,    52,   470,
    2806,  2324,   780,  1580,  1144,  1581,  1465,  4222,  2092,  1145,
     800,  3758,   408,  4221,   627,   466,  2782,  2494,  2848,  2093,
      56,  4239,  1467,  2817,  2818,  1699,  2840,  1919,  2842,  3512,
     467,   430,  1700,  3779,  2740,  2740,   427,  2545,   106,  2793,
    3289,    99,  3615,  2561,  2561,  2698,    39,   680,  4223,  1146,
    1919,    38,   175,  1255, -2675,   176,   154,    57, -2675,  1147,
    4016,  3909,    42,  1426,  1428,  4260,  4261,  4019,   471,   807,
     149,  4022,  3290,  1036,  4024,  3291,  3292,   154,  2593,  2228,
    1248,   809,    43,  1463,  2229,  3910,  3911,  3912,  3513,   471,
    2593,  1762,  2566,  2567,  2568,  2569,  2570,  2571,  2572,  2573,
      86,  3293,  1144,    58,  1884,    59,  1885,  1145,  2562,  1500,
     428,  1869,  1870,  1871,   793,  -459,  1183,  3087,   814,   814,
     814,  3088,  2922,  1144,   859,   429,   870,    87,  1145,  3057,
     871,   813,   813,   813,  1451,  1140,    60,  2938,  3550,  3551,
    1164,  1536,  3184,  3185,   791,  3709,  2600,  1037,  1144,   431,
    1501,    88,  4105,  1145,  3710,  3191,  3192,  1582,  -459,   872,
    3748,    53,  1039,    91,  1553,  3749,   908, -2936,  1558,  1560,
      95,  2496,  3195,  3196,  4064,  1191,  3063,  3064,  3643,  4065,
     783,   783,   628,   468,    93,   149,   809,  2398,   780,   814,
    1728,  1731,  1734,  4096,  3098,   437,  4097,  2754,  -895,    96,
     464,   908,   813,  1872,  2646,  1144,  4106,  3923,  3924,  2300,
    1145,  2301,  2973,  4185,  4186,  2701,  4081,  1761,  3235,    89,
      90,  -459,  1583,  2709,  2710,  2934,   432,  2497,  2498,  2499,
    2500,  3429,  2501,  4082,    97,  3779,  4100,   104,  2656,  2657,
    3431,  3432,  3433,  -895,  3220,  3943,  2666,   629,  3221,    54,
    4101,   103,   873,  2679,  3949,  -895,  2290,   111,  3294,    61,
    2290,  4262,  3952,  1694,  1695,  2519,  2519,  4083,  1453,  3780,
     106,  1140,  1140,  1140,  2700,   807,   807,   807,   430,  1140,
    1140,  1140,  1744,   112,  3110,  1744,  2755,  2756,  4224,   113,
    2623,  3322,  3086,  2654,   115,  1140,  2221,  1744,  1140,  -895,
     630,   469,   116,   174,  1140,  1140,  1140,  1140,  1140,   124,
     156,  2502,  2165,  1765,  1766,  1041,   433,   130,  2376,  1002,
    2658,  1778,  -895,  2787,  4084,  3852,  4199,  4201,  1144,  2300,
    2681,  2301,  -459,  1145,  1140,  2748,  2749,   779,   779,  1139,
   -2859,  1142,  2562,  2562,  4085,  2693,   807,   133,  3679,  3941,
     857,   137,   631,  2686,  1914,  4190,   139,  2561,  4191,  3941,
     883,  4243,  1741,  1742,  1921,  1873,  3339,  1767,  1768,  1769,
    1837,   914,   916,  1916,  1917,  1918,  -895,   143,   470,  4253,
     145,   146,  2714,  2715,  -459,   154,  2717,   908,   908, -2610,
   -2610, -2610, -2610,   908,   908,   908,   431,    57,   157,  2350,
     908,   908,  2352,   908,  1213,  1214,  1215,  1216,  2865, -2991,
    2358,   165,   169,  2763,  2362,   783,  2738,   783,   183,   184,
    4192,  2365,  2744,   188,  3137,   193,  3742,  1745,   196,   216,
    2810,   220,   221,  1042,   226,  4288,   156,  4193,  -459,  2879,
     814,   783,   258,   259,  1874,  1875,  3386,  3387,   814,   266,
    2562,  2562,  -459,   813,   267,   154,  -895,  1036,   410,   403,
     441,   813,  3400,   442,   783,   450,   451,   632,   471,  3235,
     458,  3780,  -459,   432,   475,  2974,    60, -2609, -2609, -2609,
   -2609,   479,  3872,   486,  2503,  2504,  2505,  2506,  2507,  2508,
    2509,  2178,  2510,   489,   491,  2908,   492,   908,   612,   641,
     908,   652,  4093,   814,   653,   814,   665,   908,   908,   908,
     908,   122,   667,   669,   683,   684,   813,   174,   813,  1876,
    1877,  1131,   689,  -895,   690,   696, -2991,   698,   697,  1898,
    1501,  1037,   700,  1899,  1038,   703,  1900,  1901,  1501,   716,
     717,   880,  3781,   889,   917,  2787,  1039,   925,  4113,  4114,
   -2991, -2991, -2991,   465,  4118,   408,   814,  4156,   814,   975,
    3782,  4157,  4158,   433,  1010,  1465,  4179,  1011,  4184,   813,
    2545,   813,   779,  1012,   779,  1040,  1018,  3783,  1024,  1027,
    3259,  1031,  1029,  1878,  1033,  1842,  1843,  1844,  1845,  1056,
    1140,  1069,  1059,  4225,  4226,   224,  4179,   807,   779,    61,
    1045,  4184,  4229,  -895,  1073,   807,  1086,  1088,  1465,  1103,
    1455, -2991,  2511,  1105,  1131,  1107,  1879,  3347,  1108,  3852,
    1111,   779,  1112,  1113,  1116,  1880,  3338,   466,  1119,  1244,
    1120,  1121,  -895,  1122,  1881,  2632,  2634,  2636,  2638,  1125,
    1127,  2072,   467,  1135,  1137,  1138,  1148,  1155,  1157,  2709,
     783,  1169,  1175,  2080,  1181,  2082,  1183,  1185,  1188,  1190,
     807,  -459,   807,   814,  1193,  -459,  1195,  1197,  1208,  4194,
    1501,  1217,  1221,  1442,  1882,  1222,   813,  1250,  1246,  1443,
    1430,  -459,  1945,  -459,  1440,  1446,  1444,  1468,  1140,  1481,
     908,  1469,  2118,  1471,  1483,  3589,  3590,  3591,  1476,  1041,
    4138,  1486,  2987,  3404,  2989,  1500,  2561,  3663,  3664,  2965,
    1487,  1493,  3333,   807,  3334,   807,  1491,  2986,  1503,  1495,
    3603,  3604,   951,  1507,  1514,  1516,  1528,  -895, -2991,  1530,
    1532,  1538,  1545,  2975,  1524,  1549,  1551,  3080,  1554,  1562,
    1563,  1535,  3020,  1564,  3781,  1131,  1569,  2512,  1568,  2990,
    1570,  1572, -2991, -2991, -2991,  1601,  1630,   149,  1612,  1646,
    1696,  2991,  3782,  1727,  2562,  3624,  3625,  1737,  1738,  2513,
    1749,  1748,  1758,  1763,  1770,  1772,   785,  3050,  1785,  3783,
    3051,  1796,  3052,  1789,  1200,  1199,  1839,  3857,  1849,  3059,
    3060,  3061,  3062,  1841,  1859,   468,  1895,  3067,  -895,  1908,
    1419,  1934,  -895,  1938,  1936,  2992,  2053,   779,  2057,  2061,
    2066,  2865,  2067,  3081,  2068,  2070,  2075,  1042,  3741,  2078,
     807,  2759,   908,  2761,  2085,   437,  2095,  2086,  2993,  2764,
    2765,  2766,  2088,  2710,  2120,  -459,  2771,  2122,  2123,  1622,
    2129,  2135,  2137,  2139,  2142,  2143,  3093,  2783,  2784,  1694,
    2144,  3168,  1697,  1698,  2225,  2146,  2237,  2239,  2244,  2307,
    2308,  2321,  2324,  3864,  3180,  1500,  2326,  2327,  2329,  2333,
    2334,  3690,  2339,  2337,  -895,  2340,  2341,  2351,  2343,  2353,
    2359, -2563,  2994,  2360,  2379,   154,  1091,  2349,  1883,  2361,
    1755,  2383,  1884,  2363,  1885,  2366,  1744,  2377,  2391,  2385,
    -459,   908,  2394,  2406,   814,  2395,   814,  2438,  2440,  2442,
    2443,  2452,  1051,   469,  2447,  2456,  2464,   813,  2465,   813,
    2486,  2493,  2820,   100,  2821,   921,  2547,  2557,  1465,  2548,
    1465,  2561,  2564,  2598,  -895,  2586,  2601,  3596,  2602,  2611,
    2614,  2618,  1699,  2617,  1052,  1053, -1712,  2619,  2623,  1700,
    2622,   814,  1701,  2624,  2639,  2642,  2647,  2650,  2665,  2670,
    2680,  2692,  2995,   222,   813,  2702, -2859,  1131,  4265,  2703,
   -2859,  2705,  3499,  2704,  2718,  1465, -2652,  2976,  2720,  2977,
    1846,  1848,  2722,  1690,  1045,  2369,  2745,  2762,  2978,  2979,
     470, -2639,  2980,  2981,  2368,  1837,  3611,  1902,  1903,  2435,
    2815,  2401,  1244,  1702,  2791,  1703,  1704,  2400,  2854,  -895,
    2867,  2855,  2862,  2875, -2634,   814,  2876,  2881,  2898,  2892,
    2899,  2539,  2538,  2951,  2954,  2925,  2944,  2956,   813,  2997,
    2958,  2966,  2972,  1023,  2971,  2988,  3014,  3253,  3016,  3201,
    3024,  3017,  3027,   791,  3030,  3032,  3633,  1904,  1905,  3034,
    3037,  3069,  3261,   154,  3071,  3075,  3082,  3083,  3073,  3267,
    3096,   807, -2568,   807,  3092, -2637,  -895,  3100,  3135,  2868,
    3141,  2459,  3124,  2561,  2460,  2869,  2461,  2462,  2463,  3225,
     471,  3857,  3148,  3150,  3126,  3155,  1705,  3160,  1706,  1600,
    3157,  3170,  2477,  3309,  3175,  1707,  2478,  2909,  2479,  3176,
    2910,  3080,  3190,  1708,  2488,  3193,  2939,  2940,   807,  2998,
    3205,  3229,   156,  2562,  3230,  3246,  3301,  3302,  3249,  3255,
    3251,  3256,  3303,  3258,  3305,  3319,  3168,  3317,  3271,  3271,
    3330,  3340,  3341,  3348,  3350, -3011,  3369,   156,  3000,   156,
     156,  3364,  3371,  3378,  3379,   156,  4153,   156,  3277,  3214,
    3720,  3307,  3383,  3213,  3406,  3408,  3410,  3311,  3312,  1596,
    3411,  3430,   117,  3304,  -250,  3308,  3457,  3456,  3494,  3495,
    3504,   908,   807,  3538,  1465,  3497,  3717,  3525,  3546,  3548,
    3562,  1465,  3568,  3395,  -250,  3545,  3587,  3463,  3488,   814,
    3487,   814,  3619,  3533,  3532,  3620,  3637,  -250,  3638,  3552,
    2561,  3510,   813,  2822,   813,  3660,  1709,  2056,  3665,  3674,
    3659,  3671,  3717,  3672,  -250,  3681,  3698,  3704,  3686,  3689,
    3707,  3718,  1463,  3721,  3751,  2532,  3420,  3713,  3745,  3743,
    2536,  3752,  3355,  3736,  3753,  2537,  3776,  3774,  3775,  3777,
    3801,  3873,  3866,  3894,  2555,  1500,  3904,  3900,  3903,  3907,
    3913,  3916,  3927,  1710,  3917,  3920,  3874,  1711,  3928,  2319,
    3951,  3955,  3976, -2730,  4115,  4189,  4159,  4094, -1712,  3407,
    4218,  3372,  2094,  4161,  4219,  1140,  1140,  -250,  4274,  4276,
   -2859,  4278,  2595,  2109,  2823,  4277,  4283,  4290,    21,  2607,
    2607,   951,   191,  2710,  1535,   129,   272,  2613,  1071,    36,
     121,  2125,   449,   243,  3002,   461,   123,   483,  3003,  1055,
     246,   244,   681,  1605,  3700,  3579,  2480,  3963,  3058,  3565,
    2280,  3656,  3066,  3072,  3563,   677,  3272,  3905,  3778,  3918,
    2522,  1017,  3425,  2655,  2149,  3023,  4264,  3696,  2562,  3645,
    2485,  3739,  4014,   156,  3031,  4092,   807,  4227,   807,  4228,
    3413,  3636,  4029,  3395,  3738,   156,   909,  3958,  3446,  1915,
    1439,  3428,  3944,  3945,  3428,  4070,  3948,  3129,  3956,  1627,
    2719,   402,  1210,  3428,  3428,  3428,  3436,  1631,  3094,  1637,
    3004,   156,  3090,  1684,  2320,  2381,  2788,  1154,  3099,  3127,
    3595,  3593,   858,  1204,  1160,  1779,   908,  1795,  2416,  3109,
    3452,  2434,  2801,  1212,   910,  1860,  1851,  2847,  3576,  3578,
    3576,  3133,  2446,  1933,  3134,  1907,  2873,  1247,  2466,  3145,
    3144,  3602,  3480,  1937,  2824, -2859, -2859,  1447,  3253,  2524,
    3172,  3618,  3363,  3559,  2535,  2710,  2905,  3179,  3189,  3187,
    3005,  2924,  2574,  2960,  3564,  3492,  1504,  2081,   970,  1505,
    2937,  2608,  3208,  2119,  2609,  3760,  3617,  2551,  2534,  3527,
    3600,  3876,  3975,  3524,  2729,  3891,  2731,  2725,  3629,  2138,
    3892,  3668,  3606,  3627,  2778,  3608,  1804,  2776,  1802,  3212,
    2562,  3200,  2799,  3198,  3506,  2797,  3391,  3389,  3219,  1694,
     963,  1159,   156,   156,  1868,  2448,  2930,  2931,  2245,  1712,
    1490,  2932,  2933,   935,  4060,  1007,  1867,  1774,  3265, -1712,
    1140,  3036,  1140,   249,  3336,  3008,  2563,  3140,  2246,  2468,
    1777,  2742,  1744,  2953,  2713,  3651,  4031,  3947,   701,  3651,
    1058,  2247,  1126,  3653,  2581,  2792,  1140,  3567,   908,  2786,
    -250,  3029,  3203,  3381,  3616,  3669,  3115,  3566,  3581,  3582,
    4252,  3584,  3585,  4055,  2825,  3460,   478,  3159,  1131,  1140,
    1106,  1072,   784,  2588,  3612,  3268,   799,  4061,  4057,  3867,
     791,  4212,  3010,  2231,     0,     0,     0,     0,  3601,     0,
    4076,     0,  1713,  1465,  1645,     0,     0,     0,   902,  3610,
       0,     0,  -250,  1714,  1501,     0,     0,  4166,     0,     0,
       0,     0,     0,     0,  1715,  3641,  3642,  2562,  -250,     0,
    2852,   451,     0,     0,     0,     0,  3712,     0,  3632,     0,
    3621,     0,     0,     0,     0,  1500,  3655,  3655,   156,   156,
    1003,     0,     0,  2477, -1712,     0,  4030,     0,     0,     0,
       0,  3639,     0,     0,     0,     0,     0,     0,     0,   156,
     156,     0,     0,     0,     0,  3755,  2826,     0,     0,     0,
       0,     0,     0,     0,     0,   908,     0,     0,     0,     0,
    1716,     0,     0,     0,     0, -2950,     0,     0,     0,     0,
       0,     0,     0,  2893,     0,     0,  1501,     0,   791,     0,
       0,     0,     0,     0,  3810,     0,     0,     0,     0,     0,
       0,     0,  1717,     0,     0,     0,     0,     0,  -250,     0,
    3576,     0,     0,     0,     0,     0,  -250,     0,     0,     0,
   -2950,     0,     0,     0,     0,     0,   156,  2989,   437,     0,
       0,     0, -2950,     0,     0,     0,     0,  2929,     0,     0,
       0,  3695,  3705,  3706,     0,  1140,  1134,  3703,     0,     0,
       0,     0,     0,   784,   784,   784,  3862,     0,  3746,     0,
       0,  2786,     0,  1151,     0,  1500,     0,     0,   780,   780,
    2073,     0,  2990,     0,     0,     0, -2950,     0,     0,  -250,
       0,   156,     0,     0,  2991,     0,     0,     0,     0,     0,
       0,     0,  -250,     0,     0,     0,     0,  3865,     0, -2950,
       0,     0,     0,   780,  -250,     0,     0,     0,     0,     0,
       0,     0,     0,  3761,     0,     0,     0,     0,  3773,     0,
     156,   780,     0,     0,     0,     0,  1501,  3576,  2992,  3809,
    3858,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  3870,  3871,     0,     0,
       0,  2993,     0, -2950,     0,  3950,  2565,     0,     0,  3877,
       0,  -250,  -250,     0,     0,     0,  3893,  -250,     0,  3896,
    3897,  3898,  3899,     0,  2248,     0,     0,   780,     0,     0,
       0,  -250,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   799,     0,
    -250,     0,     0,  2729,     0,  2994,  1466,     0,  3935,     0,
       0,     0,     0,  2710,  3978,     0,     0,  3942,     0,     0,
    3946,     0,     0,     0,     0,     0,  2249,     0,     0,     0,
       0,     0,  1502, -2950,     0,     0,     0,     0,     0,     0,
       0,     0,  -323,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -250,     0,     0,     0,  -250,   784,     0,
       0,     0,     0,  4021,     0,     0,     0,     0,     0,     0,
       0,     0,  4028,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2995,     0,     0,     0,     0,
       0,     0, -2950,     0,     0,  3983,  3984,  3985,  3986,  3987,
   -2950,     0,  3989,  3990,  3991,  3992,  3993,  3994,  3995,  3996,
    3997,  3998,  3999,   908,     0,  4001,     0,     0,     0,     0,
     908,   729,     0,  4013,   908,     0,   464,   908,     0,     0,
   -2907, -2907, -2907,     0,  4017,     0,  4018,     0,  1230,     0,
       0,     0,  2250,     0,  2996,     0,  4026,     0,     0,     0,
    2251,     0,  2997,     0,     0,     0,     0,     0,  2532,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -2950,     0,     0,     0,     0,     0,     0, -2950,     0,     0,
       0,     0,     0,     0,   784,   784,   784,     0,     0,     0,
       0,     0,   784,   784,   784,  1746,     0,     0,  1746, -2950,
       0,     0,     0,  2252,     0,     0,     0,     0,   784,     0,
    1746,   784, -2907,     0,  3858,     0,  2253,   784,   784,   784,
     784,   784,  2998,     0,     0, -2950,     0,     0,  2254,  2999,
       0,     0,  1003,     0,     0,  4164,   908,     0,     0,   908,
       0,     0,     0,     0,     0,     0,     0,   784,     0,     0,
     730,  3000,     0,     0,     0,     0,   731,   732,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  4103,  4104,     0,
    4109,   734,     0,  4197,     0,     0,   799,  3001,     0,     0,
       0,  1812,  1813,  1814,  1815, -2873,     0,  4112,  1816,     0,
       0,  2255,  4116,  4117,     0,     0,     0,  1108,     0,     0,
       0,     0,     0,  4236,  4237,  2256,  1817,  1818,     0,  4163,
       0,     0,  4165,     0,     0,  4167,     0,     0,     0,     0,
     783,   783,     0,     0,  -320,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1131,     0,     0,   729,     0,     0,     0,     0,   464,
       0,     0,     0,     0,  1819,   783,     0,     0,     0,     0,
       0,  1230,     0,  3328,     0, -2950,     0,     0,     0, -2950,
       0,     0,     0,   783, -2907,     0,     0,  2257,     0,     0,
       0,  2258,     0,     0,     0,     0,     0,     0,  4289,     0,
       0,  1231,     0,  1820,     0,     0,  3345,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2927,     0,  3002,     0,     0,
       0,  3003,     0,     0,     0,     0,     0,     0,     0,   783,
     735,   736,   737,     0,     0,     0,     0,     0,  2936,  2936,
       0, -2950,   738,     0,     0,  2947,     0,     0,     0,     0,
       0,  3380,     0, -2907, -2907,     0,     0,     0,     0,  3384,
       0,  3385,     0,     0,     0,     0,     0,     0,     0,   465,
       0,     0,     0,     0,     0,     0,     0,   779,   779,     0,
       0,     0,     0,   730,     0,     0,     0,     0,     0,   731,
     732,     0,     0,  3004,     0,     0,     0,     0,     0,  1232,
       0, -2950,     0,   784,   734,     0,     0,  1821,     0,     0,
       0,     0,   779,     0, -2950,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   739,   740, -2907, -2907,
     779,     0,     0,     0,   741,     0,   742,     0, -2950,     0,
       0,     0,     0,   466,     0,     0,     0,   743,   744,   745,
       0,     0,     0,  3005,  2074,   746,     0,     0,   467,     0,
    1822,     0,     0,     0,     0,     0,  3006,     0,     0,     0,
       0,     0,     0,   747,     0,     0, -2950,     0,     0,  3475,
       0,     0,     0,     0,     0,     0,   779,     0,     0,     0,
    3007,     0, -2907,     0,     0,     0,     0,  3483,  3486,  1823,
       0,   784,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2469,     0,  1233,     0,     0,  1502,  3502,
       0, -2950,  3505,     0,  1231, -2907,     0,     0,  3008,     0,
       0,     0,     0, -2950, -2907,     0,   748,     0,     0, -2950,
       0, -2950,     0, -2907,     0,     0,     0,     0,     0,  1234,
       0,     0,     0,     0,  3547,     0,     0,  3108,     0,     0,
       0,   749,     0,   735,   736,   737,     0,     0,     0,     0,
       0,     0,     0,  3009,     0,   738,     0,     0,     0,     0,
       0,     0,     0, -2907,     0,  3010,     0,     0,  1824,     0,
       0,  3011,     0,  3012,     0,     0,     0,     0,     0,     0,
       0,     0,   465,     0,     0,  2470,     0,     0,     0,     0,
       0,   468,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  3597,     0,     0,     0,     0,     0,
       0,  3483,  1232,     0,  3486,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   730,     0,     0,
    3173,     0,     0,   731,   732,     0,     0,   750,     0,   739,
     740,     0,     0,     0,     0,     0,     0,   741,   734,   742,
       0,     0,     0,     0,     0,     0,   466,     0,  1502,  1500,
     743,   744,   745,     0,     0,     0,     0,     0,   746,  1825,
       0,   467,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   747,     0,     0,  1746,
       0,     0,     0,     0,     0,     0,     0,   730,     0,     0,
       0,     0,     0,   731,   732,     0,     0,     0,     0,   469,
       0,     0,     0,     0,     0,     0,     0,     0,   734,     0,
       0,     0,     0,     0,     0,     0,  3667,     0,  1233,     0,
       0,     0,     0,     0,   751,   752,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   748,
       0,  1500,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1234,     0,     0,     0,     0,     0,  1671,     0,
    1672,  1673,     0,  1235,   749,  1236,     0, -2907,     0,     0,
       0, -2907,     0, -2907,  1237,  1238,   470,     0,  1239,  1240,
    1826,     0,     0,     0,     0,  1827,     0,     0,     0,     0,
     754,  3708,     0,     0,     0,     0,     0,   735,   736,   737,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   738,
       0,     0,     0,     0,   468,     0,   755,     0,     0,     0,
       0,     0,     0,  1828,   756,  3324,  3325,  3326,  3327,     0,
       0,     0,     0,     0,     0,  1829,   465,     0,     0,     0,
    3764,  3769,     0,     0,     0,     0,     0,     0,   757,     0,
       0,  3788,  3799,     0,   154,     0,  3861,     0,     0,     0,
     750,     0,     0,  3343,  3344,     0,   471,   735,   736,   737,
       0,     0,     0,     0,     0,  1502,     0,     0,     0,   738,
       0,  1500,  3764,     0,     0,     0,     0,  3769,  2471,     0,
       0,     0,     0,  3895,   740,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  3906,     0,
     466,     0,     0,     0,   743,   744,   745,     0,     0,  3919,
       0,     0,   746,     0,     0,   467,     0,     0,  1830,     0,
       0,  3934,   469,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2072,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   751,   752,     0,
       0,     0,     0,  3953,   740,     0,  3954,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     999,     0,     0,     0,   743,   744,   745,     0,     0,     0,
       0,     0,   746,     0,     0,     0,  1235,     0,  1236,     0,
       0,     0,     0,   748,     0,  1466,     0,  1237,  1238,   470,
       0,  1239,  1240,  3977,     0,     0,  3979,  3980,  3981,  3982,
       0,     0,     0,   754,     0,     0,  3988,     0,  1502,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  4000,
       0,     0,     0,     0,     0,  4009,     0,     0,     0,   755,
       0,  3934,  3934,     0,     0,     0,     0,   756,   784,   784,
       0,  3479,     0,  2729,     0,     0,     0,     0,     0,     0,
       0,  1852,  2729,   748,     0,     0,     0,     0,   468,     0,
   -2441,   757,     0,     0,     0, -2441,     0,   154,     0, -2441,
   -2441, -2441,     0,     0,     0,     0,     0, -2441,  1853,   471,
       0,     0,     0,  4033,  4034,  4035,  4036,  4037,  4038,  4039,
    4040,  4041,  4042,  4043,  4044,  4045,  4046,  4047,  4048,  4049,
    4050,  4051,     0,     0,   750,     0,     0,     0,     0,     0,
       0,     0,     0,   799,  3934,     0,     0,     0,     0,     0,
       0,     0,  1852,     0,     0,     0,   799,     0,     0,     0,
       0, -2441,     0,     0,     0,     0, -2441,     0,  4079,     0,
   -2441, -2441, -2441,     0,     0,     0,     0,     0, -2441,  1853,
    4080,     0,     0,     0,     0,     0,     0,  4009,     0,     0,
       0, -2441,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   750,     0,   469,  3598,     0,  4098,
    4099,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  4111, -2441,
       0,   751,   752,  4009,  4009, -2441, -2441,     0,     0,  4009,
       0,  4155,  4009,     0,     0,     0,  4009,  4009,     0,     0,
   -2441,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  4168, -2441,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   470,     0,  4188,     0,     0,     0,     0,
       0,     0,  2472,     0,     0,  1140,  1140,   754,     0,     0,
   -2441,   751,   752,   784,     0,   784, -2441, -2441,     0,     0,
       0,     0,     0,     0,     0,  1746,     0,     0,  4235,     0,
       0, -2441,     0,   755,     0,     0,     0,     0,     0,   784,
    1140,   756,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1140,     0,
    4258,  4258,   784, -2441,     0,   757,     0,     0,     0,     0,
       0,   154,     0,     0,     0,  4263,     0,   754,     0,     0,
   -2441,  4266,     0,   471,     0,     0,     0,     0,     0,     0,
       0,  4269,  4273,     0,     0,     0,     0,  4258,     0,     0,
       0,     0,     0,   755,     0,  4282,     0,     0,     0,     0,
       0,   756,     0,     0,  1140,     0,     0,     0,     0, -2441,
   -2441, -2441,     0,     0,     0,     0,     0,     0,  1502,     0,
       0, -2441,     0,     0, -2441,   757,  1502,     0,     0,     0,
       0,   154, -2441, -2441,     0,     0,     0,     0,     0,     0,
       0, -2441,     0,     0,     0,     0,     0,     0, -2441,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -2441,     0,
   -2441, -2441, -2441,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2441,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2441, -2441, -2441, -2441, -2441, -2441,     0,
       0,     0,     0, -2441,     0, -2441,     0,     0,     0, -2441,
       0,     0, -2441,     0,     0,     0, -2441, -2441, -2441,     0,
       0,     0,     0,     0, -2441,     0,     0, -2441,   784,     0,
       0,  1812,  1813,  1814,  1815,     0,     0,     0,  1816, -2441,
       0,     0, -2441,     0,     0,     0,     0,     0,  1502,     0,
       0,     0,     0,     0,     0,     0,  1817,  1818,     0,     0,
       0, -2441,     0,     0,     0,     0, -2441, -2441, -2441, -2441,
       0,     0,     0,     0, -2441,     0, -2441,     0,     0,     0,
       0,     0,     0, -2441, -2441,     0,     0, -2441, -2441, -2441,
       0,     0,     0,     0, -2441, -2441,     0,     0, -2441,     0,
       0,     0,     0, -2441,  1819, -2441,     0,     0,     0,     0,
       0,     0, -2441, -2441,     0,     0,     0,     0, -2441,     0,
       0,  2445,     0,     0,     0,     0,     0,     0,     0,  1091,
   -2441,     0, -2441,     0,     0,     0,     0, -3010, -3010, -3010,
   -3010,     0,     0,  1820, -3010,     0,     0,     0,     0,     0,
       0,     0, -2441,     0,     0, -2441,     0,     0,     0,     0,
       0,     0, -3010, -3010,     0, -2441,     0,     0,     0,     0,
       0,     0,     0,     0, -2441,     0, -2441,     0,     0,     0,
   -2441,     0,     0, -2441,     0,     0,   884,     0,     0, -2441,
       0,     0,     0,     0,     0, -2441,     0,     0,     0,     0,
   -2441, -2441,     0,     0, -2441, -2441, -2441,     0,     0, -2859,
   -3010,     0, -2441, -2859,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2441,     0,     0, -2441,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -3010,
       0, -2441,     0,     0,     0,     0,     0,  1821,     0,     0,
       0,     0,     0,   464,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1812,  1813,
    1814,  1815,     0,     0,     0,  1816,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -2441, -2441, -2441,     0,
       0,     0,     0,  1817,  1818,     0,     0,     0,     0,     0,
    1822,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2441, -2441,     0,     0,     0,     0,     0,
       0,  1453,     0,     0, -2441,     0,     0,     0,     0,     0,
   -2441, -2441,     0,     0,     0,     0,     0,     0,     0,  1823,
       0,  1819,     0,     0,     0, -2441,     0,     0,     0,     0,
       0,     0, -2441, -3010, -2441,     0, -2441,     0,  1854, -2441,
   -2441,     0, -2441, -2441, -2441, -2441,     0, -2441, -2441,  1855,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -2441,
    1820,     0,     0,     0, -2441, -2441,     0,   730,     0,     0,
       0,     0,     0,   731,   732,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -2441, -3010,     0,   734,     0,
       0,     0,     0, -2441,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2441,     0, -2441,     0, -2441,  1824,  1854,
       0, -2441,     0, -2441, -2441, -2441, -2441, -2441, -2441, -2441,
    1855,     0,     0, -2441,     0, -3010,     0,     0, -2441,     0,
   -2441,     0,     0,     0,     0, -2441,     0,     0,     0,     0,
       0,     0,     0,     0,  2527, -2441,     0,     0,     0,     0,
       0,     0,  2417,     0,     0,     0, -2441,     0,     0,     0,
       0,     0,     0,     0, -2441,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1821,     0,     0,     0,     0,  3021,
       0,     0,     0,     0, -2441, -2441, -2441,     0, -2441,     0,
       0,     0,     0, -2859, -2441,     0, -2441,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -2441, -2441, -2441,  1825,
       0,     0,     0,     0, -3010,     0,     0,     0,     0,     0,
       0,     0,     0, -2441,     0,     0,     0,  1822,     0,     0,
       0,     0,  1094,     0,  2154,  2155,  2156,  2157,  2158,     0,
    2159,  2160,     0,     0,     0,     0,  1454,   735,   736,   737,
       0,   884,     0, -2441,     0,     0,     0,     0, -3010,   738,
   -2441,     0,     0,     0,     0, -2441,  1823,     0,     0, -2441,
   -2441, -2441,     0,     0,     0,     0,     0, -2441,     0,     0,
   -2441, -2441, -2441, -2441,     0,     0,   465,     0, -2441,     0,
   -2441,     0,     0,     0,     0,     0,     0, -2441,     0,     0,
       0, -2441, -2441, -2441,     0,     0,     0,     0,  1671, -2441,
    1672,  1673, -2441,     0,     0, -3010,  2162,     0,  2163,  2164,
    2165,  2166,  2167,  2168,  2169,  2170,  2171, -2441,     0,     0,
    1826,     0,     0,     0,     0,  1827,     0,     0, -2859, -2859,
       0,     0,     0,  1455,   740,     0, -2441,     0,     0,     0,
       0,     0,     0,     0,     0,  1824,     0,     0,     0,     0,
    1456, -2441,     0,     0,   743,   744,   745,     0,     0, -2441,
       0,     0,   746,  1828,     0,   467,     0,     0,     0, -2441,
       0,     0,     0,     0,  2172,  1829,     0,     0, -2441,     0,
   -2441,     0,     0,     0,     0,     0,     0, -2441,  3121, -2441,
       0,     0,     0, -2441,     0, -2441, -2441,     0,     0,     0,
       0,     0,     0,     0, -3010, -2441, -3010, -3010,     0,     0,
   -2441,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -3010, -2441,     0,     0,
       0, -3010,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   748,     0,     0,  1825,     0,     0,     0,
       0,     0,     0,     0,     0, -2441,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1830, -3010,
       0,     0,  3503,     0,     0,     0,     0,     0,     0,     0,
       0, -3010,  2173,  2174,  2175,  2176,  2177,     0,     0,  2178,
    2179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -2441,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2441,     0,     0,     0,     0,   468,     0,
       0,     0,     0,     0,     0,  2180,     0,     0,     0,     0,
   -2441,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1671,     0,  1672,  1673,  2181,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   750,     0,     0,  1826,     0, -2441,
   -2441, -2441,  1827, -2441, -3010,     0,     0,     0,     0,     0,
       0, -2441,     0,     0,     0,     0,     0,     0,  1805,     0,
       0,     0, -2441, -2441,     0,     0,     0,     0, -2441, -2441,
       0,     0,  3613,     0,     0,     0,     0,     0, -2441,     0,
    1828,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1829,     0,     0,     0,     0,     0,     0,     0,
       0,   437,  1502,     0,     0,     0,   469, -2441, -2441, -2441,
       0, -2441,     0,     0,     0, -2441,     0, -2441, -2441, -2441,
   -2441,  2183, -2441, -2441,     0,     0,     0,     0,     0,     0,
       0,   751,   752,     0, -2441, -2441, -2441, -2441, -2441,     0,
       0,     0,     0, -2441,     0, -2441,     0,     0,     0,     0,
       0,     0, -2441,     0, -3010,     0, -2441, -2441, -2441,     0,
   -2441,     0,     0,     0, -2441,     0,     0, -2441, -2441,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2441,   470,     0,  1830,     0,     0,     0,     0,
       0,     0, -2441,     0,  1502,  2185,     0,   754, -2441,     0,
       0, -2441,     0,     0,     0,     0,     0,     0,     0,     0,
   -2441,     0,     0, -1981,     0,  2187,     0,     0,     0,     0,
       0,     0,     0,   755, -2441,     0,     0,     0,     0,     0,
       0,   756,     0,     0, -2441,     0,     0,  2188,     0,     0,
       0,     0, -1981, -2441,     0, -2441,     0,     0,     0,     0,
       0,     0, -2441,     0,     0,   757,     0,     0, -2441,     0,
     437,   154,     0,     0,     0,     0,     0,     0,     0,     0,
   -2441,     0,     0,   471,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2441,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -2441,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1502,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2191,  2192,  2193,     0,     0,     0,
       0,     0, -2745,     0,     0,     0, -2441,  1256,     0,     0,
   -2745,     0,     0,  1257,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -2745,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2074,
       0,     0,     0,  1258, -2745,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2745,  1259,  1260,  1261,  1262,  1263,  1264,
    1265,  1266,     0,     0,     0, -2745, -2745, -2745, -2441,  1267,
   -2745,     0,     0,  1268,     0,     0, -2745,     0,     0,     0,
       0,     0,     0,     0,  1269,  1270,  1271,  1272, -2745,     0,
   -2745, -2745,     0, -2441, -2441,     0,     0,     0,  2195,  2196,
    2197,  1273,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1274,     0,     0,  1275,  1276,  1277, -2745,  1278,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2441,     0, -2441,     0, -2441,     0,     0,     0,
   -2441,     0, -2441, -2441, -2441, -2441,     0, -2441, -2441,     0,
       0,  3022,  1279,     0,     0,     0,     0,     0,     0, -2441,
    1280,  1281,  1282,  1283,  1284,  1285,  1286,  1287,     0, -2745,
       0,  1288,  1289,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2745,     0,     0, -2441,     0,     0,     0,     0,
       0,     0,     0, -2441,     0,     0,  1290,  1291,     0,     0,
    1292,  1293,     0, -2745, -2745,  1294, -2745, -2745,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -2441,     0,     0,
       0,     0,     0, -2441,     0,     0,     0,  1295,     0,     0,
       0,     0,     0,     0,     0, -2441,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -2745,  1296,     0,     0,     0,  1297,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -2745,     0,     0,     0,
    1298,     0,     0,     0,     0,  1299,  1300,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1301,
    1302,  1303,  1304,  1305,     0,     0,  1306,     0,     0,     0,
   -2745, -2745, -2745,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2745,     0,     0,     0,     0, -2745,     0,     0,
       0,     0,     0,     0,     0,  1307,  1308,  1309,  1310,     0,
       0,     0,     0,     0,  1311,  1312,     0,  1313,     0,  1314,
    1315,  1316, -2745, -2745,  1317,     0,  1318,     0, -2745, -2745,
    1319,     0,     0,     0,     0,     0,     0,     0,   784,   784,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1320,
    1321,     0,     0,     0,     0,     0,     0,     0,  1322,  1323,
    1324,  1325,  1326,  1327,     0,     0,     0,     0,     0,     0,
       0,     0,  1328,   784,     0,     0,  1329,     0,     0,     0,
    1331, -2745,     0, -2745,     0,     0,     0,     0,     0,     0,
       0,   784,     0,     0,     0,     0,     0,     0, -2745,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -2745, -2745, -2745,     0,     0,  1332,     0,     0,     0,  1333,
    1334,  1335,  1336,  1337,     0,     0,     0,     0,     0,     0,
    1338,     0,     0,     0,     0,     0, -2745,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1339,   784,     0,     0,
       0,     0,     0,     0,  1340,     0,     0,     0,     0, -2745,
   -2745, -2745,     0, -2745,     0, -2745, -2745,     0, -2745, -2745,
   -2745,     0,     0,  1432, -2745,     0, -2745, -2745, -2745, -2745,
       0,     0,     0,     0,     0,     0,     0,     0,  1341,  1342,
       0,     0, -2745,     0,     0,     0,     0,     0,     0,     0,
   -2745,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2745, -2745,     0,     0,     0,     0,     0,
    1343, -2745,     0,  1344,     0,     0,     0, -2745,     0,     0,
       0,     0,     0,     0,  1345,     0,     0,     0,     0,     0,
    1346,     0,     0,     0,  1347,  1348,  1349,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1350,
       0,     0,  1351,  1352,     0,     0,     0,  1353,     0,     0,
       0,     0, -2745,     0,     0,     0,     0, -2745,     0,     0,
       0,  1354,  1355,  1356,     0,     0,     0,     0,     0,     0,
       0,  1357,  1358,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1359,  1360,
    1361,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2745, -2745, -2745,     0,     0,     0,  1362,  1363,
    1364,  1365, -2745,  1366,     0,     0,     0,  1367,  1368,     0,
       0,     0,     0,     0,     0,  1369,  1370,     0,     0,     0,
       0,     0,     0,  1371,  1372,  1373, -2745,     0,     0,     0,
       0,  1374,     0,     0,   464,     0,  1375,     0,     0, -2745,
   -2745, -2745, -2745,     0,     0,     0,     0,   274,     0,     0,
       0,   275,     0,     0,  1376,   276,     0,     0,     0, -2745,
   -2745,     0,   277,     0,     0,     0,     0,  1377,  1378,     0,
   -2745,   278,     0,  1379,     0,     0,     0,     0,  1433,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1380,  1381,     0,     0, -2745,
       0,     0,     0,     0,  1382,     0,     0,     0,     0,     0,
       0,     0,  1383, -2745, -2745,     0,     0,     0,  1384,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   279,   280,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1385,     0, -2745,     0,     0,     0,
    1386,     0,     0,     0,     0, -2745,     0,     0,     0,     0,
       0,   281, -2745,     0,     0,     0,     0,     0,   730,     0,
       0,     0,     0, -2745,   731,   732,     0, -2745, -2745, -2745,
       0,   282,     0,     0,     0,   283,     0,     0,     0,   734,
    1387,     0,     0,     0, -2745, -2745,  1388,     0,  1389, -2745,
   -2745,  1390, -2745,   284,     0,     0,     0,     0,  1391, -2745,
       0,     0,     0,     0,     0,     0,     0,     0,  1392,     0,
       0,  1393,     0,     0,     0,     0,     0, -1320,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   285,     0,     0,     0,     0,     0,   286,     0,   287,
       0,     0,   288,     0,     0,     0,   289,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   290,     0, -1320,     0,     0,     0, -1320,
   -1320, -1320, -1320, -1320,     0, -1320, -1320, -1320, -1320,     0,
   -1320, -1320,     0, -1320, -1320,     0, -1320, -1320, -1320, -1320,
   -1320, -1320, -1320, -1320, -1320, -1320, -1320,     0,     0,   291,
       0,     0,     0,     0,     0,     0,     0, -1320,     0,     0,
       0,     0, -1320,     0,     0,   292,     0,     0,     0,     0,
   -1320,     0,     0,     0,     0,   293,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   735,   736,
     737,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     738,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     294,     0,     0,     0,     0,     0,     0,   465,   295,     0,
       0,   296,     0,     0,     0,     0,   297,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   298,     0,     0,     0,     0,     0,     0,     0,
     299,     0,   300,     0,     0,   301,     0,     0,   302,     0,
       0,     0,     0, -1320,     0,   740,     0,     0,   274,     0,
       0,     0,   275,   303,     0,     0,   276,     0,     0,     0,
       0,   466,     0,   277,     0,   743,   744,   745,     0,     0,
       0,     0,   278,   746,     0,     0,   467,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   304,     0,     0,     0,     0,     0,
     305,     0,   306,     0,     0,   307,     0,     0,     0,     0,
       0,     0,   279,   280,     0,     0,     0,     0,     0,     0,
     308,     0,     0,     0,     0,     0,     0,     0,     0, -1320,
       0,     0,     0,     0,   748,     0,     0,     0,     0,     0,
       0,     0,   281,     0,     0,     0,     0, -1320, -1320, -1320,
       0, -1320, -1320, -1320, -1320,     0,     0,     0,     0,     0,
       0,     0,   282,     0,     0,     0,   283,     0,     0,     0,
       0,   309,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   284,     0,   274,     0,     0,     0,
     275,     0,     0,   310,   276,     0,     0,     0,     0,     0,
       0,   277,     0,     0,     0,     0,     0,     0,     0,   468,
     278,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   285,     0,     0,     0,     0,     0,   286,     0,
     287,     0,   311,   288,     0,     0,     0,   289,   312,     0,
       0,     0,   313,     0,     0,   314,   315,     0,     0,     0,
       0,     0,     0,     0,   290,   750,     0,     0,     0,     0,
       0,   316,     0,     0,     0,     0,     0,     0,     0,     0,
     279,   280,     0,     0,   317,     0,     0,     0,     0,   318,
       0,     0,     0,     0,     0,     0,   319,     0,     0,     0,
     291,     0,   320,     0,     0,     0,     0,     0,     0,     0,
     281,     0,     0,     0,     0,     0,   292,     0,     0,     0,
       0,   321,     0,     0,     0,     0,   293,     0,     0,     0,
     282,     0,     0,     0,   283,   322,     0,   469,     0,     0,
     730,   323,     0,     0,     0,     0,   731,   732,     0,     0,
       0,     0,   284,     0,     0,     0,     0, -1320,     0,   324,
       0,   734,   751,   752,     0,     0,     0,     0,     0,     0,
       0,   294,     0,   325,     0,     0,     0,     0,     0,   295,
       0,     0,   296,   326,   327,     0,     0,   297,     0,     0,
     285,   328,     0,     0,   329,     0,   286,     0,   287,     0,
       0,   288,     0,     0,     0,   289,     0,     0,     0,   330,
       0,     0,     0,   298,   470,     0,     0,     0,     0,     0,
       0,   299,   290,   300,     0,     0,   301,     0,   754,   302,
       0, -1320,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -1320,     0,   303,     0,     0,     0,     0,     0,
       0,     0,   332,     0,   755,     0,     0,     0,   291,     0,
       0,     0,   756,     0,   333,     0,     0,     0,     0,     0,
     334,     0,     0,     0,   292,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   293,     0,   757,     0,     0,     0,
       0,     0,   154,     0,   335,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   471,   304,     0,     0,     0,     0,
       0,   305, -1320,   306,     0,     0,   307,     0,     0,     0,
     735,   736,   737,   336,     0,     0,   337,     0,     0,   294,
       0,   308,   738,     0,     0,     0,     0,   295,     0,     0,
     296,     0,     0,     0,     0,   297,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   298,     0,     0,     0,     0,     0,     0,     0,   299,
       0,   300,   309,     0,   301,     0,     0,   302,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   274,     0,     0,
       0,   275,   303,     0,   310,   276,     0,     0,     0,     0,
       0,     0,   277,     0,     0,     0,   739,   740,     0,     0,
       0,   278,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  3498,     0,     0,     0,   743,   744,   745,
       0,     0,     0,   311,     0,   746,     0,     0,     0,   312,
       0,     0,     0,   313,     0,     0,   314,   315,     0,     0,
       0,     0,     0,   304,     0,     0,     0,     0,     0,   305,
       0,   306,   316,     0,   307,     0,     0,     0,     0,     0,
       0,   279,   280,     0,     0,   317,     0,     0,     0,   308,
     318,     0,     0,     0,     0,     0,     0,   319,     0,     0,
       0,     0,     0,   320,     0,     0,     0,     0,     0,     0,
       0,   281,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   321,     0,     0,     0,   748,     0,     0,     0,
       0,   282,     0,     0,     0,   283,   322,     0,     0,     0,
     309,     0,   323,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   284,     0,     0,     0,     0,     0,     0,
     324,     0,   310,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   325,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   326,   327,     0,     0,     0,     0,
       0,   285,   328,     0,     0,   329,     0,   286,     0,   287,
       0,   311,   288,     0,     0,     0,   289,   312,     0,     0,
     330,   313,     0,     0,   314,   315,     0,     0,     0,     0,
       0,     0,     0,   290,     0,     0,     0,     0,     0,     0,
     316,     0,   331,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   317,     0,     0,     0,   750,   318,     0,
       0,     0,     0,   332,     0,   319,     0,     0,     0,   291,
       0,   320,     0,     0,     0,   333,     0,     0,     0,     0,
       0,   334,     0,     0,     0,   292,     0,     0,     0,     0,
     321,     0,     0,     0,     0,   293,     0,     0,     0,     0,
       0,     0,     0,     0,   322,   335,     0,     0,     0,     0,
     323,     0,     0,   498,     0,     0,     0,   499,     0,     0,
       0,     0,     0,     0,   500,     0,     0,     0,   324,     0,
       0,     0,     0,   501,   336,     0,     0,   337,     0,     0,
     294,     0,   325,     0,     0,     0,     0,     0,   295,     0,
       0,   296,   326,   327,   751,   752,   297,     0,     0,     0,
     328,     0,     0,   329,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   330,     0,
       0,     0,   298,     0,     0,     0,     0,     0,     0,     0,
     299,     0,   300,   502,   503,   301,     0,     0,   302,     0,
    1544,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   303,     0,     0,     0,     0,     0,     0,
     754,   332,     0,   504,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   333,     0,     0,     0,     0,     0,   334,
       0,     0,     0,   505,     0,     0,   755,   506,     0,     0,
       0,     0,     0,     0,   756,     0,     0,     0,     0,     0,
       0,     0,     0,   335,     0,   507,     0,     0,     0,     0,
       0,     0,     0,     0,   304,     0,     0,     0,   757,     0,
     305,     0,   306,     0,   154,   307,     0,     0,     0,     0,
       0,     0,   336,     0,     0,   337,     0,     0,     0,     0,
     308,     0,     0,   508,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   509,     0,     0,     0,   510,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   511,     0,     0,
       0,     0,   512,   513,   514,   515,     0,   516,   517,   518,
     519,   309,   520,     0,   521,   522,   523,     0,   524,   525,
     526,   527,   528,   529,   530,   531,   532,   533,   534,     0,
       0,   535,     0,   310,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   536,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   537,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   311,     0,     0,     0,     0,     0,   312,     0,
       0,     0,   313,     0,     0,   314,   315,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   316,   538,     0,     0,     0,     0,     0,     0,     0,
     539,     0,     0,   540,   317,     0,     0,     0,   541,   318,
       0,     0,     0,     0,     0,     0,   319,     0,     0,     0,
       0,     0,   320,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   542,   729,     0,     0,     0,     0,
     464,   321,   543,     0,   544,     0,     0,   545,     0,     0,
     546,     0,     0,     0,     0,   322,     0,     0,     0,     0,
       0,   323,     0,     0,     0, -1640,     0,     0,     0,     0,
       0,     0,     0, -1640, -1640, -1640, -1640,     0,     0,   324,
   -1640,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   325,     0,     0,     0,     0, -1640, -1640,
       0,     0,     0,   326,   327,   729,     0,     0,     0,     0,
     464,   328,     0,     0,   329,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   547,     0,     0,   330,
       0,     0,   548,     0,   549,     0,     0,   550,     0,     0,
       0,     0,     0,     0,     0, -1640, -1640,     0,     0, -1640,
       0,     0,   551,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   332,     0,   730,     0,     0,     0,     0,     0,
     731,   732,     0,     0,   333, -1640,     0,     0,     0,     0,
     334,     0,     0,     0,     0,   734,     0,     0,     0,     0,
       0,     0,     0,   552,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   335,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   553,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   336,   730,     0,   337,     0,     0,     0,
     731,   732,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   734,     0,     0,     0,     0,
       0,     0,     0,     0,   554,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   555,     0,     0,     0,     0,     0, -1640,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   556,     0,     0,     0,     0,     0,     0,   557,     0,
       0,     0,     0,     0,   558,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   559,     0,     0,     0,     0,     0,     0,
       0,     0, -1640,     0,   735,   736,   737,     0,     0,     0,
       0,     0,     0,   560,     0,     0,   738, -1546,     0,   828,
       0,     0, -1546,     0,     0,     0,     0,     0,     0,     0,
       0,   561,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -1640,     0,   465,     0,   562,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   563,   564,     0,     0,     0,
       0,     0,     0,   565,     0,     0,   566,     0,   829,     0,
       0,     0,     0,     0,   735,   736,   737,     0,     0,     0,
       0,   567,     0,     0,     0,     0,   738,     0,     0,  1622,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     739,   740,     0,   568,     0,     0,     0,     0,   741, -1640,
     742,     0,     0,   465,     0,     0,     0,   466,     0,     0,
       0,   743,   744,   745,   569,   830,     0,     0,     0,   746,
   -1640,     0,   467,     0,     0,     0,   570,     0,     0,     0,
       0,     0,   571,     0,     0,     0,     0,   747, -1640,     0,
     729,     0,     0,     0,     0,   464, -1546,     0,     0,     0,
       0, -2867, -1546, -1546,     0,     0,     0,     0,     0,     0,
     739,   740,     0,     0, -1640,     0,     0, -1546,   741,     0,
     742,     0,     0,     0,     0,     0,     0,   831,     0,     0,
       0,   743,   744,   745,     0,   572,     0,     0,   573,   746,
       0,     0,   467,     0,     0,     0,     0,     0,     0,     0,
     748,     0,     0,     0,   832,     0,     0,   747,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -1640,     0,     0,     0,   749,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -1640, -1640,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     748,     0,     0,     0,     0,   468,     0,   833,     0,   730,
       0,     0,     0,     0,     0,   731,   732,     0,     0,     0,
       0,     0,     0,     0,     0,   749,     0,     0,     0,     0,
     734,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -1640,   750, -1640, -1640,     0,     0, -1546, -1546, -1546,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -1546,     0,
       0,   729, -1640,     0,     0,   468,   464, -1640,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -1546,     0,     0,     0,     0,
       0,     0,     0,     0,   729,     0,     0,     0,     0,   464,
       0,     0,     0,     0,     0, -1640,     0,     0,     0,     0,
       0,   750,     0,   469,     0,     0,     0, -1640,     0,     0,
       0,     0,     0,     0,     0, -3010,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   751,   752,
       0,     0, -1546, -1546,     0,     0,     0,     0,     0,     0,
   -1546,     0, -1546,     0,     0,     0,     0,     0,     0, -1546,
       0,     0,     0, -1546, -1546, -1546,     0,     0,     0,     0,
       0, -1546,     0,     0, -1546,     0,     0,     0,     0,   735,
     736,   737,     0,   469,     0,     0,     0,   834,     0, -1546,
     470,   738,     0,   597,     0,     0,     0,     0,     0,     0,
     730,     0,     0,     0,   754,     0,   731,   732,   751,   752,
   -1640,   835,     0,     0,     0,     0,     0,     0,   465,     0,
       0,   734,     0,     0, -1640,     0,     0,     0,     0,     0,
     755,     0,     0,   730,     0,     0,     0,     0,   756,   731,
     732,     0,     0,     0,     0,     0,     0,     0,   733,     0,
       0,     0, -1546,     0,   734,     0,   836,     0,     0, -1546,
     470,     0,   757,     0,     0,     0,     0, -1640,   154,     0,
       0,     0,     0,     0,   754,   739,   740, -1546,     0,     0,
     471,     0,     0,   741,     0,   742,     0,     0,     0,     0,
       0,     0,   466,     0,     0,     0,   743,   744,   745,     0,
     755,     0,     0,     0,   746,   729,     0,   467,   756,     0,
     464,     0,     0,     0,  2090,     0,     0,     0,     0,     0,
       0,     0,   747,     0,     0,     0,     0, -1546,     0,     0,
       0,     0,   757,     0,     0,     0,   837,     0,   154,     0,
       0,     0,     0,     0,     0,     0,     0, -2867,     0,     0,
     471,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -3010,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -1546,     0,     0,     0,     0,     0,     0,
     735,   736,   737,     0,     0,   748,     0,     0,     0,     0,
       0,     0,   738,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     749,     0,     0,   735,   736,   737,     0,     0,     0,   465,
       0,     0,     0,     0,     0,   738,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -1546,     0,     0,
       0,     0,     0,     0,   730, -1546,     0,     0,     0,     0,
     731,   732,   465,     0,     0,     0,     0,     0,     0,   981,
     468,     0,   729,     0,     0,   734,     0,   464,     0,     0,
   -1546, -1546,     0,     0,     0,     0,   739,   740,     0,     0,
       0,     0,     0,     0,   741,     0,   742,     0,     0,     0,
       0,     0,     0,   466,     0,     0,     0,   743,   744,   745,
       0,     0,     0,     0,   598,   746,   750,     0,   467,   739,
     740,     0,     0,     0,     0,     0,     0,   741,     0,   742,
       0,     0, -1546,   747,     0,     0,   466,     0,     0,     0,
     743,   744,   745,     0,     0,     0, -1546,     0,   746,     0,
       0,   467,     0,     0,     0,     0, -1546,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   747,     0,     0,     0,
       0,     0, -1546,     0,     0,     0,     0,     0,     0,     0,
   -1546,     0,     0,     0,     0,     0,     0, -2867,   469,     0,
       0,     0,     0,     0,     0,     0,   748,     0,     0,     0,
       0,     0,     0,     0, -1546,     0,     0,     0,     0,     0,
   -1546,   730,     0,   751,   752,     0,     0,   731,   732,     0,
       0,   749, -1546,     0,     0,     0,   912,     0,     0,   748,
       0,     0,   734,     0,   735,   736,   737,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   738,     0,     0,     0,
       0,     0,     0,     0,   749,     0,     0,     0,     0,   729,
       0,     0,     0,     0,   464,   470,     0,     0,     0,     0,
       0,   468,     0,   465,     0,     0,     0,     0,     0,   754,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   468,   755,     0,     0,     0,     0,
       0,     0,     0,   756,     0,     0,     0,   750,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   729,
     739,   740,     0,     0,   464,     0,     0,   757,   741,     0,
     742,     0,   437,   154,     0,     0,     0,   466,     0,     0,
     750,   743,   744,   745,     0,   471,     0,     0,     0,   746,
       0,     0,   467,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   747,     0,     0,
       0,  2091,     0,     0,     0,     0,     0,     0,     0,   469,
       0,   735,   736,   737,     0,     0,     0,     0,   730,     0,
       0,     0,     0,   738,   731,   732,     0,     0,     0,     0,
       0,     0,     0,     0,   751,   752,     0,     0,     0,   734,
       0,     0,   469,     0,     0,     0,     0,     0,     0,     0,
     465,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     748,     0,     0,     0,     0,     0,     0,   751,   752,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2092,
       0,     0,   753,     0,     0,   749,   470,     0,   730,     0,
    2093,     0,     0,     0,   731,   732,     0,     0,     0,     0,
     754,     0,     0,     0,     0,     0,     0,   739,   740,   734,
       0,     0,     0,     0,     0,   741,     0,   742,     0,   470,
       0,     0,     0,     0,   466,     0,   755,     0,   743,   744,
     745,     0,     0,   754,   756,   468,   746,   729,     0,   467,
       0,     0,   464,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   747,     0,     0,     0,   757,   755,
       0,     0,     0,     0,   154,     0,     0,   756,     0,     0,
       0,     0,     0,   729,     0,     0,   471,     0,   464,     0,
       0,   750,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   757,     0,     0,     0,     0,     0,   154,   735,   736,
     737,     0,     0,     0,     0,     0,     0,     0,     0,   471,
     738,     0,     0,     0,     0,     0,     0,   748,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   465,     0,     0,
       0,     0,   749,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   469,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   735,   736,
     737,     0,     0,     0,     0,     0,   730,     0,   751,   752,
     738,     0,   731,   732,     0,     0,     0,     0,     0,     0,
       0,     0,   468,   982,   739,   740,     0,   734,     0,     0,
       0,     0,   741,     0,   742,     0,     0,   465,     0,     0,
       0,   466,   730,     0,     0,   743,   744,   745,   731,   732,
       0,     0,     0,   746,   729,     0,   467,     0,     0,   464,
     470,     0,     0,   734,     0,     0,     0,     0,   750,     0,
       0,   747,     0,     0,   754,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   739,   740,     0,     0,     0,     0,
     755,     0,   741,     0,   742,     0,     0,     0,   756,     0,
       0,   466,     0,     0,     0,   743,   744,   745,     0,     0,
       0,     0,     0,   746,     0,     0,   467,     0,     0,     0,
       0,     0,   757,     0,   748,     0,     0,     0,   154,     0,
     469,   747,     0,     0,     0,     0,     0,     0,     0,     0,
     471,     0,     0,     0,     0,     0,     0,     0,     0,   749,
       0,     0,     0,     0,     0,   751,   752,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   735,   736,   737,     0,
       0,     0,  1540,   730,     0,     0,     0,     0,   738,   731,
     732,     0,     0,     0,   748,     0,     0,     0,     0,   468,
       0,   833,     0,     0,   734,     0,     0,   470,     0,     0,
       0,     0,   735,   736,   737,   465,     0,     0,     0,   749,
       0,   754,     0,     0,   738,     0,     0,     0,     0,     0,
       0,  1752,     0,     0,     0,     0,   464,     0,     0,     0,
       0,     0,     0,     0,     0,   750,     0,   755,     0,     0,
       0,   465,     0,     0,     0,   756,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   468,
       0,     0,   739,   740,     0,     0,     0,     0,     0,   757,
     741,     0,   742,     0,     0,   154,     0,     0,     0,   466,
       0,     0,     0,   743,   744,   745,     0,   471,     0,     0,
       0,   746,     0,     0,   467,     0,     0,     0,   739,   740,
       0,     0,     0,     0,     0,   750,   741,   469,   742,   747,
       0,     0,     0,     0,     0,   466,     0,     0,     0,   743,
     744,   745,     0,     0,     0,     0,     0,   746,     0,     0,
     467,     0,   751,   752,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   747,     0,     0,     0,     0,
       0,     0,     0,   735,   736,   737,     0,     0,     0,     0,
     730,     0,     0,     0,     0,   738,   731,   732,     0,  1129,
       0,     0,   748,     0,     0,     0,     0,   469,     0,     0,
       0,   734,     0,     0,   470,     0,     0,     0,     0,     0,
       0,     0,   465,     0,     0,     0,     0,   749,   754,     0,
       0,     0,   751,   752,     0,     0,     0,     0,   748,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   755,     0,     0,     0,     0,     0,
       0,     0,   756,   749,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   468,     0,   739,
     740,     0,     0,     0,   470,     0,   757,   741,     0,   742,
       0,     0,   154,     0,     0,     0,   466,     0,   754,     0,
     743,   744,   745,     0,   471,     0,     0,     0,   746,     0,
       0,   467,     0,   468,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   750,   755,     0,   747,     0,     0,     0,
       0,     0,   756,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   757,     0,     0,   750,
       0,     0,   154,     0,     0,     0,     0,     0,     0,     0,
     735,   736,   737,     0,   471,     0,     0,     0,     0,     0,
       0,     0,   738,     0,     0,     0,     0,     0,     0,   748,
       0,     0,     0,     0,     0,   469,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   465,
       0,     0,     0,     0,   749,     0,     0,     0,     0,     0,
     751,   752,     0,  2945,     0,     0,     0,     0,     0,     0,
       0,   469,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   751,   752,     0,     0,
       0,     0,     0,     0,   468,     0,   739,   740,     0,     0,
       0,     0,   470,     0,   741,     0,   742,     0,     0,     0,
       0,     0,     0,   466,     0,     0,   754,   743,   744,   745,
       0,     0,     0,     0,     0,   746,     0,     0,   467,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   470,     0,
     750,     0,   755,   747,     0,     0,     0,     0,     0,     0,
     756,     0,   754,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   757,     0,     0,     0,   755,     0,
     154,     0,     0,     0,     0,     0,   756,     0,  1256,     0,
       0,     0,   471,     0,  1257,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   748,     0,     0,     0,
     757,     0,   469,     0,     0,     0,   154,     0,     0,     0,
       0,     0,     0,     0,  1258,     0,     0,     0,   471,     0,
       0,   749,     0,     0,     0,     0,     0,   751,   752,     0,
       0,     0,     0,     0,     0,  1259,  1260,  1261,  1262,  1263,
    1264,  1265,  1266,     0,     0,     0,     0,     0,     0,     0,
    1267,     0,     0,     0,  1268,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1269,  1270,  1271,  1272,     0,
       0,   468,     0,     0,     0,     0,     0,     0,     0,   470,
       0,     0,  1273,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1274,   754,   464,  1275,  1276,  1277,     0,  1278,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   750,     0,   755,
       0,     0,     0,     0,     0,     0,     0,   756,     0,     0,
       0,     0,     0,  1279,     0,     0,     0,     0,     0,     0,
       0,  1280,  1281,  1282,  1283,  1284,  1285,  1286,  1287,     0,
       0,   757,  1288,  1289,     0,     0,     0,   154,     0,     0,
       0,     0,  1453,     0,     0,     0,     0,     0,     0,   471,
       0,     0,     0,     0,     0,     0,     0,  1290,  1291,     0,
       0,  1292,  1293,     0,     0,     0,  1294,     0,     0,   469,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1295,     0,
       0,     0,     0,     0,   751,   752,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   730,     0,
       0,     0,  1296,     0,   731,   732,  1297,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   734,
       0,  1298,     0,     0,     0,     0,  1299,  1300,     0,     0,
       0,     0,     0,     0,     0,     0,   470,     0,     0,     0,
    1301,  1302,  1303,  1304,  1305,     0,     0,  1306,     0,     0,
     754,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   755,     0,     0,     0,
       0,     0,     0,     0,   756,     0,  1307,  1308,  1309,  1310,
       0,     0,     0,     0,     0,  1311,  1312,     0,  1313,     0,
    1314,  1315,  1316,     0,     0,  1317,     0,  1318,   757,     0,
       0,  1319,     0,     0,   154,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   471,     0,     0,     0,
    1320,  1321,     0,     0,     0,     0,     0,     0,     0,  1322,
    1323,  1324,  1325,  1326,  1327,     0,     0,     0,     0,     0,
       0,     0,     0,  1328,     0,     0,     0,  1329,     0,     0,
       0,  1331,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1454,   735,   736,
     737,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     738,     0,     0,     0,     0,     0,  1332,     0,     0,     0,
    1333,  1334,  1335,  1336,  1337,     0,     0,     0,     0,     0,
       0,  1338,     0,     0,     0,     0,     0,   465,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1339,     0,     0,
       0,     0,     0,     0,     0,  1340,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1432,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1341,
    1342,     0,     0,     0,  1455,   740,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   466,     0,     0,     0,   743,   744,   745,     0,     0,
       0,  1343,     0,   746,  1344,     0,   467,     0,     0,     0,
       0,     0,     0,     0,     0,  1345,     0,     0,     0,     0,
       0,  1346,     0,     0,     0,  1347,  1348,  1349,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1350,     0,     0,  1351,  1352,     0,     0,     0,  1353,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1354,  1355,  1356,     0,     0,     0,     0,     0,
       0,     0,  1357,  1358,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   748,     0,     0,     0,     0,  1359,
    1360,  1361,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1362,
    1363,  1364,  1365,     0,  1366,     0,     0,     0,  1367,  1368,
       0,     0,     0,     0,     0,     0,  1369,  1370,     0,     0,
       0,     0,     0,     0,  1371,  1372,  1373,     0,     0,     0,
       0,     0,  1374,     0,     0,     0,     0,  1375,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   468,
       0,     0,     0,     0,     0,  1376,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1256,     0,     0,  1377,  1378,
       0,  1257,     0,     0,  1379,     0,     0,     0,     0,  1433,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   750,  1380,  1381,     0,     0,
       0,  1258,     0,     0,     0,  1382,     0,     0,     0,     0,
       0,     0,     0,  1383,     0,     0,     0,     0,     0,  1384,
       0,     0,  1259,  1260,  1261,  1262,  1263,  1264,  1265,  1266,
       0,     0,     0,     0,     0,     0,     0,  1267,     0,     0,
       0,  1268,     0,     0,     0,  1385,     0,     0,     0,     0,
       0,  1386,  1269,  1270,  1271,  1272,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   469,     0,  1273,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1274,
       0,     0,  1275,  1276,  1277,     0,  1278,     0,     0,     0,
       0,  1387,   751,   752,     0,     0,     0,  1388,     0,  1389,
       0,     0,  1390,     0,     0,     0,     0,     0,     0,  1391,
       0,     0,     0,     0,     0, -3010,     0,     0,     0,  1392,
    1279,     0,  1393,     0,     0,     0,     0,     0,  1280,  1281,
    1282,  1283,  1284,  1285,  1286,  1287,     0,     0,     0,  1288,
    1289,     0,     0,     0,   470,     0,     0,     0,     0,   730,
       0,     0,     0,     0,     0,   731,   732,     0,   754,     0,
       0,     0,     0,     0,  1290,  1291,     0,     0,  1292,  1293,
     734,     0,  1091,  1294, -1981,     0,     0,     0,     0,     0,
   -3010, -3010, -3010, -3010,   755,     0,     0, -3010,     0,     0,
       0,     0,   756,     0,     0,  1295,     0,     0,     0,     0,
       0,     0,     0, -1981,     0, -3010, -3010,     0,  2820,     0,
    2821,     0,     0,     0,     0,     0,   757,     0,     0,  1296,
       0,   437,   154,  1297,     0,     0,     0,     0,     0,     0,
       0,     0, -1712,     0,   471,     0,     0,     0,  1298,     0,
       0,     0,     0,  1299,  1300,     0,     0,     0,     0,     0,
       0,     0, -2859, -3010,     0,     0, -2859,  1301,  1302,  1303,
    1304,  1305,     0,     0,  1306,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -3010,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1307,  1308,  1309,  1310,     0,     0,     0,
       0,     0,  1311,  1312,     0,  1313,     0,  1314,  1315,  1316,
       0,     0,  1317,     0,  1318,     0,     0,     0,  1319,   735,
     736,   737,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   738,     0,     0,     0,     0,     0,  1320,  1321,     0,
       0,     0,     0,     0,     0,     0,  1322,  1323,  1324,  1325,
    1326,  1327,     0,     0,     0,     0,     0,     0,   465,     0,
    1328,     0,     0,     0,  1329,  1330,     0,     0,  1331,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -3010,     0,     0,     0,
       0,     0,     0,  1332,     0,     0,     0,  1333,  1334,  1335,
    1336,  1337,     0,     0,     0,     0,   740,     0,  1338,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   466,     0,  1339,     0,   743,   744,   745,     0,
       0,     0,  1340,     0,   746,     0,     0,   467,     0, -3010,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1341,  1342,     0,  2822,
       0,     0,     0,     0,     0,     0,     0,     0, -3010,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1343,     0,
       0,  1344,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1345,     0,     0,   748,     0,     0,  1346,     0,
       0,     0,  1347,  1348,  1349,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -1712,     0,     0,  1350,     0,     0,
    1351,  1352,     0,     0,     0,  1353, -2859,     0,     0,     0,
    2823,     0,     0,     0,     0,     0,     0,     0,     0,  1354,
    1355,  1356,     0,     0,     0,     0,     0, -3010,     0,  1357,
    1358,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1359,  1360,  1361,     0,
     468,     0,     0,     0,     0,     0,     0,     0,  3634,     0,
       0,     0,     0,     0,  -532,     0,  1362,  1363,  1364,  1365,
   -3010,  1366,     0,     0,     0,  1367,  1368,     0,     0,     0,
       0,     0,     0,  1369,  1370,     0,     0,     0,     0,     0,
       0,  1371,  1372,  1373,     0,     0,   750,     0,     0,  1374,
    -532,     0,     0,     0,  1375,  -532,  -532,  -532,  -532,  -532,
       0,     0,  -532,  -532,  -532,  -532,  -532,  -532,     0,  -532,
    -532,  -532,  1376,     0,     0,     0,     0,     0, -3010,     0,
    -532,  -532,     0,     0,     0,  1377,  1378,     0,     0,     0,
       0,  1379,     0,     0,     0,     0,     0,     0,     0,  -532,
    2824, -2859, -2859,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1380,  1381,  -532,     0,     0,   469,     0,
    -532,     0,  1382,     0,     0,     0,     0,  -532,  -532,     0,
    1383,  -532,     0,     0,     0,     0,  1384,     0,  -532,     0,
       0,     0,     0,   751,   752,  -532,     0,  -532,  -532,  -532,
    -532,  -532,  -532,  -532,  -532,  -532,     0,     0,     0,     0,
       0,     0,  1385,     0,     0,     0,     0,     0,  1386,     0,
       0,     0,     0,     0,     0,  -532,     0, -3010,     0, -3010,
   -3010,     0,     0,     0,     0, -1712,     0,     0,     0,     0,
       0,     0,  -532,     0,     0,   470,     0,     0,     0, -3010,
       0,     0,     0,     0, -3010,     0,     0,     0,  1387,   754,
       0,     0,     0,  -532,  1388,     0,  1389,     0,     0,  1390,
       0,     0,     0,     0,     0,     0,  1391,     0,     0,     0,
    2825,     0,     0,     0,     0,   755,  1392,     0,     0,  1393,
       0,     0, -3010,   756,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -3010,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   757,     0,     0,
       0,     0,  -532,   154,     0,     0,     0,     0,     0,     0,
       0,  -532,     0,     0,     0,   471,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -1712,     0,  1020,     0,     0,     0,     0,     0,     0,     0,
       0,  -532,  -532,  -532,  -532,  -532,     0,     0,  -532,  -532,
       0,     0,  2826,     0,  -532,     0,     0,     0,     0,  -532,
       0,     0,  -532,     0,  -532,     0,     0, -3010,     0,     0,
       0,  -532,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -532,     0,     0,  -532,     0,     0,     0,     0,     0,
       0,     0,     0,  -532,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -532,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   437,  -532,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -532,     0,     0,     0,  -532,
       0,     0,     0,     0,     0,     0,     0,  -532,     0,     0,
    -532,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -532,     0,     0,     0,  -532,     0,     0,  -532,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -532,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -532,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -532,     0,     0,     0,     0,
       0,     0,  -532,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -532,     0,  -532,     0,  -532,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -532,  -532,     0,     0,     0,     0,     0,
       0,     0,  4058,     0,     0,  -532,     0,     0,     0,  -532,
       0,     0,     0,     0,  -532,     0,  -532,  -532,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -532,     0,     0,  -532,
       0,     0,     0,     0,  -532,     0,     0,     0,  -532,     0,
       0,  1019,     0,     0,     0,     0,     0,  -532,  -532,  -532,
    -532,  -532,     0,  -532,  -532,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -532,     0,     0,     0,  -532,     0,
       0,     0,  -532,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -532,     0,  -532,  -532,     0,     0,     0,     0,
       0,     0,     0,  -532,     0,     0,  -532,  -532,  -532,  -532,
    -532,     0,  -532,  -532,     0,     0,     0,     0,     0,     0,
    -532,  -532,     0,  -532,  -532,  -532,     0,  -532,     0,     0,
       0,  -532,     0,     0,     0,     0,     0,     0,     0,  -532,
       0,  -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,
       0,  -532,     0,  -532,  -532,  -532,     0,  -532,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -532,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -532,     0,     0,     0,     0,     0,     0,     0,  -532,     0,
    -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,  -532,     0,
       0,     0,  -532,     0,     0,     0,     0,  -532,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -532,     0,     0,
       0,     0,  -532,     0,     0,     0,     0,     0,  -532,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -532,     0,     0,     0,  -532,  -532,     0,     0,     0,
       0,     0,  -532,     0,     0,     0,     0,  -532,  -532,  -532,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -532,     0,     0,     0,     0,     0,     0,
       0,  -532,     0,     0,     0,     0,     0,  -532,     0,     0,
       0,     0,     0,     0,     0,     0,  1020,     0,     0,     0,
       0,     0,  -532,     0,     0,  -532,  -532,  -532,  -532,  -532,
     165,     0,  -532,  -532,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -532,  -532,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1020,  -532,     0,  -532,     0,
       0,     0,     0,     0,  -532,  -532,  -532,  -532,  -532,     0,
       0,  -532,  -532,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -532,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -532,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -532,     0,  -532,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -532,     0,     0,  -532,     0,     0,     0,     0,     0,
       0,  -532,     0,     0,     0,  -532,     0,     0,     0,  1950,
       0,     0,     0,     0,     0,     0,     0,  1951,  -532,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -532,     0,
       0,     0,  1952,     0,     0,     0,     0,     0,     0,     0,
    -532,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1953,     0,     0,  -532,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -532,     0,     0,
    1954,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1955,  1956,  1957,     0,     0,  1958,     0,     0,
       0,     0,     0,  1959,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -532,     0,  1960,     0,  1961,  1962,  -532,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -532,     0,
       0,     0,     0,     0,  1963,     0,     0,     0,     0,     0,
       0,     0,     0,  -532,     0,     0,     0,     0,  -532,     0,
       0,     0,     0,     0,  -532,     0,     0,     0,  -532,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -532,     0,     0,     0,     0,     0,     0,  -532,     0,     0,
       0,     0,     0,     0,     0,     0,  1964,     0,     0,     0,
       0,     0,  -532,     0,     0,     0,     0,  -532,     0,  1965,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -532,
    1966,  1967,     0,  1968,  1969,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -532,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1970,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1971,     0,  -532,     0,  -532,  -532,  -532,
       0,     0,     0,     0,     0,     0,     0,  -532,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1972,  1973,  1974,
       0,     0,     0,     0,     0,     0,     0,     0,  -532,     0,
       0,     0,     0,     0,  -532,     0,  -532,  -532,  -532,  1975,
       0,     0,     0,     0,  1976,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -532,     0,     0,  1977,
    1978,     0,     0,     0,     0,  1979,  1980,     0,     0,     0,
       0,  -532,     0,     0,     0,  -532,     0,     0,     0,     0,
       0,     0,  -532,     0,     0,     0,     0,     0,     0,     0,
       0,  -532,  -532,  -532,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -532,     0,  -532,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1981,     0,
    1982,  -532,     0,     0,  -532,  -532,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1983,     0,     0,     0,     0,
    -532,  -532,  -532,     0,   165,     0,     0,  1984,  1985,  1986,
       0,     0,     0,     0,     0,     0,  -532,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -532,     0,     0,  1987,     0,     0,     0,     0,  -532,     0,
       0,     0,     0,     0,     0,     0,  4119,     0,     0,     0,
       0,     0,     0,   165,     0,     0,  1988,  1989,  1990,     0,
    1991,     0,  1992,  1993,     0,  1994,  1995,  1996,     0,     0,
       0,  1997,     0,  1998,  1999,  2000,  2001,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2002,
       0,     0,     0,     0,     0,     0,     0,  2003,  2154,  2155,
    2156,  2157,  2158,     0,  2159,  2160,     0,     0,     0,     0,
    2004,  2005,     0,     0,     0,  2161,     0,     0,  2006,     0,
    1091,     0,     0, -3010,  2007,     0,     0,     0, -3010, -3010,
   -3010, -3010,     0,     0,  1092, -3010,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -3010,     0,     0,     0,     0,     0,
       0,     0,  4120,     0,     0,  1699,  4121,     0,     0,  2008,
       0,     0,  1700,     0,  2009,     0,     0,     0,     0,     0,
    2162,     0,  2163,  2164,  2165,  2166,  2167,  2168,  2169,  2170,
    2171,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -2859, -3010,     0,     0, -2859,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2010,
    2011,  2012,     0,     0,     0,     0,     0,     0,     0,  2013,
       0,     0,     0,     0,     0,     0,     0,     0, -3010,     0,
   -3010,     0,     0,     0,     0,     0,     0,     0,  2172,     0,
       0,     0,     0,  2014,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2015,  2016,  2017,  2018,
       0,     0, -3010,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2019,  2020,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2021,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2022,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2023,  2024,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2173,  2174,  2175,  2176,
    2177,     0,     0,  2178,  2179,     0,     0,     0,     0,     0,
       0,     0,     0,  2025,     0,     0,     0,     0,     0,     0,
       0,     0,  2026,     0,     0,     0,     0,     0,     0,  2027,
       0,     0,     0,     0,     0,     0,     0,  4122,     0,  2180,
    2028,     0,     0,     0,  2029,  2030,  2031, -3010,     0,     0,
       0,     0,     0,     0,     0,     0,  1093, -3010,     0,     0,
       0,  2032,  2033,  2181,     0,     0,  2034,  2035,     0,  2036,
       0,     0,     0,     0,     0,     0,  2037,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -3010,     0,     0,     0,
    2182,     0,     0,     0,  2152,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  4123,     0,     0,  2153,
       0,     0,  2154,  2155,  2156,  2157,  2158,     0,  2159,  2160,
   -3010,     0,     0,     0,     0,     0,     0,     0,     0,  2161,
       0,     0,     0, -3010,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -2859,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2183,     0,     0,     0,     0,
       0,     0, -3010,     0,     0, -3010,     0,     0,  4124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1094,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2162,     0,  2163,  2164,  2165,  2166,
    2167,  2168,  2169,  2170,  2171,     0,     0,     0, -3010,     0,
    4125,     0,     0,     0,     0, -3010,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2185,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2186,     0,     0,     0,     0,  2187,
   -3010,     0,     0,     0,     0,  4126,     0,     0,     0,     0,
       0,     0,  2172,     0,     0,     0, -3010,     0,     0,     0,
       0,  2188,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -2859,
   -2859,  2152,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -3010,     0,     0, -3010,  2153,     0,     0,  2154,
    2155,  2156,  2157,  2158,     0,  2159,  2160,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2161,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -3010,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -3010,     0, -3010, -3010,     0,
    2173,  2174,  2175,  2176,  2177,     0,     0,  2178,  2179,     0,
       0,     0,     0,     0,     0,     0,  2190, -3010,  2191,  2192,
    2193,     0, -3010,     0,     0,     0,     0,     0,  4127,     0,
   -3010,  2162,     0,  2163,  2164,  2165,  2166,  2167,  2168,  2169,
    2170,  2171,     0,  2180,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  4128,
   -3010,     0,     0,     0,     0,     0,     0,  2181,     0,     0,
       0,     0, -3010,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2182,     0,     0,  4129,     0,  2172,
       0,     0,     0,     0,     0, -3010, -2982,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  4130,     0, -2861, -3010,     0,     0,     0,
       0,     0,  2195,  2196,  2197,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2198,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2146,     0,     0, -3010,  4131,     0,     0,  2183,
       0,     0,     0,     0,     0,     0,     0, -3010,     0,     0,
       0,     0,     0,     0, -3010,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2173,  2174,  2175,
    2176,  2177,     0,     0,  2178,  2179,     0,     0,     0,     0,
       0,     0,   437,     0,  2184,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2185,     0,     0,     0,     0,     0,     0,
    2180,     0,     0,     0,     0,     0,     0,     0,  2186,     0,
       0,     0,     0,  2187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2181,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2188,     0,     0,     0,     0,
       0,   106,  3812,     0,     0,     0,     0,  3813,  3814,  3815,
    3816,  2182,     0,     0,  3817,  2154,  2155,  2156,  2157,  2158,
       0,  2159,  2160, -2982,  2189,     0,     0,     0,     0,     0,
       0,     0,  3818,  3819,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  3820,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  3821,     0,     0,     0,     0,     0,     0,  1638,
    3822,     0,     0,  1639,     0,     0,  2183,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2162,     0,  2163,
    2164,  2165,  2166,  2167,  2168,  2169,  2170,  2171,     0,     0,
    2190,     0,  2191,  2192,  2193,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  3823,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2184,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2194,     0,     0,
    2185,     0,     0,     0,     0,  2172,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2186,     0,     0,     0,     0,
    2187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -591,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2188,     0,     0,     0,     0,     0,     0,     0,
   -2982,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2195,  2196,  2197,     0,
       0,  2189,     0,  3824,     0,     0,     0,     0,     0,     0,
       0,     0,  2198,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2146,     0,     0,     0,
       0,     0,     0,     0,  2199,     0,     0,     0,     0,     0,
       0,     0,     0,  2173,  2174,  2175,  2176,  2177,     0,     0,
    2178,  2179,     0,     0,     0,     0,  3825,     0,     0,     0,
       0,     0,     0,     0,  3826,     0,  3827,     0,     0,     0,
       0,     0,     0, -2907,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  3828,     0,     0,  2180,  2190,     0,  2191,
    2192,  2193,     0,     0,     0,  3829,   730,     0,     0,     0,
       0,     0,   731,   732,     0,     0,     0,     0,     0,     0,
    2181,     0,     0,     0,     0,     0,     0,   734,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,     0,  2194,     0,     0,  2182,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  3830,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  3831,     0,     0,     0,  3832,  -592,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  3833,     0,     0, -2982,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2195,  2196,  2197,     0,     0,     0,     0,
       0,     0,  2183,     0,     0,     0,     0,     0,     0,  2198,
       0,     0,     0,     0,     0,     0,     0,  3834,     0,     0,
       0,     0,     0,  2146,  1665,     0,     0,     0,     0,     0,
       0,  2199,  3812,     0,     0,     0,     0,  3813,  3814,  3815,
    3816,     0,     0,     0,  3817,  2154,  2155,  2156,  2157,  2158,
       0,  2159,  2160,     0,     0,     0,     0,  3835,     0,  1666,
       0,     0,  3818,  3819,     0,     0,   735,   736,   737,     0,
       0,     0,     0,     0,     0,  3836,  2185,     0,   738,     0,
       0,  3820,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2186,     0,     0,     0,     0,  2187,     0,  1641,  1642,
       0,     0,  3821,     0,     0,   465,     0,     0,     0,  1638,
    3822,     0,     0,  1639,     0,     0,     0,     0,  2188,     0,
       0,  3837,     0,     0,     0,     0,     0,  2162,     0,  2163,
    2164,  2165,  2166,  2167,  2168,  2169,  2170,  2171,  1647,     0,
       0,     0,     0,  1648,  1649,  1650,  1651,     0,     0,     0,
    1652,     0,     0,     0,     0,     0,     0,  3823,     0,     0,
       0,     0,   739,   740,  3838,     0,     0,     0,     0,  1653,
     741,     0,   742,     0,  1671,     0,  1672,  1673,     0,   466,
       0,     0,     0,   743,   744,   745,     0,     0,     0,     0,
       0,   746,     0,     0,   467,  2172,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  3839,
       0,     0,     0,     0,     0,     0,  1654,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2190,     0,  2191,  2192,  2193,     0,  3840,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  3841,     0,  1655,     0,  1656,     0,     0,     0,     0,
       0,     0,     0,  3824,     0,     0,     0,     0,     0,     0,
       0,     0,   748,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  3842,     0,     0,  1657,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   749,     0,     0,
       0,     0,     0,  2173,  2174,  2175,  2176,  2177,     0,  3843,
    2178,  2179,     0,     0,  -894,     0,  3825,     0,     0,     0,
    3844,     0,     0,     0,  3826,     0,  3827,     0,     0,     0,
       0,     0,   730, -2907,     0,     0,     0,     0,   731,   732,
       0,     0,     0,  3828,  3845,     0,  2180,   468,     0,  2195,
    2196,  2197,     0,   734,     0,  3829,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2198,     0,     0,     0,     0,
    2181,     0,     0,  3846,     0,     0,     0,     0,     0,  2146,
       0,     0,     0,     0,     0,     0,     0,   106,     0,     0,
       0,     0,     0,   750,   730,     0,     0,  2182,     0,     0,
     731,   732,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  3830,     0,     0,   734,     0,     0,     0,     0,
       0,     0,  1658,  3831,     0,     0,     0,  3832,     0,     0,
       0,     0,  1659,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  3833,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1660,     0,     0,     0,   469,     0,     0,     0,     0,
       0,     0,  2183,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  3834,     0,     0,
     751,   752,     0,     0,  1665,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   735,   736,   737,     0,     0,     0,  1661,     0,
       0,     0,     0,     0,   738,     0,     0,  3835,     0,  1666,
       0,     0,   730,     0,     0,     0,     0,     0,   731,   732,
       0,     0,   470,     0,   771,  3836,  2185,  1662,     0,     0,
    1663,   465,     0,   734,     0,     0,   754,     0,     0,     0,
       0,  2186,     0,     0,     0,     0,  2187,     0,  1641,  1642,
       0,     0,     0,     0,   735,   736,   737,     0,     0,     0,
       0,     0,   755,     0,     0,     0,   738,     0,  2188,     0,
     756,  3837,     0,  1664,     0,     0,     0,     0,     0,     0,
    1665,     0,     0,     0,     0,     0,     0,     0,   739,   740,
       0,     0,     0,   465,   757,     0,   741,     0,   742,     0,
     154,     0,     0,     0,     0,   466,     0,     0,     0,   743,
     744,   745,   471,     0,  3838,  1666,     0,   746,     0,     0,
     467,     0,     0,     0,  1671,     0,  1672,  1673,     0,     0,
       0,  1667,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     739,   740,     0,     0,     0,     0,     0,     0,   741,  3839,
     742,     0,     0,     0,     0,     0,     0,   466,     0,     0,
       0,   743,   744,   745,     0,     0,     0,  1668,     0,   746,
    1669,     0,   467,  2190,     0,  2191,  2192,  2193,     0,  3840,
       0,     0,   735,   736,   737,     0,     0,     0,   748,     0,
       0,  3841,     0,     0,   738,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1670,     0,     0,   749,     0,     0,     0,     0,     0,     0,
    1671,   465,  1672,  1673,  3842,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1674,     0,     0,     0,     0,  1675,     0,  3843,
     748,     0,     0,     0,  -897,  1676,     0,     0,     0,     0,
    3844,     0,     0,   468,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   749,     0,     0,   739,   740,
       0,     0,     0,     0,  3845, -2969,   741,     0,   742,  2195,
    2196,  2197,     0,     0,     0,   466,   730,  1677,     0,   743,
     744,   745,   731,   732,     0,  2198,     0,   746,     0,   750,
     467,     0,     0,  3846,     0,     0,     0,   734,     0,  2146,
       0,     0,     0,     0,     0,   468,     0,     0,     0,     0,
    1678,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1679,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   750,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   469,     0,     0,     0,     0,     0,     0,   748,     0,
    1680,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1681,     0,     0,     0,   751,   752,     0,  1682,
       0,     0,     0,   749,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   730,     0,     0,     0,     0,     0,
     731,   732,     0,   469,     0,  1733,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   734,   770,     0,   470,     0,
     771,   772,     0,   468,     0,     0,     0,     0,   751,   752,
       0,     0,   754,     0,     0,     0,   735,   736,   737,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   738,     0,
       0,     0,     0,     0,     0,     0,   730,     0,   755,     0,
       0,     0,   731,   732,     0,     0,   756,  2590,     0,   750,
       0,     0,     0,     0,     0,   465,     0,   734,   770,     0,
     470,     0,   771,   772,     0,     0,     0,     0,     0,     0,
     757,     0,     0,     0,   754,     0,   154,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   471,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     755,     0,     0,     0,     0,     0,     0,     0,   756,     0,
       0,     0,   739,   740,     0,     0,     0,     0,     0,   730,
     741,   469,   742,     0,     0,   731,   732,     0,     0,   466,
       0,     0,   757,   743,   744,   745,     0,     0,   154,     0,
     734,   746,     0,     0,   467,     0,   751,   752,     0,     0,
     471,     0,     0,     0,   735,   736,   737,  2758,     0,     0,
       0,     0,     0,     0,     0,     0,   738,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   730,     0,     0,     0,     0,     0,   731,   732,     0,
       0,     0,     0,   465,     0,     0,   770,     0,   470,     0,
     771,   772,   734,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   754,     0,     0,     0,   735,   736,   737,     0,
       0,     0,   748,     0,     0,     0,     0,     0,   738,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   755,     0,
       0,     0,     0,     0,     0,     0,   756,   749,     0,     0,
     739,   740,     0,     0,     0,   465,     0,     0,   741,     0,
     742,     0,     0,     0,     0,     0,     0,   466,     0,     0,
     757,   743,   744,   745,     0,     0,   154,     0,     0,   746,
       0,     0,   467,     0,     0,     0,     0,     0,   471,     0,
       0,     0,     0,     0,     0,     0,     0,   468,     0,   735,
     736,   737,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   738,   739,   740,     0,     0,     0,     0,     0,     0,
     741,     0,   742,     0,     0,     0,     0,     0,     0,   466,
       0,     0,     0,   743,   744,   745,     0,     0,   465,     0,
       0,   746,     0,   750,   467,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     748,   735,   736,   737,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   738,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   749,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   739,   740,     0,     0,     0,
     465,     0,     0,   741,     0,   742,     0,     0,     0,     0,
       0,     0,   466,     0,     0,   469,   743,   744,   745,     0,
       0,     0,   748,     0,   746,     0,     0,   467,     0,     0,
       0,     0,     0,     0,     0,   468,     0,     0,     0,     0,
     751,   752,     0,     0,     0,     0,     0,   749,     0,     0,
       0,  2760,     0,     0,     0,     0,     0,   739,   740,     0,
       0,     0,     0,     0,     0,   741,     0,   742,     0,     0,
       0,     0,     0,     0,   466,     0,     0,     0,   743,   744,
     745,   750,     0,     0,     0,     0,   746,     0,     0,   467,
     770,     0,   470,     0,   771,   772,     0,   468,     0,     0,
       0,     0,     0,     0,     0,   748,   754,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     749,     0,   755,     0,     0,     0,     0,     0,     0,     0,
     756,     0,     0,   750,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   469,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   757,     0,     0,   748,     0,     0,
     154,     0,     0,     0,     0,     0,     0,     0,   751,   752,
     468,     0,   471,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   749,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   469,     0,  2781,     0,     0,
       0,     0,     0,     0,     0,     0,   750,     0,   770,     0,
     470,     0,   771,   772,     0,     0,     0,     0,     0,     0,
     751,   752,   468,     0,   754,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     755,     0,     0,     0,     0,     0,     0,     0,   756,  4240,
       0,     0,     0,     0,     0,     0,     0,     0,   750,     0,
     770,     0,   470,     0,   771,   772,     0,     0,   469,     0,
       0,     0,   757,     0,     0,     0,   754,     0,   154,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     471,     0,     0,   751,   752,     0,     0,     0,     0,     0,
       0,     0,   755,     0,     0,     0,     0,     0,     0,     0,
     756,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     469,     0,     0,     0,   757,     0,     0,     0,     0,     0,
     154,     0,     0,     0,     0,   470,     0,     0,     0,     0,
       0,     0,   471,     0,     0,   751,   752,     0,     0,   754,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   755,     0,     0,     0,     0,
       0,     0,     0,   756,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   470,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   757,     0,     0,
       0,   754,     0,   154,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   471,     0,     0,     0,     0,
       0,  4241,     0,     0,     0,     0,     0,   755,     0,     0,
       0,     0,     0,     0,     0,   756,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   757,
       0,     0,     0,     0,     0,   154,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   471
};

static const yytype_int16 yycheck[] =
{
     120,   464,   588,  1107,   202,    50,    51,    52,   655,   656,
     657,   643,    57,   660,  1102,   923,  1566,  1429,  1434,   596,
      47,  1038,   107,   142,   109,   973,    53,   592,  1640,   114,
     725,  2349,   584,  1645,  2146,   179,   216,  1551,   949,  1607,
    2064,   964,   579,   594,   608,   107,  2364,   109,   580,   245,
    1129,   602,   114,   817,  2304,  2495,  2227,   813,   103,  1561,
     105,   625,   588,   243,  1125,   110,   111,   152,   739,   753,
    2252,  2253,  3018,   624,  1579,   120,  1137,  2076,  2107,   579,
     602,  1225,   813,    15,  1459,   130,  1811,  3250,    15,   148,
     152,    15,  2679,     1,  1812,     1,     1,   624,    24,    15,
    2061,     0,   624,    76,  1822,   150,    15,  1832,    37,     1,
    1784,    30,    15,     0,   640,    15,  2228,  2229,   105,    15,
     763,    82,    13,   168,     1,  2251,    61,   741,   742,    41,
      24,    15,    12,    15,   805,   749,   181,   780,  2542,    82,
     154,  1220,    55,   158,  2869,   107,   189,    30,    17,   106,
      40,     1,   202,   110,   153,   182,   134,   219,   198,   107,
      15,    82,   220,    54,  1689,    41,   107,    42,   233,    42,
     116,   136,   242,  1847,   938,  2437,   790,   908,   219,    29,
     225,   284,   119,   263,  1107,   219,  1698,   192,   306,   753,
     134,   106,   225,   306,  1706,   110,   242,   230,   233,   306,
     117,   326,   299,   326,   119,  2475,   284,   233,   184,   184,
      79,   154,   442,   969,    99,   260,   296,   297,  2744,   225,
     976,   273,   267,    94,   230,   107,   445,   254,   255,   273,
    2965,    10,    43,   770,   771,   772,  1489,   700,   969,    10,
     117,   158,   159,    82,   270,   278,   272,    41,  3554,   379,
      15,   410,   894,    22,   786,  2828,   134,  1207,   890,   442,
     166,  3540,   226,   420,   248,  2838,  3147,    37,  2106,  3399,
     199,   771,   278,   398,   248,   343,   373,  2595,  2596,   192,
     472,   158,   159,   134,   472,    29,    22,    43,   553,    40,
      56,   220,   542,  1200,  2309,  2310,  3059,   398,   982,   542,
    2315,  2316,  2317,  2485,   326,   398,    10,   406,   577,   527,
      82,   224,   340,  2986,   840,   284,   100,   410,    83,  3529,
     387,    55,  1229,   429,   444,   876,    22,   292,   161,   342,
     275,   425,   398,   469,   434,   342,    41,   888,   340,   165,
      74,   166,   167,   630,   243,   373,   627,   375,    37,    37,
     249,   284,   325,   985,   106,  3084,   243,   232,   110,   232,
     154,   134,   249,  3399,   107,  2326,  3549,  1446,  4018,   120,
    3750,   379,   397,   124,   306,   720,   715,   326,   423,   424,
    3765,  2055,   798,   308,   221,   583,   413,   585,   442,   715,
    4084,   442,   790,   335,   942,  1090,  2984,  3643,   443,   398,
     514,  1096,   442,    13,  2419,   450,   835,  2422,   959,   605,
      10,  3621,   798,  3543,  2429,   966,     0,  2432,   982,   617,
     283,  1177,   620,   809,   267,   292,   308,   425,   626,   199,
    1578,   197,   546,   213,   228,   864,   192,   959,   975,    10,
     498,   340,   487,  3031,    54,   341,  3692,   339,   646,   154,
    2124,   478,   368,   340,  1602,   292,   854,   326,  3641,  1007,
     876,  1540,   514,   107,   442,   368,  1109,   344,    22,   134,
     720,   134,   524,  4167,   373,   975,   375,   344,   134,   398,
    1892,   398,   442,   343,   829,   410,   373,  3867,   375,   409,
     829,   416,  3877,  2743,   273,   134,    22,   377,   378,  4149,
     442,   526,   437,   829,   579,    10,  2688,  3543,   465,   584,
     199,   199,  1207,   475,  2543,   398,   596,   408,   580,   594,
     801,   398,   584,   228,  2548,  1168,   442,   602,   442,   409,
     450,   539,   594,   595,   483,   533,   801,   422,   420,   442,
     602,   457,   343,   762,   598,   725,   608,   598,   740,   624,
     465,  3761,   565,   745,   457,   691,   525,   619,   565,   621,
     622,    22,   624,   625,  3237,   685,    15,  3093,   513,   597,
     450,   498,   398,   860,  1111,  1112,  1113,  1129,   498,   829,
     680,   450,  1119,  1120,  1121,  3314,   654,   649,   806,   368,
     326,   460,   525,  2863,   693,   597,  2866,  2612,  1135,  1616,
     706,  1138,   790,   368,   802,   797,  2120,  1144,  1145,  1146,
    1147,  1148,   382,   882,   543,   637,   630,   815,   498,  1119,
    1120,  1121,   394,   394,   804,   442,   720,   442,   478,   498,
     326,   442,   366,   852,  1545,  1135,   442,  1174,   422,   596,
     883,   742,   559,   498,   410,  3526,   290,   472,   219,   442,
     780,  3414,  3415,   883,   433,    47,   435,   814,   107,  1224,
     425,   579,   627,   442,   368,   895,   584,   267,  1220,   630,
    1196,   716,   795,   200,  1174,  1736,   594,   442,   457,  1758,
    3267,   596,   559,   434,   602,   221,   442,   630,   763,   423,
     617,   753,   457,   654,   610,   770,   771,   772,   597,    40,
     883,  2375,   737,   574,   596,   780,   624,   610,   888,   630,
     597,  1475,  3152,   465,   483,  1859,  2284,  1249,   512,  2680,
    1864,   455,   473,   882,   786,   795,  4032,   923,   806,   433,
     795,   435,  4011,   808,   478,   692,   294,  2575,   442,   533,
    2245,   385,   706,   923,   796,   516,   808,   483,   275,   795,
    3323,   309,   796,   457,   698,   719,   106,  3492,   883,  1515,
     110,   616,    38,   588,   790,   883,   703,    10,   368,  1411,
     883,   542,   652,  3177,   779,  1418,   883,   692,   663,   120,
     883,   560,  1453,   124,  1455,   790,   883,   854,   703,   658,
     883,   630,  3318,   973,  1705,   836,  3926,   368,   733,   745,
     305,   876,   836,   883,  2329,  2058,   876,   640,  2823,   785,
     785,   795,   728,   888,   876,   654,   416,   883,   883,   728,
     698,   795,   556,  2335,   882,   728,   888,   677,   579,   580,
     720,   610,   882,   433,   795,   435,   801,   810,   795,   882,
     883,  3103,   442,   883,   596,   610,   860,   698,   792,   790,
     883,   797,   770,   771,   772,   803,   560,   457,  1808,   308,
     808,   923,   433,   368,   435,   864,   881,   854,   883,   442,
    3140,   442,   472,   806,   949,  3610,   882,   883,   795,   663,
    2133,   943,  1805,   626,   959,  3648,   457,   343,   782,   805,
     808,   966,  1443,  1430,  1446,   807,   795,   959,   442,   860,
     975,  3027,   805,   795,   966,   883,   610,  3632,   795,   882,
    1090,   838,  3352,   796,   792,  2876,  1096,   860,   795,   368,
     982,  1443,   884,   657,   837,   698,   883,  2441,   433,   483,
     435,   807,   814,   677,  3460,   895,   769,   442,   704,   860,
     692,   792,   768,  1028,   192,   795,   222,   807,   883,   728,
     876,   883,   457,   882,   874,  1506,   795,   483,   876,   883,
     560,   888,    15,   728,   883,   882,  1028,   883,   883,   883,
     888,   420,  3376,  1032,    27,   883,  3563,   883,   883,   895,
    1025,  1179,  1162,   883,   883,   876,   895,   883,  1540,   560,
    1897,  1528,   895,   442,   874,  2956,   883,  2958,  1612,   883,
    1756,  4164,   882,   576,  3950,   882,   807,   882,   457,  1068,
     610,  1191,  2387,   882,   883,   878,   891,   796,   891,   792,
     883,   860,   483,  1068,   728,  1756,   805,  1207,  1528,  1091,
      83,   368,  3978,   698,   805,   698,  1111,  1112,  1113,   610,
     805,   959,   698,   795,  1119,  1120,  1121,  1122,   966,  1094,
    1125,  1722,  1723,  3014,  1129,   560,   233,   975,   883,   698,
    1135,  3587,  1137,  1138,    10,  1749,   883,  1129,   883,  1144,
    1145,  1146,  1147,  1148,   291,   226,   460,   883,   614,   425,
     895,  1752,  1725,  2757,  2802,  2803,  2804,  2805,   839,   895,
     883,   825,   368,   434,  2464,   343,   488,   489,   490,  1174,
     226,   805,   895,   434,   883,   610,   379,   442,  1167,   442,
     377,     0,  2837,  1808,  2616,   465,   895,   498,   676,  1646,
     457,   498,   442,   442,   372,   368,   398,   792,   728,   792,
     895,   882,   295,   442,   442,  1210,   792,  3308,   803,   514,
     442,   573,   409,   808,  2658,  1220,   194,  2613,  3177,   442,
     794,   542,   398,   792,  2062,   442,    46,   728,  1220,   803,
     804,   610,   420,   442,   808,  2839,   442,  2841,   704,   445,
     387,  2078,   409,   450,  2085,    30,   233,   233,  3177,   883,
     780,   457,   181,   450,    36,  1749,   387,  1249,   405,   497,
     433,   895,   435,  1111,  1112,  1113,   120,     0,  2703,   442,
     124,  1119,  1120,  1121,   405,   805,  1758,   422,  2535,   601,
     486,  1129,  2886,   450,   457,   742,   339,  1135,   542,   790,
    1138,   498,   571,   728,   666,  1598,  1144,  1145,  1146,  1147,
    1148,   498,  1352,   760,   805,   808,   528,   290,   579,   580,
      79,  1773,   494,   715,   689,   343,   596,   799,   107,  1429,
     777,   221,  4198,   434,    18,   450,  1174,   488,   489,   103,
     104,   498,   310,    19,   822,   836,    22,    23,   576,  3382,
     492,   325,  2289,   368,  2948,   682,  2950,   658,   429,   728,
     577,  3452,  3735,   883,  3455,  1481,     1,   228,  1931,   598,
     387,   621,   469,   152,   325,   895,  1873,    75,  2442,   857,
     805,  2362,  1220,   429,   862,   359,   188,   274,   405,  3732,
     288,   379,   883,   291,   158,   368,  1496,   560,   442,   284,
     202,  1898,   895,   220,   895,   326,   419,   548,   359,  2843,
    1907,   883,   584,  3932,   610,   791,   553,   799,   559,   883,
     118,  1521,   692,  1418,   188,   109,   326,   442,   472,   680,
     795,   895,   553,   715,   243,  1430,   805,   829,   202,  2438,
     249,  2440,   457,   715,  1508,   814,   414,   610,  1443,  3735,
     601,  1446,   425,  3796,   720,  3557,   598,   882,   883,   720,
     540,  1443,  3735,   543,  1446,   129,   726,   419,  3735,   442,
     895,   728,   486,   723,   724,   621,   325,  3115,   288,  2306,
     626,   368,   166,   181,   457,   169,   654,   171,   732,   387,
    3581,  3582,   284,   469,  2493,  4014,  2059,  2060,   466,   811,
     812,   883,   368,  1949,  3735,   276,   190,   405,   820,   721,
     359,  1506,  3872,   284,   883,   559,    89,    90,    91,  2123,
     882,   494,   528,   792,  1506,   795,   895,  2090,   663,  3562,
     882,   340,   728,  1528,   666,   553,   796,   733,   734,   792,
    2408,  2368,  2369,   165,   521,  1540,  2968,   829,  2970,   123,
    1545,   666,   442,   596,  1519,   442,   130,   829,  1540,   886,
    3504,   549,  2246,   335,   373,   728,   375,   433,  2900,   435,
     457,  2957,   883,  2400,  2401,   548,   442,  2867,   882,   680,
     808,   882,  2053,  2054,   503,   882,  1568,  3075,   449,  2180,
     434,   457,  1430,   271,  1599,   610,   352,   353,   282,  2598,
     895,   684,  1567,   796,   796,  1443,  1571,  1572,  1446,  1574,
     478,  2053,  1577,  1578,  2066,  1580,   882,  1599,  2445,   213,
     788,   233,   543,   398,  2451,   876,  2189,   790,   883,   795,
     883,   204,   205,   206,  2465,   706,   814,   610,   895,  2123,
     525,  2088,   805,   883,   883,   742,  1939,  1594,   719,   459,
     373,   772,   375,   821,   883,   553,  4029,  2904,  2115,  2116,
     706,   883,   528,   760,  1646,   882,   883,   895,  1506,   820,
     883,   427,   428,   719,    18,   772,   883,   577,  1778,   579,
     777,   498,   582,  1648,   883,   882,   586,   874,   588,  1654,
    1528,   450,  3330,  1658,   560,  2115,  2116,  2373,  2525,   895,
     398,   460,  1540,   450,   604,   605,   341,   720,  1808,   609,
    1705,  2538,  2539,   728,   412,   721,   616,   874,   732,   397,
     883,   735,   736,   610,    40,   882,  3320,  2342,   547,   368,
    1770,   631,   895,   525,   432,   579,   580,  4110,   223,  3333,
    3334,  1736,   883,  4029,   610,   645,   883,   647,   648,   151,
     882,   498,    13,   621,   722,   728,  4029,   379,   626,  1754,
     395,   445,  4029,  1758,   737,   109,   742,  1749,   720,  2612,
    3802,  3803,  1754,  3943,  2605,   788,  1758,  2714,  2715,  3870,
    3871,   470,  3952,   592,   760,   397,   883,  3381,   597,   787,
     805,  1773,   456,    54,   458,   559,   772,   803,  4029,   795,
     285,   777,   808,   442,  2367,   569,   570,   483,   484,   807,
    1789,   803,   291,   793,   387,   388,   808,   382,   457,   792,
    1785,   794,   166,   803,  4110,   169,   450,   171,   808,   688,
     803,   804,   805,   733,   807,   808,   788,  4110,   411,   883,
    1835,   728,   339,  4110,   522,   813,   190,   469,   526,   533,
     398,   895,  1847,   537,  1819,   721,  2323,   533,   534,   539,
     434,   434,   728,  1828,   438,  1847,  2322,  2323,   883,   434,
     770,   771,   772,   189,   498,  2322,  2323,   641,  2325,  4110,
     895,   368,   629,   450,   609,   458,  4269,   576,  4271,  4221,
    4273,  3923,  3924,  2350,   442,  2352,   796,   661,   662,  2726,
    2406,  2464,   276,   615,  1944,   165,   618,   223,   387,   521,
     284,   623,  2383,   593,   526,   379,  1754,   539,   805,  2376,
    1758,   821,   895,   621,   344,  4257,   405,   625,   787,   442,
    2350,   498,  2352,   373,   398,   375,  1931,  1932,   282,   805,
    2808,    13,  2399,   641,   803,   643,  2062,    13,   798,   808,
     881,   631,   883,  4170,   442,   442,  2376,  2057,  2443,   809,
     644,  4199,  2062,  2430,  2431,   123,  2438,  1416,  2440,   285,
     457,   610,   130,   233,  2430,  2431,   368,  2077,   398,  2399,
     553,     1,    54,  2430,  2431,   498,   181,   379,    54,   794,
    1439,   795,   540,   893,   787,   543,   883,   681,   791,   804,
    3935,   726,   689,   903,   904,  4222,  4223,  3942,   895,  1847,
     326,  3946,   585,   368,  3949,   588,   589,   883,  2118,   621,
    2491,  2493,   388,   923,   626,   750,   751,   752,   344,   895,
    2130,   792,  2096,  2097,  2098,  2099,  2100,  2101,  2102,  2103,
     795,   614,   803,   727,   800,   729,   802,   808,  2088,   949,
     442,    19,    20,    21,   581,    75,   621,   434,  2053,  2054,
    2055,   438,  2547,   803,   591,   457,   563,   795,   808,   634,
     567,  2053,  2054,  2055,   553,   975,   760,   792,  3410,  3411,
     827,   981,  2909,  2910,  2066,   275,  2126,   442,   803,   728,
    2085,   795,   104,   808,   284,  2922,  2923,   795,   118,   596,
     621,   445,   457,   213,  1004,   626,  2088,   165,  1008,  1009,
     795,     1,  2939,  2940,   803,   827,    31,    32,  3552,   808,
    2115,  2116,   610,   610,   799,   326,  2598,   328,  2585,  2124,
    1111,  1112,  1113,  4068,   792,   882,  4071,  2752,     7,   689,
      15,  2123,  2124,   111,  2184,   803,   158,   430,   431,   523,
     808,   525,    27,   531,   532,  2255,   308,  1138,  2986,    35,
      36,   181,   860,  2281,  2282,  2585,   805,    57,    58,    59,
      60,  3286,    62,   325,   298,   233,   188,   714,   292,   293,
    3295,  3296,  3297,    52,   434,  3823,  2226,   675,   438,   533,
     202,   742,   689,  2233,  3832,    64,  1623,   714,   771,   883,
    1627,  4233,  3840,  1103,  1104,  2059,  2060,   359,    83,   469,
     398,  1111,  1112,  1113,  2254,  2053,  2054,  2055,   610,  1119,
    1120,  1121,  1122,   795,  2815,  1125,  2344,  2345,  4172,   795,
     344,  3099,  2738,  2198,   795,  1135,  2201,  1137,  1138,   108,
     728,   728,   795,   442,  1144,  1145,  1146,  1147,  1148,   379,
    2232,   141,   142,  1144,  1145,   610,   895,    35,   793,  1159,
     374,  1161,   131,  2379,   416,  3735,  4165,  4166,   803,   523,
    2235,   525,   292,   808,  1174,  2330,  2331,  2115,  2116,   770,
     596,   772,  2322,  2323,   436,  2250,  2124,   249,  3620,  3821,
     589,   243,   780,  2240,  1394,   494,   795,  2744,   497,  3831,
     599,  4200,  1120,  1121,  2867,   273,  3133,  1146,  1147,  1148,
    1210,   610,   611,  1413,  1414,  1415,   185,   689,   805,  4218,
     795,   795,  2287,  2288,   344,   883,  2291,  2309,  2310,   840,
     841,   842,   843,  2315,  2316,  2317,   728,   681,   132,  1728,
    2322,  2323,  1731,  2325,   840,   841,   842,   843,  2464,   609,
    1739,   883,   883,  2358,  1743,  2350,  2321,  2352,   883,    36,
     559,  1750,  2327,   192,  2861,   213,  3698,  2362,   795,   442,
    2409,   442,   814,   728,   883,  4274,  2358,   576,   398,  2479,
    2375,  2376,   442,   795,   352,   353,  3213,  3214,  2383,   667,
    2430,  2431,   412,  2375,   264,   883,   265,   368,   795,   885,
     701,  2383,  3230,   720,  2399,   110,   119,   895,   895,  3237,
     883,   469,   432,   805,   442,   290,   760,   840,   841,   842,
     843,   398,  3754,   473,   314,   315,   316,   317,   318,   319,
     320,   321,   322,   393,   795,  2535,   795,  2419,   700,   590,
    2422,   689,  4057,  2438,   137,  2440,   795,  2429,  2430,  2431,
    2432,   795,   546,   135,   782,   573,  2438,   442,  2440,   427,
     428,   760,   703,   332,   395,   795,   726,   189,   795,    15,
    2465,   442,   791,    19,   445,   795,    22,    23,  2473,   668,
     795,   790,   742,   883,   288,  2601,   457,   883,  4103,  4104,
     750,   751,   752,   368,  4109,   883,  2491,  4112,  2493,   807,
     760,  4116,  4117,   895,   795,  2621,  4147,   689,  4149,  2491,
    2610,  2493,  2350,   449,  2352,   486,   795,   777,   429,   543,
    3026,    31,   799,   491,   795,  1213,  1214,  1215,  1216,   883,
    1430,   473,   442,  4174,  4175,   596,  4177,  2375,  2376,   883,
     895,  4182,  4183,   412,   136,  2383,   654,   234,  2664,   807,
     425,   609,   442,   469,   853,   335,   524,  3148,   539,  4029,
     807,  2399,   807,   807,   807,   533,  3129,   442,   807,   868,
     807,   807,   441,   807,   542,  2156,  2157,  2158,  2159,   807,
     235,  1481,   457,   807,   807,   807,   290,   790,   236,  2707,
    2585,   237,   667,  1493,   239,  1495,   621,   159,   772,   772,
    2438,   621,  2440,  2598,   351,   625,    77,   240,   882,   808,
    2605,   241,   442,   912,   582,   242,  2598,   779,   179,   790,
     807,   641,  3958,   643,   246,    82,   247,   780,  1528,   335,
    2612,   883,  1532,   250,   252,  3462,  3463,  3464,   251,   610,
    4110,   883,  2624,  3234,     7,  1545,  3093,  3584,  3585,  2614,
     253,   864,  3124,  2491,  3126,  2493,   254,  2622,   284,   335,
    3487,  3488,   472,   591,    41,   255,   807,   536,   726,   256,
     394,   257,   335,   548,   973,   258,   196,  2717,   259,   795,
     689,   980,  2647,   440,   742,   984,   795,   577,   638,    52,
     795,   152,   750,   751,   752,   771,   795,   326,   883,   785,
     165,    64,   760,   790,  2744,  3532,  3533,   883,   883,   599,
     790,   883,   343,   385,   444,   594,   442,  2682,   473,   777,
    2685,   881,  2687,   652,   525,   284,   839,  3735,   854,  2694,
    2695,  2696,  2697,   881,   876,   610,   229,  2702,   607,   305,
     640,   245,   611,   394,   335,   108,   790,  2585,   709,   357,
     782,  2867,   787,  2718,   776,   394,   394,   728,  3695,   233,
    2598,  2351,  2744,  2353,   790,   882,    82,   790,   131,  2359,
    2360,  2361,   790,  2891,   196,   795,  2366,   335,   335,    82,
     394,   260,   335,   595,   795,   689,  2751,  2377,  2378,  1689,
     641,  2891,    33,    34,   153,   860,    30,   539,   795,    75,
     531,   106,    13,  3740,  2904,  1705,   410,   720,   134,   894,
     893,  3639,   742,   410,   683,   742,   531,   792,   480,   792,
     792,   343,   185,   792,   790,   883,    40,  1727,   796,   792,
    1129,   654,   800,   792,   802,   792,  1736,   792,    36,   790,
     860,  2823,   659,   410,  2839,   637,  2841,   343,   343,    24,
     782,   244,   823,   728,   895,   640,   790,  2839,    82,  2841,
     220,   343,    76,   883,    78,   379,   876,   540,  2984,   876,
    2986,  3318,   539,   343,   743,   742,   790,  3468,    15,    12,
     592,   689,   123,   795,   855,   856,   100,   680,   344,   130,
     860,  2886,   133,   782,   876,   663,   445,   398,   344,   883,
     864,   704,   265,   106,  2886,   742,   120,  1206,  4240,   450,
     124,    56,  3369,   703,   720,  3031,   883,   792,   546,   794,
    1219,  1220,   191,   442,   895,   721,   792,   792,   803,   804,
     805,   343,   807,   808,   528,  1835,  3499,   483,   484,  1839,
     233,   526,  1241,   184,   498,   186,   187,   397,   229,   818,
      82,   860,   860,   326,   326,  2950,   165,   703,    13,   795,
     335,   521,   233,   795,   548,   640,   577,   197,  2950,   332,
     769,    93,   689,  3540,   795,   895,    88,  3017,   824,  2944,
      36,   790,   701,  2965,   883,   790,  3538,   533,   534,   326,
     438,   539,  3032,   883,   704,    24,   883,   795,   580,  3039,
     335,  2839,   343,  2841,   792,   343,   875,   206,   181,    13,
     801,  1911,   636,  3460,  1914,    54,  1916,  1917,  1918,  2966,
     895,  4029,   480,   202,   636,   549,   267,   544,   269,  3034,
     551,   248,  1932,  3073,   450,   276,  1936,   181,  1938,   410,
     503,  3081,   450,   284,  1944,   434,   527,   806,  2886,   412,
     847,   795,  3034,  3093,   782,   443,  3061,  3062,   335,   720,
     787,   883,  3067,   442,  3069,   200,  3166,   381,  3050,  3051,
     788,    82,   192,   759,   592,   498,    82,  3059,   441,  3061,
    3062,   498,   450,   450,   450,  3067,  4110,  3069,  3053,   522,
    3681,  3070,   795,   271,   883,   790,   198,  3076,  3077,     1,
     198,    12,   107,  3068,     6,  3070,    22,   483,   592,   552,
     835,  3093,  2950,   615,  3230,   659,  3679,   481,   798,   385,
     325,  3237,   566,  3223,    26,   705,   790,   525,   525,  3124,
     284,  3126,   596,   525,   284,   189,   795,    39,   790,   382,
    3587,    40,  3124,   357,  3126,   460,   387,  1446,   191,   799,
     666,   642,  3715,   732,    56,   821,   198,   543,   795,   795,
     795,   482,  2062,   759,   483,  2065,  3266,   799,   220,   758,
    2070,    22,  3154,   536,   592,  2075,   200,   410,   410,   425,
     795,   539,   876,   538,  2084,  2085,   425,   410,   410,   165,
     410,   410,   344,   434,   425,   772,  3759,   438,   117,   224,
     106,   344,   548,   429,   359,   398,   383,   795,   422,  3249,
     335,  3176,  1511,   531,   493,  2115,  2116,   119,    82,   559,
     434,   746,  2122,  1522,   438,   559,   548,   559,    11,  2129,
    2130,   472,   135,  3351,  1533,    87,   192,  2137,   704,    25,
      63,  1540,   234,   164,   607,   240,    70,   257,   611,   679,
     170,   166,   434,  1048,  3650,  3450,  1939,  3867,  2693,  3425,
    3265,  3572,  2701,  2707,  3423,   418,  3051,  3785,  3729,  3796,
    2060,   658,  3277,  2201,  1567,  2654,  4238,  3647,  3318,  3556,
    1940,  3692,  3931,  3265,  2664,  4057,  3124,  4177,  3126,  4182,
    3255,  3543,  3957,  3393,  3688,  3277,   609,  3863,  3303,  1411,
     910,  3283,  3824,  3824,  3286,  4018,  3831,  2845,  3859,  1085,
    2292,   195,   846,  3295,  3296,  3297,  3298,  1090,  2752,  1096,
     683,  3303,  2743,  1102,  1664,  1764,  2381,   786,  2790,  2843,
    3467,  3466,   590,   840,   796,  1162,  3318,  1192,  1808,  2808,
    3305,  1835,  2407,   852,   609,  1227,  1224,  2442,  3448,  3449,
    3450,  2854,  1864,  1422,  2856,  1252,  2473,   872,  1929,  2872,
    2871,  3484,  3346,  1427,   578,   579,   580,   918,  3408,  2062,
    2898,  3522,  3166,  3413,  2069,  3493,  2535,  2904,  2913,  2912,
     743,  2550,  2104,  2610,  3424,  3350,   957,  1494,   625,   959,
    2588,  2130,  2954,  1533,  2130,  3715,  3520,  2083,  2068,  3393,
    3481,  3762,  3882,  3382,  2304,  3767,  2305,  2303,  3536,  1559,
    3770,  3599,  3490,  3535,  2372,  3491,  1203,  2371,  1202,  2959,
    3460,  2943,  2405,  2942,  3375,  2404,  3218,  3217,  2960,  2329,
     624,   793,  3414,  3415,  1241,  1867,  2581,  2581,     6,   680,
     943,  2581,  2581,   619,  4011,   649,  1232,  1156,  3034,   663,
    2350,  2671,  2352,   172,  3129,   818,  2090,  2867,    26,  1931,
    1159,  2325,  2362,  2601,  2286,  3565,  3976,  3826,   476,  3569,
     683,    39,   756,  3569,  2106,  2397,  2376,  3442,  3460,  2379,
     382,  2660,  2949,  3203,  3516,  3601,  2824,  3430,  3453,  3454,
    4213,  3456,  3457,  4008,   708,  3319,   251,  2888,  1797,  2399,
     727,   708,   579,  2111,  3499,  3041,  2406,  4013,  4008,  3747,
    3492,  4167,   875,  1576,    -1,    -1,    -1,    -1,  3483,    -1,
    4026,    -1,   763,  3639,  1100,    -1,    -1,    -1,   605,  3494,
      -1,    -1,   434,   774,  3529,    -1,    -1,  4128,    -1,    -1,
      -1,    -1,    -1,    -1,   785,  3550,  3551,  3587,   450,    -1,
    2450,   119,    -1,    -1,    -1,    -1,  3674,    -1,  3537,    -1,
    3525,    -1,    -1,    -1,    -1,  2465,  3571,  3572,  3550,  3551,
     647,    -1,    -1,  2473,   788,    -1,  3959,    -1,    -1,    -1,
      -1,  3546,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3571,
    3572,    -1,    -1,    -1,    -1,  3713,   810,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  3587,    -1,    -1,    -1,    -1,
     851,    -1,    -1,    -1,    -1,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2523,    -1,    -1,  3621,    -1,  3610,    -1,
      -1,    -1,    -1,    -1,  3734,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,   550,    -1,
    3750,    -1,    -1,    -1,    -1,    -1,   558,    -1,    -1,    -1,
      52,    -1,    -1,    -1,    -1,    -1,  3648,     7,   882,    -1,
      -1,    -1,    64,    -1,    -1,    -1,    -1,  2577,    -1,    -1,
      -1,  3646,  3661,  3662,    -1,  2585,   763,  3652,    -1,    -1,
      -1,    -1,    -1,   770,   771,   772,  3736,    -1,  3703,    -1,
      -1,  2601,    -1,   780,    -1,  2605,    -1,    -1,  4165,  4166,
    3826,    -1,    52,    -1,    -1,    -1,   108,    -1,    -1,   621,
      -1,  3703,    -1,    -1,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   634,    -1,    -1,    -1,    -1,  3742,    -1,   131,
      -1,    -1,    -1,  4200,   646,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  3718,    -1,    -1,    -1,    -1,  3723,    -1,
    3742,  4218,    -1,    -1,    -1,    -1,  3761,  3867,   108,  3734,
    3735,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  3751,  3752,    -1,    -1,
      -1,   131,    -1,   185,    -1,  3835,  2095,    -1,    -1,  3764,
      -1,   703,   704,    -1,    -1,    -1,  3771,   709,    -1,  3774,
    3775,  3776,  3777,    -1,   382,    -1,    -1,  4274,    -1,    -1,
      -1,   723,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2738,    -1,
     742,    -1,    -1,  2743,    -1,   185,   923,    -1,  3813,    -1,
      -1,    -1,    -1,  3971,  3894,    -1,    -1,  3822,    -1,    -1,
    3825,    -1,    -1,    -1,    -1,    -1,   434,    -1,    -1,    -1,
      -1,    -1,   949,   265,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   450,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   795,    -1,    -1,    -1,   799,   975,    -1,
      -1,    -1,    -1,  3943,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  3952,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   265,    -1,    -1,    -1,    -1,
      -1,    -1,   324,    -1,    -1,  3900,  3901,  3902,  3903,  3904,
     332,    -1,  3907,  3908,  3909,  3910,  3911,  3912,  3913,  3914,
    3915,  3916,  3917,  3935,    -1,  3920,    -1,    -1,    -1,    -1,
    3942,    10,    -1,  3928,  3946,    -1,    15,  3949,    -1,    -1,
      19,    20,    21,    -1,  3939,    -1,  3941,    -1,    27,    -1,
      -1,    -1,   550,    -1,   324,    -1,  3951,    -1,    -1,    -1,
     558,    -1,   332,    -1,    -1,    -1,    -1,    -1,  2898,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     412,    -1,    -1,    -1,    -1,    -1,    -1,   419,    -1,    -1,
      -1,    -1,    -1,    -1,  1111,  1112,  1113,    -1,    -1,    -1,
      -1,    -1,  1119,  1120,  1121,  1122,    -1,    -1,  1125,   441,
      -1,    -1,    -1,   621,    -1,    -1,    -1,    -1,  1135,    -1,
    1137,  1138,   111,    -1,  4029,    -1,   634,  1144,  1145,  1146,
    1147,  1148,   412,    -1,    -1,   467,    -1,    -1,   646,   419,
      -1,    -1,  1159,    -1,    -1,  4125,  4068,    -1,    -1,  4071,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1174,    -1,    -1,
     149,   441,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  4082,  4083,    -1,
    4085,   170,    -1,  4163,    -1,    -1,  3026,   467,    -1,    -1,
      -1,    48,    49,    50,    51,   703,    -1,  4102,    55,    -1,
      -1,   709,  4107,  4108,    -1,    -1,    -1,   539,    -1,    -1,
      -1,    -1,    -1,  4193,  4194,   723,    73,    74,    -1,  4124,
      -1,    -1,  4127,    -1,    -1,  4130,    -1,    -1,    -1,    -1,
    4165,  4166,    -1,    -1,   742,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2490,    -1,    -1,    10,    -1,    -1,    -1,    -1,    15,
      -1,    -1,    -1,    -1,   121,  4200,    -1,    -1,    -1,    -1,
      -1,    27,    -1,  3113,    -1,   607,    -1,    -1,    -1,   611,
      -1,    -1,    -1,  4218,   273,    -1,    -1,   795,    -1,    -1,
      -1,   799,    -1,    -1,    -1,    -1,    -1,    -1,  4278,    -1,
      -1,   290,    -1,   160,    -1,    -1,  3146,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2564,    -1,   607,    -1,    -1,
      -1,   611,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  4274,
     329,   330,   331,    -1,    -1,    -1,    -1,    -1,  2587,  2588,
      -1,   683,   341,    -1,    -1,  2594,    -1,    -1,    -1,    -1,
      -1,  3201,    -1,   352,   353,    -1,    -1,    -1,    -1,  3209,
      -1,  3211,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   368,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  4165,  4166,    -1,
      -1,    -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,   683,    -1,    -1,    -1,    -1,    -1,   398,
      -1,   743,    -1,  1430,   170,    -1,    -1,   274,    -1,    -1,
      -1,    -1,  4200,    -1,   756,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   425,   426,   427,   428,
    4218,    -1,    -1,    -1,   433,    -1,   435,    -1,   780,    -1,
      -1,    -1,    -1,   442,    -1,    -1,    -1,   446,   447,   448,
      -1,    -1,    -1,   743,  1481,   454,    -1,    -1,   457,    -1,
     327,    -1,    -1,    -1,    -1,    -1,   756,    -1,    -1,    -1,
      -1,    -1,    -1,   472,    -1,    -1,   818,    -1,    -1,  3329,
      -1,    -1,    -1,    -1,    -1,    -1,  4274,    -1,    -1,    -1,
     780,    -1,   491,    -1,    -1,    -1,    -1,  3347,  3348,   366,
      -1,  1528,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    15,    -1,   514,    -1,    -1,  1545,  3369,
      -1,   863,  3372,    -1,   290,   524,    -1,    -1,   818,    -1,
      -1,    -1,    -1,   875,   533,    -1,   535,    -1,    -1,   881,
      -1,   883,    -1,   542,    -1,    -1,    -1,    -1,    -1,   548,
      -1,    -1,    -1,    -1,  3404,    -1,    -1,  2806,    -1,    -1,
      -1,   560,    -1,   329,   330,   331,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   863,    -1,   341,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   582,    -1,   875,    -1,    -1,   455,    -1,
      -1,   881,    -1,   883,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   368,    -1,    -1,   107,    -1,    -1,    -1,    -1,
      -1,   610,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  3474,    -1,    -1,    -1,    -1,    -1,
      -1,  3481,   398,    -1,  3484,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
    2899,    -1,    -1,   155,   156,    -1,    -1,   656,    -1,   425,
     426,    -1,    -1,    -1,    -1,    -1,    -1,   433,   170,   435,
      -1,    -1,    -1,    -1,    -1,    -1,   442,    -1,  1705,  3529,
     446,   447,   448,    -1,    -1,    -1,    -1,    -1,   454,   556,
      -1,   457,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   472,    -1,    -1,  1736,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,   728,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  3596,    -1,   514,    -1,
      -1,    -1,    -1,    -1,   753,   754,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   535,
      -1,  3621,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   548,    -1,    -1,    -1,    -1,    -1,   655,    -1,
     657,   658,    -1,   792,   560,   794,    -1,   796,    -1,    -1,
      -1,   800,    -1,   802,   803,   804,   805,    -1,   807,   808,
     677,    -1,    -1,    -1,    -1,   682,    -1,    -1,    -1,    -1,
     819,  3671,    -1,    -1,    -1,    -1,    -1,   329,   330,   331,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   341,
      -1,    -1,    -1,    -1,   610,    -1,   845,    -1,    -1,    -1,
      -1,    -1,    -1,   720,   853,  3104,  3105,  3106,  3107,    -1,
      -1,    -1,    -1,    -1,    -1,   732,   368,    -1,    -1,    -1,
    3720,  3721,    -1,    -1,    -1,    -1,    -1,    -1,   877,    -1,
      -1,  3731,  3732,    -1,   883,    -1,  3736,    -1,    -1,    -1,
     656,    -1,    -1,  3142,  3143,    -1,   895,   329,   330,   331,
      -1,    -1,    -1,    -1,    -1,  1932,    -1,    -1,    -1,   341,
      -1,  3761,  3762,    -1,    -1,    -1,    -1,  3767,   420,    -1,
      -1,    -1,    -1,  3773,   426,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3788,    -1,
     442,    -1,    -1,    -1,   446,   447,   448,    -1,    -1,  3799,
      -1,    -1,   454,    -1,    -1,   457,    -1,    -1,   825,    -1,
      -1,  3811,   728,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  3826,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   753,   754,    -1,
      -1,    -1,    -1,  3843,   426,    -1,  3846,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     442,    -1,    -1,    -1,   446,   447,   448,    -1,    -1,    -1,
      -1,    -1,   454,    -1,    -1,    -1,   792,    -1,   794,    -1,
      -1,    -1,    -1,   535,    -1,  2062,    -1,   803,   804,   805,
      -1,   807,   808,  3893,    -1,    -1,  3896,  3897,  3898,  3899,
      -1,    -1,    -1,   819,    -1,    -1,  3906,    -1,  2085,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3919,
      -1,    -1,    -1,    -1,    -1,  3925,    -1,    -1,    -1,   845,
      -1,  3931,  3932,    -1,    -1,    -1,    -1,   853,  2115,  2116,
      -1,  3340,    -1,  3943,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,  3952,   535,    -1,    -1,    -1,    -1,   610,    -1,
      10,   877,    -1,    -1,    -1,    15,    -1,   883,    -1,    19,
      20,    21,    -1,    -1,    -1,    -1,    -1,    27,    28,   895,
      -1,    -1,    -1,  3983,  3984,  3985,  3986,  3987,  3988,  3989,
    3990,  3991,  3992,  3993,  3994,  3995,  3996,  3997,  3998,  3999,
    4000,  4001,    -1,    -1,   656,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  4013,  4014,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,  4026,    -1,    -1,    -1,
      -1,    10,    -1,    -1,    -1,    -1,    15,    -1,  4038,    -1,
      19,    20,    21,    -1,    -1,    -1,    -1,    -1,    27,    28,
    4050,    -1,    -1,    -1,    -1,    -1,    -1,  4057,    -1,    -1,
      -1,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   656,    -1,   728,  3476,    -1,  4079,
    4080,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  4098,   149,
      -1,   753,   754,  4103,  4104,   155,   156,    -1,    -1,  4109,
      -1,  4111,  4112,    -1,    -1,    -1,  4116,  4117,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  4131,   111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   805,    -1,  4155,    -1,    -1,    -1,    -1,
      -1,    -1,   814,    -1,    -1,  4165,  4166,   819,    -1,    -1,
     149,   753,   754,  2350,    -1,  2352,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2362,    -1,    -1,  4188,    -1,
      -1,   170,    -1,   845,    -1,    -1,    -1,    -1,    -1,  2376,
    4200,   853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  4218,    -1,
    4220,  4221,  2399,   273,    -1,   877,    -1,    -1,    -1,    -1,
      -1,   883,    -1,    -1,    -1,  4235,    -1,   819,    -1,    -1,
     290,  4241,    -1,   895,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  4251,  4252,    -1,    -1,    -1,    -1,  4257,    -1,    -1,
      -1,    -1,    -1,   845,    -1,  4265,    -1,    -1,    -1,    -1,
      -1,   853,    -1,    -1,  4274,    -1,    -1,    -1,    -1,   329,
     330,   331,    -1,    -1,    -1,    -1,    -1,    -1,  2465,    -1,
      -1,   341,    -1,    -1,   273,   877,  2473,    -1,    -1,    -1,
      -1,   883,   352,   353,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   290,    -1,    -1,    -1,    -1,    -1,    -1,   368,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   398,    -1,
     329,   330,   331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   341,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   352,   353,   425,   426,   427,   428,    -1,
      -1,    -1,    -1,   433,    -1,   435,    -1,    -1,    -1,   368,
      -1,    -1,   442,    -1,    -1,    -1,   446,   447,   448,    -1,
      -1,    -1,    -1,    -1,   454,    -1,    -1,   457,  2585,    -1,
      -1,    48,    49,    50,    51,    -1,    -1,    -1,    55,   398,
      -1,    -1,   472,    -1,    -1,    -1,    -1,    -1,  2605,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,
      -1,   491,    -1,    -1,    -1,    -1,   425,   426,   427,   428,
      -1,    -1,    -1,    -1,   433,    -1,   435,    -1,    -1,    -1,
      -1,    -1,    -1,   442,   514,    -1,    -1,   446,   447,   448,
      -1,    -1,    -1,    -1,   524,   454,    -1,    -1,   457,    -1,
      -1,    -1,    -1,   533,   121,   535,    -1,    -1,    -1,    -1,
      -1,    -1,   542,   472,    -1,    -1,    -1,    -1,   548,    -1,
      -1,   551,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    40,
     560,    -1,   491,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    -1,    -1,   160,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   582,    -1,    -1,   514,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,   524,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   533,    -1,   535,    -1,    -1,    -1,
     610,    -1,    -1,   542,    -1,    -1,     1,    -1,    -1,   548,
      -1,    -1,    -1,    -1,    -1,    10,    -1,    -1,    -1,    -1,
      15,   560,    -1,    -1,    19,    20,    21,    -1,    -1,   120,
     121,    -1,    27,   124,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   582,    -1,    -1,   656,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
      -1,   610,    -1,    -1,    -1,    -1,    -1,   274,    -1,    -1,
      -1,    -1,    -1,    15,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    -1,    -1,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   111,   656,   728,    -1,
      -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,
     327,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   753,   754,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   366,
      -1,   121,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,   792,   274,   794,    -1,   796,    -1,   798,   728,
     800,    -1,   802,   803,   804,   805,    -1,   807,   808,   809,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   819,
     160,    -1,    -1,    -1,   753,   754,    -1,   149,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   845,   327,    -1,   170,    -1,
      -1,    -1,    -1,   853,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   792,    -1,   794,    -1,   796,   455,   798,
      -1,   800,    -1,   802,   803,   804,   805,   877,   807,   808,
     809,    -1,    -1,   883,    -1,   366,    -1,    -1,   273,    -1,
     819,    -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   289,   290,    -1,    -1,    -1,    -1,
      -1,    -1,   499,    -1,    -1,    -1,   845,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   853,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   274,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,    -1,   329,   330,   331,    -1,   877,    -1,
      -1,    -1,    -1,   434,   883,    -1,   341,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   895,   352,   353,   556,
      -1,    -1,    -1,    -1,   455,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   368,    -1,    -1,    -1,   327,    -1,    -1,
      -1,    -1,   473,    -1,    56,    57,    58,    59,    60,    -1,
      62,    63,    -1,    -1,    -1,    -1,   328,   329,   330,   331,
      -1,     1,    -1,   398,    -1,    -1,    -1,    -1,   499,   341,
      10,    -1,    -1,    -1,    -1,    15,   366,    -1,    -1,    19,
      20,    21,    -1,    -1,    -1,    -1,    -1,    27,    -1,    -1,
     425,   426,   427,   428,    -1,    -1,   368,    -1,   433,    -1,
     435,    -1,    -1,    -1,    -1,    -1,    -1,   442,    -1,    -1,
      -1,   446,   447,   448,    -1,    -1,    -1,    -1,   655,   454,
     657,   658,   457,    -1,    -1,   556,   138,    -1,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   472,    -1,    -1,
     677,    -1,    -1,    -1,    -1,   682,    -1,    -1,   579,   580,
      -1,    -1,    -1,   425,   426,    -1,   491,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   455,    -1,    -1,    -1,    -1,
     442,   111,    -1,    -1,   446,   447,   448,    -1,    -1,   514,
      -1,    -1,   454,   720,    -1,   457,    -1,    -1,    -1,   524,
      -1,    -1,    -1,    -1,   206,   732,    -1,    -1,   533,    -1,
     535,    -1,    -1,    -1,    -1,    -1,    -1,   542,   498,   149,
      -1,    -1,    -1,   548,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   655,   560,   657,   658,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   677,   582,    -1,    -1,
      -1,   682,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   535,    -1,    -1,   556,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   610,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   825,   720,
      -1,    -1,  3369,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   732,   314,   315,   316,   317,   318,    -1,    -1,   321,
     322,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   656,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   273,    -1,    -1,    -1,    -1,   610,    -1,
      -1,    -1,    -1,    -1,    -1,   357,    -1,    -1,    -1,    -1,
     290,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   655,    -1,   657,   658,   381,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   656,    -1,    -1,   677,    -1,   329,
     330,   331,   682,   728,   825,    -1,    -1,    -1,    -1,    -1,
      -1,   341,    -1,    -1,    -1,    -1,    -1,    -1,   839,    -1,
      -1,    -1,   352,   353,    -1,    -1,    -1,    -1,   753,   754,
      -1,    -1,  3499,    -1,    -1,    -1,    -1,    -1,   368,    -1,
     720,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   732,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   882,  3529,    -1,    -1,    -1,   728,   792,   398,   794,
      -1,   796,    -1,    -1,    -1,   800,    -1,   802,   803,   804,
     805,   483,   807,   808,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   753,   754,    -1,   819,   425,   426,   427,   428,    -1,
      -1,    -1,    -1,   433,    -1,   435,    -1,    -1,    -1,    -1,
      -1,    -1,   442,    -1,   776,    -1,   446,   447,   448,    -1,
     845,    -1,    -1,    -1,   454,    -1,    -1,   457,   853,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   472,   805,    -1,   825,    -1,    -1,    -1,    -1,
      -1,    -1,   877,    -1,  3621,   557,    -1,   819,   883,    -1,
      -1,   491,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     895,    -1,    -1,   835,    -1,   577,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   845,   514,    -1,    -1,    -1,    -1,    -1,
      -1,   853,    -1,    -1,   524,    -1,    -1,   599,    -1,    -1,
      -1,    -1,   864,   533,    -1,   535,    -1,    -1,    -1,    -1,
      -1,    -1,   542,    -1,    -1,   877,    -1,    -1,   548,    -1,
     882,   883,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     560,    -1,    -1,   895,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   582,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     610,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  3761,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   716,   717,   718,    -1,    -1,    -1,
      -1,    -1,     3,    -1,    -1,    -1,   656,     8,    -1,    -1,
      11,    -1,    -1,    14,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    26,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3826,
      -1,    -1,    -1,    44,    45,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    -1,    -1,    -1,    76,    77,    78,   728,    80,
      81,    -1,    -1,    84,    -1,    -1,    87,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    95,    96,    97,    98,    99,    -1,
     101,   102,    -1,   753,   754,    -1,    -1,    -1,   830,   831,
     832,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,    -1,    -1,   125,   126,   127,   128,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   792,    -1,   794,    -1,   796,    -1,    -1,    -1,
     800,    -1,   802,   803,   804,   805,    -1,   807,   808,    -1,
      -1,   883,   163,    -1,    -1,    -1,    -1,    -1,    -1,   819,
     171,   172,   173,   174,   175,   176,   177,   178,    -1,   180,
      -1,   182,   183,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   193,    -1,    -1,   845,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   853,    -1,    -1,   207,   208,    -1,    -1,
     211,   212,    -1,   214,   215,   216,   217,   218,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   877,    -1,    -1,
      -1,    -1,    -1,   883,    -1,    -1,    -1,   238,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     261,   262,    -1,    -1,    -1,   266,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   277,    -1,    -1,    -1,
     281,    -1,    -1,    -1,    -1,   286,   287,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   300,
     301,   302,   303,   304,    -1,    -1,   307,    -1,    -1,    -1,
     311,   312,   313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   333,    -1,    -1,    -1,    -1,   338,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   346,   347,   348,   349,    -1,
      -1,    -1,    -1,    -1,   355,   356,    -1,   358,    -1,   360,
     361,   362,   363,   364,   365,    -1,   367,    -1,   369,   370,
     371,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  4165,  4166,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   390,
     391,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   399,   400,
     401,   402,   403,   404,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   413,  4200,    -1,    -1,   417,    -1,    -1,    -1,
     421,   422,    -1,   424,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  4218,    -1,    -1,    -1,    -1,    -1,    -1,   439,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     451,   452,   453,    -1,    -1,   456,    -1,    -1,    -1,   460,
     461,   462,   463,   464,    -1,    -1,    -1,    -1,    -1,    -1,
     471,    -1,    -1,    -1,    -1,    -1,   477,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   487,  4274,    -1,    -1,
      -1,    -1,    -1,    -1,   495,    -1,    -1,    -1,    -1,   500,
     501,   502,    -1,   504,    -1,   506,   507,    -1,   509,   510,
     511,    -1,    -1,   514,   515,    -1,   517,   518,   519,   520,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   529,   530,
      -1,    -1,   533,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     541,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   554,   555,    -1,    -1,    -1,    -1,    -1,
     561,   562,    -1,   564,    -1,    -1,    -1,   568,    -1,    -1,
      -1,    -1,    -1,    -1,   575,    -1,    -1,    -1,    -1,    -1,
     581,    -1,    -1,    -1,   585,   586,   587,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   600,
      -1,    -1,   603,   604,    -1,    -1,    -1,   608,    -1,    -1,
      -1,    -1,   613,    -1,    -1,    -1,    -1,   618,    -1,    -1,
      -1,   622,   623,   624,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   632,   633,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   649,   650,
     651,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   663,   664,   665,    -1,    -1,    -1,   669,   670,
     671,   672,   673,   674,    -1,    -1,    -1,   678,   679,    -1,
      -1,    -1,    -1,    -1,    -1,   686,   687,    -1,    -1,    -1,
      -1,    -1,    -1,   694,   695,   696,   697,    -1,    -1,    -1,
      -1,   702,    -1,    -1,    15,    -1,   707,    -1,    -1,   710,
     711,   712,   713,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,     5,    -1,    -1,   725,     9,    -1,    -1,    -1,   730,
     731,    -1,    16,    -1,    -1,    -1,    -1,   738,   739,    -1,
     741,    25,    -1,   744,    -1,    -1,    -1,    -1,   749,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   766,   767,    -1,    -1,   770,
      -1,    -1,    -1,    -1,   775,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   783,   784,   785,    -1,    -1,    -1,   789,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   815,    -1,   817,    -1,    -1,    -1,
     821,    -1,    -1,    -1,    -1,   826,    -1,    -1,    -1,    -1,
      -1,   115,   833,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,    -1,    -1,   844,   155,   156,    -1,   848,   849,   850,
      -1,   135,    -1,    -1,    -1,   139,    -1,    -1,    -1,   170,
     861,    -1,    -1,    -1,   865,   866,   867,    -1,   869,   870,
     871,   872,   873,   157,    -1,    -1,    -1,    -1,   879,   880,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   889,    -1,
      -1,   892,    -1,    -1,    -1,    -1,    -1,   181,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   195,    -1,    -1,    -1,    -1,    -1,   201,    -1,   203,
      -1,    -1,   206,    -1,    -1,    -1,   210,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   227,    -1,   229,    -1,    -1,    -1,   233,
     234,   235,   236,   237,    -1,   239,   240,   241,   242,    -1,
     244,   245,    -1,   247,   248,    -1,   250,   251,   252,   253,
     254,   255,   256,   257,   258,   259,   260,    -1,    -1,   263,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   271,    -1,    -1,
      -1,    -1,   276,    -1,    -1,   279,    -1,    -1,    -1,    -1,
     284,    -1,    -1,    -1,    -1,   289,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   329,   330,
     331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     341,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     334,    -1,    -1,    -1,    -1,    -1,    -1,   368,   342,    -1,
      -1,   345,    -1,    -1,    -1,    -1,   350,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   376,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     384,    -1,   386,    -1,    -1,   389,    -1,    -1,   392,    -1,
      -1,    -1,    -1,   397,    -1,   426,    -1,    -1,     1,    -1,
      -1,    -1,     5,   407,    -1,    -1,     9,    -1,    -1,    -1,
      -1,   442,    -1,    16,    -1,   446,   447,   448,    -1,    -1,
      -1,    -1,    25,   454,    -1,    -1,   457,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   468,    -1,    -1,    -1,    -1,    -1,
     474,    -1,   476,    -1,    -1,   479,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    -1,
     494,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   503,
      -1,    -1,    -1,    -1,   535,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,    -1,    -1,    -1,    -1,   521,   522,   523,
      -1,   525,   526,   527,   528,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   135,    -1,    -1,    -1,   139,    -1,    -1,    -1,
      -1,   545,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   157,    -1,     1,    -1,    -1,    -1,
       5,    -1,    -1,   567,     9,    -1,    -1,    -1,    -1,    -1,
      -1,    16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   610,
      25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   195,    -1,    -1,    -1,    -1,    -1,   201,    -1,
     203,    -1,   606,   206,    -1,    -1,    -1,   210,   612,    -1,
      -1,    -1,   616,    -1,    -1,   619,   620,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   227,   656,    -1,    -1,    -1,    -1,
      -1,   635,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    86,    -1,    -1,   648,    -1,    -1,    -1,    -1,   653,
      -1,    -1,    -1,    -1,    -1,    -1,   660,    -1,    -1,    -1,
     263,    -1,   666,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,    -1,    -1,    -1,   279,    -1,    -1,    -1,
      -1,   685,    -1,    -1,    -1,    -1,   289,    -1,    -1,    -1,
     135,    -1,    -1,    -1,   139,   699,    -1,   728,    -1,    -1,
     149,   705,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,   157,    -1,    -1,    -1,    -1,   721,    -1,   723,
      -1,   170,   753,   754,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   334,    -1,   737,    -1,    -1,    -1,    -1,    -1,   342,
      -1,    -1,   345,   747,   748,    -1,    -1,   350,    -1,    -1,
     195,   755,    -1,    -1,   758,    -1,   201,    -1,   203,    -1,
      -1,   206,    -1,    -1,    -1,   210,    -1,    -1,    -1,   773,
      -1,    -1,    -1,   376,   805,    -1,    -1,    -1,    -1,    -1,
      -1,   384,   227,   386,    -1,    -1,   389,    -1,   819,   392,
      -1,   795,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   806,    -1,   407,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   816,    -1,   845,    -1,    -1,    -1,   263,    -1,
      -1,    -1,   853,    -1,   828,    -1,    -1,    -1,    -1,    -1,
     834,    -1,    -1,    -1,   279,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   289,    -1,   877,    -1,    -1,    -1,
      -1,    -1,   883,    -1,   858,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   895,   468,    -1,    -1,    -1,    -1,
      -1,   474,   876,   476,    -1,    -1,   479,    -1,    -1,    -1,
     329,   330,   331,   887,    -1,    -1,   890,    -1,    -1,   334,
      -1,   494,   341,    -1,    -1,    -1,    -1,   342,    -1,    -1,
     345,    -1,    -1,    -1,    -1,   350,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   376,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   384,
      -1,   386,   545,    -1,   389,    -1,    -1,   392,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,     5,   407,    -1,   567,     9,    -1,    -1,    -1,    -1,
      -1,    -1,    16,    -1,    -1,    -1,   425,   426,    -1,    -1,
      -1,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   442,    -1,    -1,    -1,   446,   447,   448,
      -1,    -1,    -1,   606,    -1,   454,    -1,    -1,    -1,   612,
      -1,    -1,    -1,   616,    -1,    -1,   619,   620,    -1,    -1,
      -1,    -1,    -1,   468,    -1,    -1,    -1,    -1,    -1,   474,
      -1,   476,   635,    -1,   479,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    86,    -1,    -1,   648,    -1,    -1,    -1,   494,
     653,    -1,    -1,    -1,    -1,    -1,    -1,   660,    -1,    -1,
      -1,    -1,    -1,   666,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   685,    -1,    -1,    -1,   535,    -1,    -1,    -1,
      -1,   135,    -1,    -1,    -1,   139,   699,    -1,    -1,    -1,
     545,    -1,   705,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    -1,    -1,    -1,    -1,    -1,
     723,    -1,   567,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   737,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   747,   748,    -1,    -1,    -1,    -1,
      -1,   195,   755,    -1,    -1,   758,    -1,   201,    -1,   203,
      -1,   606,   206,    -1,    -1,    -1,   210,   612,    -1,    -1,
     773,   616,    -1,    -1,   619,   620,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   227,    -1,    -1,    -1,    -1,    -1,    -1,
     635,    -1,   795,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   648,    -1,    -1,    -1,   656,   653,    -1,
      -1,    -1,    -1,   816,    -1,   660,    -1,    -1,    -1,   263,
      -1,   666,    -1,    -1,    -1,   828,    -1,    -1,    -1,    -1,
      -1,   834,    -1,    -1,    -1,   279,    -1,    -1,    -1,    -1,
     685,    -1,    -1,    -1,    -1,   289,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   699,   858,    -1,    -1,    -1,    -1,
     705,    -1,    -1,     5,    -1,    -1,    -1,     9,    -1,    -1,
      -1,    -1,    -1,    -1,    16,    -1,    -1,    -1,   723,    -1,
      -1,    -1,    -1,    25,   887,    -1,    -1,   890,    -1,    -1,
     334,    -1,   737,    -1,    -1,    -1,    -1,    -1,   342,    -1,
      -1,   345,   747,   748,   753,   754,   350,    -1,    -1,    -1,
     755,    -1,    -1,   758,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   773,    -1,
      -1,    -1,   376,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     384,    -1,   386,    85,    86,   389,    -1,    -1,   392,    -1,
     795,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   407,    -1,    -1,    -1,    -1,    -1,    -1,
     819,   816,    -1,   115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   828,    -1,    -1,    -1,    -1,    -1,   834,
      -1,    -1,    -1,   135,    -1,    -1,   845,   139,    -1,    -1,
      -1,    -1,    -1,    -1,   853,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   858,    -1,   157,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   468,    -1,    -1,    -1,   877,    -1,
     474,    -1,   476,    -1,   883,   479,    -1,    -1,    -1,    -1,
      -1,    -1,   887,    -1,    -1,   890,    -1,    -1,    -1,    -1,
     494,    -1,    -1,   195,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   206,    -1,    -1,    -1,   210,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   229,    -1,    -1,
      -1,    -1,   234,   235,   236,   237,    -1,   239,   240,   241,
     242,   545,   244,    -1,   246,   247,   248,    -1,   250,   251,
     252,   253,   254,   255,   256,   257,   258,   259,   260,    -1,
      -1,   263,    -1,   567,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   279,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   289,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   606,    -1,    -1,    -1,    -1,    -1,   612,    -1,
      -1,    -1,   616,    -1,    -1,   619,   620,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   635,   334,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     342,    -1,    -1,   345,   648,    -1,    -1,    -1,   350,   653,
      -1,    -1,    -1,    -1,    -1,    -1,   660,    -1,    -1,    -1,
      -1,    -1,   666,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   376,    10,    -1,    -1,    -1,    -1,
      15,   685,   384,    -1,   386,    -1,    -1,   389,    -1,    -1,
     392,    -1,    -1,    -1,    -1,   699,    -1,    -1,    -1,    -1,
      -1,   705,    -1,    -1,    -1,    40,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    -1,    -1,   723,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   737,    -1,    -1,    -1,    -1,    73,    74,
      -1,    -1,    -1,   747,   748,    10,    -1,    -1,    -1,    -1,
      15,   755,    -1,    -1,   758,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   468,    -1,    -1,   773,
      -1,    -1,   474,    -1,   476,    -1,    -1,   479,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   120,   121,    -1,    -1,   124,
      -1,    -1,   494,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   816,    -1,   149,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,    -1,   828,   160,    -1,    -1,    -1,    -1,
     834,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   545,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   858,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   567,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   887,   149,    -1,   890,    -1,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   616,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   635,    -1,    -1,    -1,    -1,    -1,   274,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   653,    -1,    -1,    -1,    -1,    -1,    -1,   660,    -1,
      -1,    -1,    -1,    -1,   666,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   685,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   327,    -1,   329,   330,   331,    -1,    -1,    -1,
      -1,    -1,    -1,   705,    -1,    -1,   341,    10,    -1,   274,
      -1,    -1,    15,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   723,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   366,    -1,   368,    -1,   737,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   747,   748,    -1,    -1,    -1,
      -1,    -1,    -1,   755,    -1,    -1,   758,    -1,   323,    -1,
      -1,    -1,    -1,    -1,   329,   330,   331,    -1,    -1,    -1,
      -1,   773,    -1,    -1,    -1,    -1,   341,    -1,    -1,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     425,   426,    -1,   795,    -1,    -1,    -1,    -1,   433,   434,
     435,    -1,    -1,   368,    -1,    -1,    -1,   442,    -1,    -1,
      -1,   446,   447,   448,   816,   380,    -1,    -1,    -1,   454,
     455,    -1,   457,    -1,    -1,    -1,   828,    -1,    -1,    -1,
      -1,    -1,   834,    -1,    -1,    -1,    -1,   472,   473,    -1,
      10,    -1,    -1,    -1,    -1,    15,   149,    -1,    -1,    -1,
      -1,   154,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     425,   426,    -1,    -1,   499,    -1,    -1,   170,   433,    -1,
     435,    -1,    -1,    -1,    -1,    -1,    -1,   442,    -1,    -1,
      -1,   446,   447,   448,    -1,   887,    -1,    -1,   890,   454,
      -1,    -1,   457,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     535,    -1,    -1,    -1,   469,    -1,    -1,   472,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   556,    -1,    -1,    -1,   560,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   579,   580,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     535,    -1,    -1,    -1,    -1,   610,    -1,   542,    -1,   149,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   560,    -1,    -1,    -1,    -1,
     170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     655,   656,   657,   658,    -1,    -1,   329,   330,   331,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   341,    -1,
      -1,    10,   677,    -1,    -1,   610,    15,   682,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   368,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    10,    -1,    -1,    -1,    -1,    15,
      -1,    -1,    -1,    -1,    -1,   720,    -1,    -1,    -1,    -1,
      -1,   656,    -1,   728,    -1,    -1,    -1,   732,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   275,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   753,   754,
      -1,    -1,   425,   426,    -1,    -1,    -1,    -1,    -1,    -1,
     433,    -1,   435,    -1,    -1,    -1,    -1,    -1,    -1,   442,
      -1,    -1,    -1,   446,   447,   448,    -1,    -1,    -1,    -1,
      -1,   454,    -1,    -1,   457,    -1,    -1,    -1,    -1,   329,
     330,   331,    -1,   728,    -1,    -1,    -1,   732,    -1,   472,
     805,   341,    -1,   343,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,    -1,   819,    -1,   155,   156,   753,   754,
     825,   756,    -1,    -1,    -1,    -1,    -1,    -1,   368,    -1,
      -1,   170,    -1,    -1,   839,    -1,    -1,    -1,    -1,    -1,
     845,    -1,    -1,   149,    -1,    -1,    -1,    -1,   853,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,
      -1,    -1,   535,    -1,   170,    -1,   801,    -1,    -1,   542,
     805,    -1,   877,    -1,    -1,    -1,    -1,   882,   883,    -1,
      -1,    -1,    -1,    -1,   819,   425,   426,   560,    -1,    -1,
     895,    -1,    -1,   433,    -1,   435,    -1,    -1,    -1,    -1,
      -1,    -1,   442,    -1,    -1,    -1,   446,   447,   448,    -1,
     845,    -1,    -1,    -1,   454,    10,    -1,   457,   853,    -1,
      15,    -1,    -1,    -1,   263,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   472,    -1,    -1,    -1,    -1,   610,    -1,    -1,
      -1,    -1,   877,    -1,    -1,    -1,   881,    -1,   883,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   630,    -1,    -1,
     895,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   513,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   656,    -1,    -1,    -1,    -1,    -1,    -1,
     329,   330,   331,    -1,    -1,   535,    -1,    -1,    -1,    -1,
      -1,    -1,   341,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     560,    -1,    -1,   329,   330,   331,    -1,    -1,    -1,   368,
      -1,    -1,    -1,    -1,    -1,   341,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   720,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   728,    -1,    -1,    -1,    -1,
     155,   156,   368,    -1,    -1,    -1,    -1,    -1,    -1,   164,
     610,    -1,    10,    -1,    -1,   170,    -1,    15,    -1,    -1,
     753,   754,    -1,    -1,    -1,    -1,   425,   426,    -1,    -1,
      -1,    -1,    -1,    -1,   433,    -1,   435,    -1,    -1,    -1,
      -1,    -1,    -1,   442,    -1,    -1,    -1,   446,   447,   448,
      -1,    -1,    -1,    -1,   654,   454,   656,    -1,   457,   425,
     426,    -1,    -1,    -1,    -1,    -1,    -1,   433,    -1,   435,
      -1,    -1,   805,   472,    -1,    -1,   442,    -1,    -1,    -1,
     446,   447,   448,    -1,    -1,    -1,   819,    -1,   454,    -1,
      -1,   457,    -1,    -1,    -1,    -1,   829,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   472,    -1,    -1,    -1,
      -1,    -1,   845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     853,    -1,    -1,    -1,    -1,    -1,    -1,   860,   728,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   535,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   877,    -1,    -1,    -1,    -1,    -1,
     883,   149,    -1,   753,   754,    -1,    -1,   155,   156,    -1,
      -1,   560,   895,    -1,    -1,    -1,   164,    -1,    -1,   535,
      -1,    -1,   170,    -1,   329,   330,   331,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   341,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   560,    -1,    -1,    -1,    -1,    10,
      -1,    -1,    -1,    -1,    15,   805,    -1,    -1,    -1,    -1,
      -1,   610,    -1,   368,    -1,    -1,    -1,    -1,    -1,   819,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   610,   845,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   853,    -1,    -1,    -1,   656,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    10,
     425,   426,    -1,    -1,    15,    -1,    -1,   877,   433,    -1,
     435,    -1,   882,   883,    -1,    -1,    -1,   442,    -1,    -1,
     656,   446,   447,   448,    -1,   895,    -1,    -1,    -1,   454,
      -1,    -1,   457,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   472,    -1,    -1,
      -1,   720,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   728,
      -1,   329,   330,   331,    -1,    -1,    -1,    -1,   149,    -1,
      -1,    -1,    -1,   341,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   753,   754,    -1,    -1,    -1,   170,
      -1,    -1,   728,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     368,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     535,    -1,    -1,    -1,    -1,    -1,    -1,   753,   754,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   798,
      -1,    -1,   768,    -1,    -1,   560,   805,    -1,   149,    -1,
     809,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,
     819,    -1,    -1,    -1,    -1,    -1,    -1,   425,   426,   170,
      -1,    -1,    -1,    -1,    -1,   433,    -1,   435,    -1,   805,
      -1,    -1,    -1,    -1,   442,    -1,   845,    -1,   446,   447,
     448,    -1,    -1,   819,   853,   610,   454,    10,    -1,   457,
      -1,    -1,    15,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   472,    -1,    -1,    -1,   877,   845,
      -1,    -1,    -1,    -1,   883,    -1,    -1,   853,    -1,    -1,
      -1,    -1,    -1,    10,    -1,    -1,   895,    -1,    15,    -1,
      -1,   656,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   877,    -1,    -1,    -1,    -1,    -1,   883,   329,   330,
     331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   895,
     341,    -1,    -1,    -1,    -1,    -1,    -1,   535,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   368,    -1,    -1,
      -1,    -1,   560,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   728,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   329,   330,
     331,    -1,    -1,    -1,    -1,    -1,   149,    -1,   753,   754,
     341,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   610,   768,   425,   426,    -1,   170,    -1,    -1,
      -1,    -1,   433,    -1,   435,    -1,    -1,   368,    -1,    -1,
      -1,   442,   149,    -1,    -1,   446,   447,   448,   155,   156,
      -1,    -1,    -1,   454,    10,    -1,   457,    -1,    -1,    15,
     805,    -1,    -1,   170,    -1,    -1,    -1,    -1,   656,    -1,
      -1,   472,    -1,    -1,   819,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   425,   426,    -1,    -1,    -1,    -1,
     845,    -1,   433,    -1,   435,    -1,    -1,    -1,   853,    -1,
      -1,   442,    -1,    -1,    -1,   446,   447,   448,    -1,    -1,
      -1,    -1,    -1,   454,    -1,    -1,   457,    -1,    -1,    -1,
      -1,    -1,   877,    -1,   535,    -1,    -1,    -1,   883,    -1,
     728,   472,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     895,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   560,
      -1,    -1,    -1,    -1,    -1,   753,   754,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   329,   330,   331,    -1,
      -1,    -1,   335,   149,    -1,    -1,    -1,    -1,   341,   155,
     156,    -1,    -1,    -1,   535,    -1,    -1,    -1,    -1,   610,
      -1,   542,    -1,    -1,   170,    -1,    -1,   805,    -1,    -1,
      -1,    -1,   329,   330,   331,   368,    -1,    -1,    -1,   560,
      -1,   819,    -1,    -1,   341,    -1,    -1,    -1,    -1,    -1,
      -1,    10,    -1,    -1,    -1,    -1,    15,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   656,    -1,   845,    -1,    -1,
      -1,   368,    -1,    -1,    -1,   853,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   610,
      -1,    -1,   425,   426,    -1,    -1,    -1,    -1,    -1,   877,
     433,    -1,   435,    -1,    -1,   883,    -1,    -1,    -1,   442,
      -1,    -1,    -1,   446,   447,   448,    -1,   895,    -1,    -1,
      -1,   454,    -1,    -1,   457,    -1,    -1,    -1,   425,   426,
      -1,    -1,    -1,    -1,    -1,   656,   433,   728,   435,   472,
      -1,    -1,    -1,    -1,    -1,   442,    -1,    -1,    -1,   446,
     447,   448,    -1,    -1,    -1,    -1,    -1,   454,    -1,    -1,
     457,    -1,   753,   754,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   472,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   329,   330,   331,    -1,    -1,    -1,    -1,
     149,    -1,    -1,    -1,    -1,   341,   155,   156,    -1,   790,
      -1,    -1,   535,    -1,    -1,    -1,    -1,   728,    -1,    -1,
      -1,   170,    -1,    -1,   805,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   368,    -1,    -1,    -1,    -1,   560,   819,    -1,
      -1,    -1,   753,   754,    -1,    -1,    -1,    -1,   535,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   845,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   853,   560,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   610,    -1,   425,
     426,    -1,    -1,    -1,   805,    -1,   877,   433,    -1,   435,
      -1,    -1,   883,    -1,    -1,    -1,   442,    -1,   819,    -1,
     446,   447,   448,    -1,   895,    -1,    -1,    -1,   454,    -1,
      -1,   457,    -1,   610,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   656,   845,    -1,   472,    -1,    -1,    -1,
      -1,    -1,   853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   877,    -1,    -1,   656,
      -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     329,   330,   331,    -1,   895,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   341,    -1,    -1,    -1,    -1,    -1,    -1,   535,
      -1,    -1,    -1,    -1,    -1,   728,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   368,
      -1,    -1,    -1,    -1,   560,    -1,    -1,    -1,    -1,    -1,
     753,   754,    -1,   720,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   728,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   753,   754,    -1,    -1,
      -1,    -1,    -1,    -1,   610,    -1,   425,   426,    -1,    -1,
      -1,    -1,   805,    -1,   433,    -1,   435,    -1,    -1,    -1,
      -1,    -1,    -1,   442,    -1,    -1,   819,   446,   447,   448,
      -1,    -1,    -1,    -1,    -1,   454,    -1,    -1,   457,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,
     656,    -1,   845,   472,    -1,    -1,    -1,    -1,    -1,    -1,
     853,    -1,   819,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   877,    -1,    -1,    -1,   845,    -1,
     883,    -1,    -1,    -1,    -1,    -1,   853,    -1,     8,    -1,
      -1,    -1,   895,    -1,    14,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   535,    -1,    -1,    -1,
     877,    -1,   728,    -1,    -1,    -1,   883,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,   895,    -1,
      -1,   560,    -1,    -1,    -1,    -1,    -1,   753,   754,    -1,
      -1,    -1,    -1,    -1,    -1,    65,    66,    67,    68,    69,
      70,    71,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    95,    96,    97,    98,    -1,
      -1,   610,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   805,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   819,    15,   125,   126,   127,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,    -1,   845,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   853,    -1,    -1,
      -1,    -1,    -1,   163,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   171,   172,   173,   174,   175,   176,   177,   178,    -1,
      -1,   877,   182,   183,    -1,    -1,    -1,   883,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,   895,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   207,   208,    -1,
      -1,   211,   212,    -1,    -1,    -1,   216,    -1,    -1,   728,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   238,    -1,
      -1,    -1,    -1,    -1,   753,   754,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,    -1,   262,    -1,   155,   156,   266,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   170,
      -1,   281,    -1,    -1,    -1,    -1,   286,   287,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,
     300,   301,   302,   303,   304,    -1,    -1,   307,    -1,    -1,
     819,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   845,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   853,    -1,   346,   347,   348,   349,
      -1,    -1,    -1,    -1,    -1,   355,   356,    -1,   358,    -1,
     360,   361,   362,    -1,    -1,   365,    -1,   367,   877,    -1,
      -1,   371,    -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,
     390,   391,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   399,
     400,   401,   402,   403,   404,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   413,    -1,    -1,    -1,   417,    -1,    -1,
      -1,   421,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   328,   329,   330,
     331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     341,    -1,    -1,    -1,    -1,    -1,   456,    -1,    -1,    -1,
     460,   461,   462,   463,   464,    -1,    -1,    -1,    -1,    -1,
      -1,   471,    -1,    -1,    -1,    -1,    -1,   368,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   487,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   495,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   514,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   529,
     530,    -1,    -1,    -1,   425,   426,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   442,    -1,    -1,    -1,   446,   447,   448,    -1,    -1,
      -1,   561,    -1,   454,   564,    -1,   457,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   575,    -1,    -1,    -1,    -1,
      -1,   581,    -1,    -1,    -1,   585,   586,   587,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     600,    -1,    -1,   603,   604,    -1,    -1,    -1,   608,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   622,   623,   624,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   632,   633,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   535,    -1,    -1,    -1,    -1,   649,
     650,   651,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   669,
     670,   671,   672,    -1,   674,    -1,    -1,    -1,   678,   679,
      -1,    -1,    -1,    -1,    -1,    -1,   686,   687,    -1,    -1,
      -1,    -1,    -1,    -1,   694,   695,   696,    -1,    -1,    -1,
      -1,    -1,   702,    -1,    -1,    -1,    -1,   707,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   610,
      -1,    -1,    -1,    -1,    -1,   725,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     8,    -1,    -1,   738,   739,
      -1,    14,    -1,    -1,   744,    -1,    -1,    -1,    -1,   749,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   656,   766,   767,    -1,    -1,
      -1,    44,    -1,    -1,    -1,   775,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   783,    -1,    -1,    -1,    -1,    -1,   789,
      -1,    -1,    65,    66,    67,    68,    69,    70,    71,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    80,    -1,    -1,
      -1,    84,    -1,    -1,    -1,   815,    -1,    -1,    -1,    -1,
      -1,   821,    95,    96,    97,    98,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   728,    -1,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
      -1,    -1,   125,   126,   127,    -1,   129,    -1,    -1,    -1,
      -1,   861,   753,   754,    -1,    -1,    -1,   867,    -1,   869,
      -1,    -1,   872,    -1,    -1,    -1,    -1,    -1,    -1,   879,
      -1,    -1,    -1,    -1,    -1,   776,    -1,    -1,    -1,   889,
     163,    -1,   892,    -1,    -1,    -1,    -1,    -1,   171,   172,
     173,   174,   175,   176,   177,   178,    -1,    -1,    -1,   182,
     183,    -1,    -1,    -1,   805,    -1,    -1,    -1,    -1,   149,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,   819,    -1,
      -1,    -1,    -1,    -1,   207,   208,    -1,    -1,   211,   212,
     170,    -1,    40,   216,   835,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,   845,    -1,    -1,    55,    -1,    -1,
      -1,    -1,   853,    -1,    -1,   238,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   864,    -1,    73,    74,    -1,    76,    -1,
      78,    -1,    -1,    -1,    -1,    -1,   877,    -1,    -1,   262,
      -1,   882,   883,   266,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,   895,    -1,    -1,    -1,   281,    -1,
      -1,    -1,    -1,   286,   287,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   120,   121,    -1,    -1,   124,   300,   301,   302,
     303,   304,    -1,    -1,   307,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   346,   347,   348,   349,    -1,    -1,    -1,
      -1,    -1,   355,   356,    -1,   358,    -1,   360,   361,   362,
      -1,    -1,   365,    -1,   367,    -1,    -1,    -1,   371,   329,
     330,   331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   341,    -1,    -1,    -1,    -1,    -1,   390,   391,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   399,   400,   401,   402,
     403,   404,    -1,    -1,    -1,    -1,    -1,    -1,   368,    -1,
     413,    -1,    -1,    -1,   417,   418,    -1,    -1,   421,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   274,    -1,    -1,    -1,
      -1,    -1,    -1,   456,    -1,    -1,    -1,   460,   461,   462,
     463,   464,    -1,    -1,    -1,    -1,   426,    -1,   471,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   442,    -1,   487,    -1,   446,   447,   448,    -1,
      -1,    -1,   495,    -1,   454,    -1,    -1,   457,    -1,   327,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   529,   530,    -1,   357,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   366,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   561,    -1,
      -1,   564,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   575,    -1,    -1,   535,    -1,    -1,   581,    -1,
      -1,    -1,   585,   586,   587,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   422,    -1,    -1,   600,    -1,    -1,
     603,   604,    -1,    -1,    -1,   608,   434,    -1,    -1,    -1,
     438,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   622,
     623,   624,    -1,    -1,    -1,    -1,    -1,   455,    -1,   632,
     633,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   649,   650,   651,    -1,
     610,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
      -1,    -1,    -1,    -1,     7,    -1,   669,   670,   671,   672,
     498,   674,    -1,    -1,    -1,   678,   679,    -1,    -1,    -1,
      -1,    -1,    -1,   686,   687,    -1,    -1,    -1,    -1,    -1,
      -1,   694,   695,   696,    -1,    -1,   656,    -1,    -1,   702,
      43,    -1,    -1,    -1,   707,    48,    49,    50,    51,    52,
      -1,    -1,    55,    56,    57,    58,    59,    60,    -1,    62,
      63,    64,   725,    -1,    -1,    -1,    -1,    -1,   556,    -1,
      73,    74,    -1,    -1,    -1,   738,   739,    -1,    -1,    -1,
      -1,   744,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
     578,   579,   580,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   766,   767,   108,    -1,    -1,   728,    -1,
     113,    -1,   775,    -1,    -1,    -1,    -1,   120,   121,    -1,
     783,   124,    -1,    -1,    -1,    -1,   789,    -1,   131,    -1,
      -1,    -1,    -1,   753,   754,   138,    -1,   140,   141,   142,
     143,   144,   145,   146,   147,   148,    -1,    -1,    -1,    -1,
      -1,    -1,   815,    -1,    -1,    -1,    -1,    -1,   821,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,   655,    -1,   657,
     658,    -1,    -1,    -1,    -1,   663,    -1,    -1,    -1,    -1,
      -1,    -1,   185,    -1,    -1,   805,    -1,    -1,    -1,   677,
      -1,    -1,    -1,    -1,   682,    -1,    -1,    -1,   861,   819,
      -1,    -1,    -1,   206,   867,    -1,   869,    -1,    -1,   872,
      -1,    -1,    -1,    -1,    -1,    -1,   879,    -1,    -1,    -1,
     708,    -1,    -1,    -1,    -1,   845,   889,    -1,    -1,   892,
      -1,    -1,   720,   853,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   732,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   877,    -1,    -1,
      -1,    -1,   265,   883,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   274,    -1,    -1,    -1,   895,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     788,    -1,   305,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   314,   315,   316,   317,   318,    -1,    -1,   321,   322,
      -1,    -1,   810,    -1,   327,    -1,    -1,    -1,    -1,   332,
      -1,    -1,   335,    -1,   337,    -1,    -1,   825,    -1,    -1,
      -1,   344,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   354,    -1,    -1,   357,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   366,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   381,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   882,   398,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   408,    -1,    -1,    -1,   412,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   420,    -1,    -1,
     423,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   434,    -1,    -1,    -1,   438,    -1,    -1,   441,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   455,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     483,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   498,    -1,    -1,    -1,    -1,
      -1,    -1,   505,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   536,    -1,   538,    -1,   540,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   556,   557,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,   572,
      -1,    -1,    -1,    -1,   577,    -1,   579,   580,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   599,    -1,    -1,   602,
      -1,    -1,    -1,    -1,   607,    -1,    -1,    -1,   611,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    28,    -1,
      -1,    -1,   645,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   655,    -1,   657,   658,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    53,    -1,    -1,    56,    57,    58,    59,
      60,    -1,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
     683,   120,    -1,    73,   123,   124,    -1,   690,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   138,
      -1,   140,   141,   142,   143,   144,   145,   146,   147,   148,
      -1,   714,    -1,   716,   717,   718,    -1,   720,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   732,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     743,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   138,    -1,
     140,   141,   142,   143,   144,   145,   146,   147,   148,    -1,
      -1,    -1,   765,    -1,    -1,    -1,    -1,   206,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   790,    -1,    -1,
      -1,    -1,   795,    -1,    -1,    -1,    -1,    -1,   801,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   814,    -1,    -1,    -1,   818,   206,    -1,    -1,    -1,
      -1,    -1,   825,    -1,    -1,    -1,    -1,   830,   831,   832,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   846,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   854,    -1,    -1,    -1,    -1,    -1,   860,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   305,    -1,    -1,    -1,
      -1,    -1,   875,    -1,    -1,   314,   315,   316,   317,   318,
     883,    -1,   321,   322,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   292,   293,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   305,   355,    -1,   357,    -1,
      -1,    -1,    -1,    -1,   314,   315,   316,   317,   318,    -1,
      -1,   321,   322,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   381,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   344,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   357,    -1,   408,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   420,    -1,    -1,   374,    -1,    -1,    -1,    -1,    -1,
      -1,   381,    -1,    -1,    -1,   434,    -1,    -1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    11,   398,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   408,    -1,
      -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     420,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    -1,   483,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   496,    -1,    -1,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    77,    78,    -1,    -1,    81,    -1,    -1,
      -1,    -1,    -1,    87,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   483,    -1,    99,    -1,   101,   102,   538,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   557,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   572,    -1,    -1,    -1,    -1,   577,    -1,
      -1,    -1,    -1,    -1,   583,    -1,    -1,    -1,   538,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     599,    -1,    -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,    -1,
      -1,    -1,   572,    -1,    -1,    -1,    -1,   577,    -1,   193,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   599,
     214,   215,    -1,   217,   218,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   628,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   261,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   277,    -1,   714,    -1,   716,   717,   718,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   726,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   311,   312,   313,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   757,    -1,
      -1,    -1,    -1,    -1,   714,    -1,   716,   717,   718,   333,
      -1,    -1,    -1,    -1,   338,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   795,    -1,    -1,   363,
     364,    -1,    -1,    -1,    -1,   369,   370,    -1,    -1,    -1,
      -1,   761,    -1,    -1,    -1,   814,    -1,    -1,    -1,    -1,
      -1,    -1,   821,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   830,   831,   832,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   795,    -1,   846,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   422,    -1,
     424,   860,    -1,    -1,   814,   864,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   439,    -1,    -1,    -1,    -1,
     830,   831,   832,    -1,   883,    -1,    -1,   451,   452,   453,
      -1,    -1,    -1,    -1,    -1,    -1,   846,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     860,    -1,    -1,   477,    -1,    -1,    -1,    -1,   868,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,   883,    -1,    -1,   500,   501,   502,    -1,
     504,    -1,   506,   507,    -1,   509,   510,   511,    -1,    -1,
      -1,   515,    -1,   517,   518,   519,   520,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   533,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   541,    56,    57,
      58,    59,    60,    -1,    62,    63,    -1,    -1,    -1,    -1,
     554,   555,    -1,    -1,    -1,    73,    -1,    -1,   562,    -1,
      40,    -1,    -1,    43,   568,    -1,    -1,    -1,    48,    49,
      50,    51,    -1,    -1,    54,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   120,    -1,    -1,   123,   124,    -1,    -1,   613,
      -1,    -1,   130,    -1,   618,    -1,    -1,    -1,    -1,    -1,
     138,    -1,   140,   141,   142,   143,   144,   145,   146,   147,
     148,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     120,   121,    -1,    -1,   124,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   663,
     664,   665,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   673,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   158,    -1,
     160,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   206,    -1,
      -1,    -1,    -1,   697,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   710,   711,   712,   713,
      -1,    -1,   192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   730,   731,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   741,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   770,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     784,   785,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   314,   315,   316,   317,
     318,    -1,    -1,   321,   322,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   817,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   826,    -1,    -1,    -1,    -1,    -1,    -1,   833,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   355,    -1,   357,
     844,    -1,    -1,    -1,   848,   849,   850,   327,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   336,   337,    -1,    -1,
      -1,   865,   866,   381,    -1,    -1,   870,   871,    -1,   873,
      -1,    -1,    -1,    -1,    -1,    -1,   880,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   366,    -1,    -1,    -1,
     408,    -1,    -1,    -1,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   434,    -1,    -1,    53,
      -1,    -1,    56,    57,    58,    59,    60,    -1,    62,    63,
     410,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,   423,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   434,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   483,    -1,    -1,    -1,    -1,
      -1,    -1,   452,    -1,    -1,   455,    -1,    -1,   496,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   473,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   138,    -1,   140,   141,   142,   143,
     144,   145,   146,   147,   148,    -1,    -1,    -1,   498,    -1,
     538,    -1,    -1,    -1,    -1,   505,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   557,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   572,    -1,    -1,    -1,    -1,   577,
     540,    -1,    -1,    -1,    -1,   583,    -1,    -1,    -1,    -1,
      -1,    -1,   206,    -1,    -1,    -1,   556,    -1,    -1,    -1,
      -1,   599,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   579,
     580,    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   602,    -1,    -1,   605,    53,    -1,    -1,    56,
      57,    58,    59,    60,    -1,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   645,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   655,    -1,   657,   658,    -1,
     314,   315,   316,   317,   318,    -1,    -1,   321,   322,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   714,   677,   716,   717,
     718,    -1,   682,    -1,    -1,    -1,    -1,    -1,   726,    -1,
     690,   138,    -1,   140,   141,   142,   143,   144,   145,   146,
     147,   148,    -1,   357,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   757,
     720,    -1,    -1,    -1,    -1,    -1,    -1,   381,    -1,    -1,
      -1,    -1,   732,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   398,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   408,    -1,    -1,   795,    -1,   206,
      -1,    -1,    -1,    -1,    -1,   765,   420,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   821,    -1,   785,   786,    -1,    -1,    -1,
      -1,    -1,   830,   831,   832,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   846,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   860,    -1,    -1,   825,   864,    -1,    -1,   483,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   837,    -1,    -1,
      -1,    -1,    -1,    -1,   844,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   314,   315,   316,
     317,   318,    -1,    -1,   321,   322,    -1,    -1,    -1,    -1,
      -1,    -1,   882,    -1,   538,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   557,    -1,    -1,    -1,    -1,    -1,    -1,
     357,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   572,    -1,
      -1,    -1,    -1,   577,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   381,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   599,    -1,    -1,    -1,    -1,
      -1,   398,    43,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,   408,    -1,    -1,    55,    56,    57,    58,    59,    60,
      -1,    62,    63,   420,   628,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,    -1,    -1,    -1,    -1,    -1,   120,
     121,    -1,    -1,   124,    -1,    -1,   483,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   138,    -1,   140,
     141,   142,   143,   144,   145,   146,   147,   148,    -1,    -1,
     714,    -1,   716,   717,   718,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   538,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   761,    -1,    -1,
     557,    -1,    -1,    -1,    -1,   206,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   572,    -1,    -1,    -1,    -1,
     577,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   795,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   599,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     814,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   830,   831,   832,    -1,
      -1,   628,    -1,   274,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   846,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   860,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   868,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   314,   315,   316,   317,   318,    -1,    -1,
     321,   322,    -1,    -1,    -1,    -1,   327,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   335,    -1,   337,    -1,    -1,    -1,
      -1,    -1,    -1,   344,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   354,    -1,    -1,   357,   714,    -1,   716,
     717,   718,    -1,    -1,    -1,   366,   149,    -1,    -1,    -1,
      -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     381,    -1,    -1,    -1,    -1,    -1,    -1,   170,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   398,    -1,    -1,
      -1,    -1,    -1,    -1,   761,    -1,    -1,   408,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   423,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   434,    -1,    -1,    -1,   438,   795,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   455,    -1,    -1,   814,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   830,   831,   832,    -1,    -1,    -1,    -1,
      -1,    -1,   483,    -1,    -1,    -1,    -1,    -1,    -1,   846,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   498,    -1,    -1,
      -1,    -1,    -1,   860,   505,    -1,    -1,    -1,    -1,    -1,
      -1,   868,    43,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      -1,    62,    63,    -1,    -1,    -1,    -1,   538,    -1,   540,
      -1,    -1,    73,    74,    -1,    -1,   329,   330,   331,    -1,
      -1,    -1,    -1,    -1,    -1,   556,   557,    -1,   341,    -1,
      -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   572,    -1,    -1,    -1,    -1,   577,    -1,   579,   580,
      -1,    -1,   113,    -1,    -1,   368,    -1,    -1,    -1,   120,
     121,    -1,    -1,   124,    -1,    -1,    -1,    -1,   599,    -1,
      -1,   602,    -1,    -1,    -1,    -1,    -1,   138,    -1,   140,
     141,   142,   143,   144,   145,   146,   147,   148,    43,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    -1,    -1,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,   168,    -1,    -1,
      -1,    -1,   425,   426,   645,    -1,    -1,    -1,    -1,    74,
     433,    -1,   435,    -1,   655,    -1,   657,   658,    -1,   442,
      -1,    -1,    -1,   446,   447,   448,    -1,    -1,    -1,    -1,
      -1,   454,    -1,    -1,   457,   206,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   690,
      -1,    -1,    -1,    -1,    -1,    -1,   121,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   714,    -1,   716,   717,   718,    -1,   720,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   732,    -1,   158,    -1,   160,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   274,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   535,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   765,    -1,    -1,   192,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   560,    -1,    -1,
      -1,    -1,    -1,   314,   315,   316,   317,   318,    -1,   790,
     321,   322,    -1,    -1,   795,    -1,   327,    -1,    -1,    -1,
     801,    -1,    -1,    -1,   335,    -1,   337,    -1,    -1,    -1,
      -1,    -1,   149,   344,    -1,    -1,    -1,    -1,   155,   156,
      -1,    -1,    -1,   354,   825,    -1,   357,   610,    -1,   830,
     831,   832,    -1,   170,    -1,   366,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   846,    -1,    -1,    -1,    -1,
     381,    -1,    -1,   854,    -1,    -1,    -1,    -1,    -1,   860,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   398,    -1,    -1,
      -1,    -1,    -1,   656,   149,    -1,    -1,   408,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   423,    -1,    -1,   170,    -1,    -1,    -1,    -1,
      -1,    -1,   327,   434,    -1,    -1,    -1,   438,    -1,    -1,
      -1,    -1,   337,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   455,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   366,    -1,    -1,    -1,   728,    -1,    -1,    -1,    -1,
      -1,    -1,   483,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   498,    -1,    -1,
     753,   754,    -1,    -1,   505,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   329,   330,   331,    -1,    -1,    -1,   423,    -1,
      -1,    -1,    -1,    -1,   341,    -1,    -1,   538,    -1,   540,
      -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,    -1,   805,    -1,   807,   556,   557,   452,    -1,    -1,
     455,   368,    -1,   170,    -1,    -1,   819,    -1,    -1,    -1,
      -1,   572,    -1,    -1,    -1,    -1,   577,    -1,   579,   580,
      -1,    -1,    -1,    -1,   329,   330,   331,    -1,    -1,    -1,
      -1,    -1,   845,    -1,    -1,    -1,   341,    -1,   599,    -1,
     853,   602,    -1,   498,    -1,    -1,    -1,    -1,    -1,    -1,
     505,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   425,   426,
      -1,    -1,    -1,   368,   877,    -1,   433,    -1,   435,    -1,
     883,    -1,    -1,    -1,    -1,   442,    -1,    -1,    -1,   446,
     447,   448,   895,    -1,   645,   540,    -1,   454,    -1,    -1,
     457,    -1,    -1,    -1,   655,    -1,   657,   658,    -1,    -1,
      -1,   556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     425,   426,    -1,    -1,    -1,    -1,    -1,    -1,   433,   690,
     435,    -1,    -1,    -1,    -1,    -1,    -1,   442,    -1,    -1,
      -1,   446,   447,   448,    -1,    -1,    -1,   602,    -1,   454,
     605,    -1,   457,   714,    -1,   716,   717,   718,    -1,   720,
      -1,    -1,   329,   330,   331,    -1,    -1,    -1,   535,    -1,
      -1,   732,    -1,    -1,   341,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     645,    -1,    -1,   560,    -1,    -1,    -1,    -1,    -1,    -1,
     655,   368,   657,   658,   765,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   677,    -1,    -1,    -1,    -1,   682,    -1,   790,
     535,    -1,    -1,    -1,   795,   690,    -1,    -1,    -1,    -1,
     801,    -1,    -1,   610,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   560,    -1,    -1,   425,   426,
      -1,    -1,    -1,    -1,   825,   720,   433,    -1,   435,   830,
     831,   832,    -1,    -1,    -1,   442,   149,   732,    -1,   446,
     447,   448,   155,   156,    -1,   846,    -1,   454,    -1,   656,
     457,    -1,    -1,   854,    -1,    -1,    -1,   170,    -1,   860,
      -1,    -1,    -1,    -1,    -1,   610,    -1,    -1,    -1,    -1,
     765,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   656,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   728,    -1,    -1,    -1,    -1,    -1,    -1,   535,    -1,
     825,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   837,    -1,    -1,    -1,   753,   754,    -1,   844,
      -1,    -1,    -1,   560,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,   728,    -1,   792,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   170,   803,    -1,   805,    -1,
     807,   808,    -1,   610,    -1,    -1,    -1,    -1,   753,   754,
      -1,    -1,   819,    -1,    -1,    -1,   329,   330,   331,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   341,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,   845,    -1,
      -1,    -1,   155,   156,    -1,    -1,   853,   792,    -1,   656,
      -1,    -1,    -1,    -1,    -1,   368,    -1,   170,   803,    -1,
     805,    -1,   807,   808,    -1,    -1,    -1,    -1,    -1,    -1,
     877,    -1,    -1,    -1,   819,    -1,   883,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   895,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   853,    -1,
      -1,    -1,   425,   426,    -1,    -1,    -1,    -1,    -1,   149,
     433,   728,   435,    -1,    -1,   155,   156,    -1,    -1,   442,
      -1,    -1,   877,   446,   447,   448,    -1,    -1,   883,    -1,
     170,   454,    -1,    -1,   457,    -1,   753,   754,    -1,    -1,
     895,    -1,    -1,    -1,   329,   330,   331,   764,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   341,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,   368,    -1,    -1,   803,    -1,   805,    -1,
     807,   808,   170,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   819,    -1,    -1,    -1,   329,   330,   331,    -1,
      -1,    -1,   535,    -1,    -1,    -1,    -1,    -1,   341,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   845,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   853,   560,    -1,    -1,
     425,   426,    -1,    -1,    -1,   368,    -1,    -1,   433,    -1,
     435,    -1,    -1,    -1,    -1,    -1,    -1,   442,    -1,    -1,
     877,   446,   447,   448,    -1,    -1,   883,    -1,    -1,   454,
      -1,    -1,   457,    -1,    -1,    -1,    -1,    -1,   895,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   610,    -1,   329,
     330,   331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   341,   425,   426,    -1,    -1,    -1,    -1,    -1,    -1,
     433,    -1,   435,    -1,    -1,    -1,    -1,    -1,    -1,   442,
      -1,    -1,    -1,   446,   447,   448,    -1,    -1,   368,    -1,
      -1,   454,    -1,   656,   457,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     535,   329,   330,   331,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   341,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   560,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   425,   426,    -1,    -1,    -1,
     368,    -1,    -1,   433,    -1,   435,    -1,    -1,    -1,    -1,
      -1,    -1,   442,    -1,    -1,   728,   446,   447,   448,    -1,
      -1,    -1,   535,    -1,   454,    -1,    -1,   457,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   610,    -1,    -1,    -1,    -1,
     753,   754,    -1,    -1,    -1,    -1,    -1,   560,    -1,    -1,
      -1,   764,    -1,    -1,    -1,    -1,    -1,   425,   426,    -1,
      -1,    -1,    -1,    -1,    -1,   433,    -1,   435,    -1,    -1,
      -1,    -1,    -1,    -1,   442,    -1,    -1,    -1,   446,   447,
     448,   656,    -1,    -1,    -1,    -1,   454,    -1,    -1,   457,
     803,    -1,   805,    -1,   807,   808,    -1,   610,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   535,   819,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     560,    -1,   845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     853,    -1,    -1,   656,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   728,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   877,    -1,    -1,   535,    -1,    -1,
     883,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   753,   754,
     610,    -1,   895,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   560,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   728,    -1,   792,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   656,    -1,   803,    -1,
     805,    -1,   807,   808,    -1,    -1,    -1,    -1,    -1,    -1,
     753,   754,   610,    -1,   819,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   853,   647,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   656,    -1,
     803,    -1,   805,    -1,   807,   808,    -1,    -1,   728,    -1,
      -1,    -1,   877,    -1,    -1,    -1,   819,    -1,   883,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     895,    -1,    -1,   753,   754,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     728,    -1,    -1,    -1,   877,    -1,    -1,    -1,    -1,    -1,
     883,    -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,    -1,
      -1,    -1,   895,    -1,    -1,   753,   754,    -1,    -1,   819,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   845,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   853,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   877,    -1,    -1,
      -1,   819,    -1,   883,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,    -1,
      -1,   839,    -1,    -1,    -1,    -1,    -1,   845,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   853,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   877,
      -1,    -1,    -1,    -1,    -1,   883,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   895
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   898,   899,     0,   900,   901,   902,   905,   906,   373,
     375,   903,   904,   907,   908,   918,   919,   267,   915,   941,
     942,   904,   340,   597,   920,   923,   213,   213,   916,   151,
     943,   944,   924,   921,   547,   932,   932,   795,   795,   181,
    1091,  1093,   689,   388,  1022,  1023,    18,   109,   166,   169,
     171,   190,   282,   445,   533,   537,   644,   681,   727,   729,
     760,   883,   945,   946,   947,   948,   952,   966,   972,   973,
     974,   975,   976,   982,   998,  1000,  1005,  1008,  1013,  1014,
    1016,  1017,  1018,  1019,  1020,  1021,   795,   795,   795,   915,
     915,   213,   917,   799,  1094,   795,   689,   298,  1024,     1,
     883,  1963,  1963,   742,   714,  2112,   398,  2083,  2083,  2083,
    1963,   714,   795,   795,  2083,   795,   795,   107,   152,  2066,
     977,   947,   795,   975,   379,   999,   442,   598,   925,   925,
      35,   933,   934,   249,   909,   910,   911,   243,   913,   795,
     592,  1356,  1360,   689,  1095,   795,   795,  1025,   983,   326,
    1011,  2074,  2083,  2083,   883,  1956,  2008,   132,  1956,  2083,
    2083,   953,   967,  1956,   949,   883,  1006,  1007,  1156,   883,
    1001,  1002,  1003,  1964,   442,   540,   543,   978,   980,   981,
    1731,  2014,  2083,   883,    36,   927,   927,  2083,   192,   936,
     912,   911,   914,   213,  1384,  1385,   795,   295,   684,  1092,
    1096,  1097,  1099,   406,   693,  1026,  1078,  1079,   984,   987,
     988,  2074,  2083,    22,   483,  1956,   442,   379,   539,  2135,
     442,   814,   106,   465,   596,   692,   883,   954,   955,   956,
     957,   958,   962,   963,   965,  2065,  2107,   339,   596,   968,
     969,   970,   950,   965,  1007,  2083,  1002,    30,   398,  1964,
    2085,  1731,   398,   742,  2103,  2083,   166,  1963,   442,   795,
     922,   486,   732,   735,   736,   935,   667,   264,   937,   442,
     598,   926,   926,  1357,     1,     5,     9,    16,    25,    85,
      86,   115,   135,   139,   157,   195,   201,   203,   206,   210,
     227,   263,   279,   289,   334,   342,   345,   350,   376,   384,
     386,   389,   392,   407,   468,   474,   476,   479,   494,   545,
     567,   606,   612,   616,   619,   620,   635,   648,   653,   660,
     666,   685,   699,   705,   723,   737,   747,   748,   755,   758,
     773,   795,   816,   828,   834,   858,   887,   890,  1386,  1387,
    1415,  1420,  1425,  1430,  1452,  1456,  1464,  1468,  1469,  1470,
    1473,  1478,  1483,  1534,  1538,  1540,  1543,  1557,  1561,  1564,
    1567,  1571,  1572,  1579,  1589,  1592,  1595,  1613,  1622,  1626,
    1628,  1632,  1635,  1639,  1652,  1666,  1668,  1672,  1687,  1688,
    1698,  1701,  1702,  1706,  1712,  1713,  1721,  1728,  1745,  1755,
    1764,  1770,  1781,  1785,  1787,  1790,  1793,  1796,  1807,  1826,
    1834,  1859,  1386,   885,  1146,  1148,  1149,     1,   883,  1946,
     795,   514,   546,  2044,    29,   478,   677,  1080,  1081,  1082,
    1083,  1085,  1086,   985,   986,    22,   483,   368,   442,   457,
     610,   728,   805,   895,  1009,  1010,  2018,   882,  1015,  2127,
    2008,   701,   720,  2115,  2083,   795,   956,   795,   883,   955,
     110,   119,   959,  2067,    15,   341,   883,   971,   883,     1,
     795,   970,   951,  2127,    15,   368,   442,   457,   610,   728,
     805,   895,  2019,  2020,  2021,   442,  1004,  2015,  2103,   398,
    1963,  1963,   979,   980,   928,  2083,   473,  2095,  2083,   393,
     939,   795,   795,   472,   740,   745,   797,  1434,     5,     9,
      16,    25,    85,    86,   115,   135,   139,   157,   195,   206,
     210,   229,   234,   235,   236,   237,   239,   240,   241,   242,
     244,   246,   247,   248,   250,   251,   252,   253,   254,   255,
     256,   257,   258,   259,   260,   263,   279,   289,   334,   342,
     345,   350,   376,   384,   386,   389,   392,   468,   474,   476,
     479,   494,   545,   567,   616,   635,   653,   660,   666,   685,
     705,   723,   737,   747,   748,   755,   758,   773,   795,   816,
     828,   834,   887,   890,  2051,  2052,  2053,  1388,  1416,  1421,
    1426,  1431,  1453,  1457,  1465,  1474,  1471,  1479,  1484,  1535,
    1539,  1541,  1544,  1558,  1562,  1565,  1568,   343,   654,  1444,
    1560,  1573,  1580,  1590,  1593,  1596,   342,   565,  1627,  1629,
    1633,  1636,   700,  1640,  1653,  1667,  1669,  1673,  1689,  1699,
    1703,  1707,  1714,  1722,  1729,  1746,  1756,   442,   610,   675,
     728,   780,   895,  1768,  1769,  1914,  2002,  2003,  2008,  1771,
    1782,   590,  1786,  1788,  1373,  1791,  1794,  1797,  1808,  1827,
     342,   565,   689,   137,  1127,   226,   706,   719,  1150,  1151,
    1153,  1164,  1166,  1168,  2036,   795,  1098,   546,  1963,   135,
    1087,   621,   723,   724,  1084,     1,   795,  1082,  2083,  2083,
     379,  1010,  1012,   782,   573,  2083,  2014,   795,  2083,   703,
     395,   927,     1,   341,   395,   927,   795,   795,   189,  2021,
     791,  2015,  1963,   795,   136,   292,   627,   801,   929,   930,
     931,  2083,   116,   745,   797,   938,   668,   795,   105,   854,
    1361,   542,   883,  1389,  1392,  1393,  1394,  1947,  2002,    10,
     149,   155,   156,   164,   170,   329,   330,   331,   341,   425,
     426,   433,   435,   446,   447,   448,   454,   472,   535,   560,
     656,   753,   754,   768,   819,   845,   853,   877,  1417,  1938,
    1968,  1969,  1971,  1972,  2002,  2019,  2023,  2024,  2025,  2026,
     803,   807,   808,  1422,  1933,  1934,  1935,  1936,  1937,  1938,
    1972,  1975,  2002,  2020,  2023,   442,  1427,  1428,  1952,  1953,
    1954,  2008,  1432,  1434,   442,   598,  1454,  1455,  1988,  2002,
     881,  1458,  1459,  1461,  1946,    10,  1466,  1938,  1939,  1940,
    1966,  2005,  2006,  2008,  2020,   799,  1475,  1946,    15,  1472,
    2002,  2004,   387,   405,   553,   772,  1480,  1482,   274,   323,
     380,   442,   469,   542,   732,   756,   801,   881,  1485,  1486,
    1487,  1488,  1490,  1497,  1499,  1500,  1501,  1504,  1509,  1512,
    1513,  1516,  1518,  1968,  1988,  2002,  1536,  1969,  1480,  1434,
    1542,   798,   809,  1545,  1546,  1547,  1918,  1919,  1920,   339,
     563,   567,   596,   689,  1559,  1563,  1965,  1966,  1566,  2008,
     790,  1569,  2123,  1969,     1,  1917,  1918,  1581,  1965,   883,
    1591,  1948,   158,   881,  1300,  1521,  1594,  2002,  1597,  1598,
    2002,  2019,  2023,  1614,  1623,  1747,  1997,  1998,  2008,  1300,
    1521,  1630,   164,  1634,  1969,  1637,  1969,   288,  1641,  1642,
    1643,   379,  1654,  1912,  2076,   883,  1947,   284,  1670,  2002,
    1674,  1946,  1690,  1947,  1700,  1941,  2008,  1704,  1946,   799,
    1708,  1941,  1942,    15,  1715,  1943,  2008,  1723,  1947,   267,
     416,   472,  1730,  1733,  1734,  1737,  1738,  1739,  1740,  1741,
    1742,  1743,  1744,  1914,  1949,  1950,  1965,  1996,  1998,  2008,
    1747,  1757,  1946,  1765,  2002,   807,  2012,  2013,  1772,  1773,
    1774,   164,   768,  1783,  1968,  1789,  1948,   795,   883,  1374,
    1375,  1378,  1379,  1384,  1792,  1999,  2002,  1795,  1946,   442,
    1798,  1989,  2002,  2023,  1809,  2002,  1828,  1942,  1835,  1860,
     795,   689,   449,  1225,  1156,  1156,  1156,  1151,   795,     1,
     305,  1154,  1155,  1156,   429,  1100,  1027,   543,  2101,   799,
    2073,    31,  2057,   795,    38,   222,   368,   442,   445,   457,
     486,   610,   728,   733,   734,   895,   989,   991,   992,   993,
     996,   823,   855,   856,   990,   991,   883,  1051,  2018,   442,
    2014,   445,   762,   852,   964,  1959,  2009,  2010,   960,   473,
    2021,   931,  2107,   136,    47,   488,   489,   490,   601,   811,
     812,   820,  2047,  2083,  1363,  1362,   654,  1370,   234,  1414,
    1390,    40,    54,   336,   473,  1398,  1399,  1400,  1404,  1407,
    2060,  2061,  2127,   807,  1395,   469,  2094,   335,   539,  2100,
    2002,   807,   807,   807,  2028,  2100,   807,  2135,  2135,   807,
     807,   807,   807,  2135,  1997,   807,  2028,   235,  1419,   790,
    1418,  1969,  2003,  2020,  2023,   807,  2027,   807,   807,  1937,
    2002,  1933,  1937,   107,   803,   808,   794,   804,   290,  2003,
    2020,  2023,   385,  2039,  1428,   790,  2135,   236,  1451,  1912,
    1455,  1462,  1946,   629,   827,  1460,  2127,  2141,  2100,   237,
    1467,   273,   796,  1940,  2133,   667,  2045,  2012,  2013,  1476,
    1946,   239,  1477,   621,  2109,   159,  2069,  2002,   772,  2120,
     772,  1947,  1498,   351,  1519,    77,  2063,   240,  1533,   284,
     525,  1870,  1872,  1874,  1488,  1967,  1968,  1489,   882,  1491,
    1395,  1510,  1519,   840,   841,   842,   843,   241,  1537,    82,
     394,   442,   242,  1556,    24,   876,  1548,  1549,  1550,  1552,
      27,   290,   398,   514,   548,   792,   794,   803,   804,   807,
     808,  1921,  1922,  1924,  1969,  2083,   179,  1560,  1966,  1951,
     779,  1575,  1582,  2127,  1948,  2002,     8,    14,    44,    65,
      66,    67,    68,    69,    70,    71,    72,    80,    84,    95,
      96,    97,    98,   112,   122,   125,   126,   127,   129,   163,
     171,   172,   173,   174,   175,   176,   177,   178,   182,   183,
     207,   208,   211,   212,   216,   238,   262,   266,   281,   286,
     287,   300,   301,   302,   303,   304,   307,   346,   347,   348,
     349,   355,   356,   358,   360,   361,   362,   365,   367,   371,
     390,   391,   399,   400,   401,   402,   403,   404,   413,   417,
     418,   421,   456,   460,   461,   462,   463,   464,   471,   487,
     495,   529,   530,   561,   564,   575,   581,   585,   586,   587,
     600,   603,   604,   608,   622,   623,   624,   632,   633,   649,
     650,   651,   669,   670,   671,   672,   674,   678,   679,   686,
     687,   694,   695,   696,   702,   707,   725,   738,   739,   744,
     766,   767,   775,   783,   789,   815,   821,   861,   867,   869,
     872,   879,   889,   892,  1305,  1307,  1309,  1311,  1313,  1315,
    1317,  1319,  1322,  1324,  1326,  1327,  1329,  1331,  1332,  1334,
    1336,  1339,  1340,   419,   720,   788,  1341,  1342,   161,   640,
     769,  1599,  1600,  1602,  1603,  1615,  2002,  1624,  2002,  1749,
     807,  2012,   514,   749,  1302,  1303,  1304,  1305,  2043,  1341,
     246,  1631,  1969,   790,   247,  1638,    82,  1642,   291,   387,
     405,   553,  1644,    83,   328,   425,   442,  1158,  1659,  1660,
    1661,  1952,  1990,  2002,  2008,  2019,  2023,  2127,   780,   883,
    1671,   250,  1686,   494,   584,  2042,   251,  1697,   469,   691,
    1691,   335,  1829,   252,  1705,  2109,   883,   253,  1711,  1829,
    1943,   254,  1720,   864,  1716,   335,  1724,  1725,  1982,  1986,
    2002,  2020,  2023,   284,  1740,  1742,  1965,   591,   790,  1950,
     219,   790,   836,  1732,    41,  2012,   255,  1763,   308,   410,
     416,  1759,  1444,  1766,  1969,  2127,  1931,  1933,   807,  2013,
     256,  1780,   394,  1775,  1776,  1969,  2002,  1997,   257,  1784,
     335,  1948,   689,   795,   795,   335,   621,   626,  2110,   258,
    1806,   196,  1799,  2002,   259,  1833,  1829,  1836,  2002,  1861,
    2002,  1147,   795,   689,   440,  1227,  1167,  1169,   638,   795,
     795,  1152,   152,    75,   118,   181,   412,   432,   621,   625,
     641,   643,   795,   860,  1101,  1102,  1104,  1108,  1109,  1112,
    1113,  1119,  1122,  1124,  1125,  2083,     1,  1028,  1029,  1955,
    1956,   771,  2119,  2074,  1959,   993,    24,   782,   107,   475,
     884,  2139,   883,  2011,   961,  2074,  2083,   488,   489,   601,
     820,   940,    82,  1364,  1365,  1366,  2064,  1364,   542,   883,
     795,  1398,   895,  1991,  1996,  2014,  2083,  1400,   120,   124,
     434,   579,   580,  1405,  1406,  2132,   785,    43,    48,    49,
      50,    51,    55,    74,   121,   158,   160,   192,   327,   337,
     366,   423,   452,   455,   498,   505,   540,   556,   602,   605,
     645,   655,   657,   658,   677,   682,   690,   732,   765,   786,
     825,   837,   844,  1408,  1411,  1412,  1413,  2070,  2108,   134,
     442,  1396,  1397,  1977,  2002,  2002,   165,    33,    34,   123,
     130,   133,   184,   186,   187,   267,   269,   276,   284,   387,
     434,   438,   680,   763,   774,   785,   851,   883,  1403,  1950,
    2130,   263,   296,   297,   596,  1980,  2003,   790,  1931,  1933,
    2034,  1931,  2035,   792,  1931,  2030,  2031,   883,   883,  1933,
    2033,  2033,  2033,  1974,  2002,  2020,  2023,  2032,   883,   790,
    1974,  2029,    10,  1938,  1939,  1969,  2008,  2020,   343,  1933,
    1974,  1931,   792,   385,  2040,  1934,  1934,  1935,  1935,  1935,
     444,  1423,   594,  1429,  1954,  1435,  1436,  1989,  2002,  1460,
     450,   498,  2074,  2003,  1933,   473,  2046,  2013,  1946,   652,
    1679,  1680,  1681,  1481,  2127,  1491,   881,  1968,  1381,  1382,
    1381,  1873,  1874,  1871,  1872,   839,  1404,  1407,  1493,  1494,
    1495,  2127,    48,    49,    50,    51,    55,    73,    74,   121,
     160,   274,   327,   366,   455,   556,   677,   682,   720,   732,
     825,  1412,  1492,  1532,   274,  1502,  1503,  2002,  2020,   839,
    1520,   881,  1870,  1870,  1870,  1870,  1969,  1939,  1969,   854,
    1438,  1547,     1,    28,   798,   809,  1553,  1554,  1919,   876,
    1550,  1551,   242,   795,   876,  1381,  1923,  1924,  1922,    19,
      20,    21,   111,   273,   352,   353,   427,   428,   491,   524,
     533,   542,   582,   796,   800,   802,  1925,  1926,  1927,  1928,
    1929,  1930,   198,  1570,  1952,   229,  1574,  1576,    15,    19,
      22,    23,   483,   484,   533,   534,  1583,  1587,   305,   379,
     796,  2077,  1338,  2014,  2077,  1340,  2077,  2077,  2077,  1342,
    1984,  2003,  2007,  2020,    15,   107,   308,   420,   814,  1606,
    1607,  1608,  1601,  1602,   245,  1621,   335,  1621,   394,  1751,
    2101,  2127,   398,   796,  2084,  1303,    30,   398,   796,  2086,
       3,    11,    26,    45,    64,    76,    77,    78,    81,    87,
      99,   101,   102,   128,   180,   193,   214,   215,   217,   218,
     261,   277,   311,   312,   313,   333,   338,   363,   364,   369,
     370,   422,   424,   439,   451,   452,   453,   477,   500,   501,
     502,   504,   506,   507,   509,   510,   511,   515,   517,   518,
     519,   520,   533,   541,   554,   555,   562,   568,   613,   618,
     663,   664,   665,   673,   697,   710,   711,   712,   713,   730,
     731,   741,   770,   784,   785,   817,   826,   833,   844,   848,
     849,   850,   865,   866,   870,   871,   873,   880,  1306,  1308,
    1310,  1312,  1314,  1316,  1318,  1320,  1321,  1323,  1325,  1328,
    1330,  1333,  1335,   790,  1965,  1939,  1969,   709,  1645,  2100,
    2100,   357,  1913,  1913,   835,   864,   782,   787,   776,  2109,
     394,  1995,  2002,  2019,  2023,   394,  1675,  1679,   233,  1717,
    2002,  1717,  2002,  1726,  2127,   790,   790,   790,   790,  1731,
     263,   720,   798,   809,  1969,    82,    55,    74,   366,   423,
     455,   556,   657,   825,  1735,  1736,  2083,  1758,  2127,  1969,
     275,   513,   134,   698,   792,  1932,   793,  1933,  2002,  1776,
     196,  1777,   335,   335,  1939,  1969,  1376,  1982,  2064,   394,
    1802,    13,    54,  1830,  1831,   260,  1858,   335,  1858,   595,
    1148,  1128,   795,   689,   641,  1229,   860,  1217,  1180,  1181,
    2083,  2008,    28,    53,    56,    57,    58,    59,    60,    62,
      63,    73,   138,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   206,   314,   315,   316,   317,   318,   321,   322,
     357,   381,   408,   483,   538,   557,   572,   577,   599,   628,
     714,   716,   717,   718,   761,   830,   831,   832,   846,   868,
    1174,  1175,  1176,  1177,  1178,  1181,  1182,  1183,  1187,  1188,
    1189,  1192,  1195,  1212,  1213,  1215,  1216,  1217,  1222,  1223,
    1224,  2083,  2114,  1157,  2083,   153,  2068,  2083,   621,   626,
    2140,  2140,  2083,  2068,  2083,  2095,  2083,    30,  2056,   539,
    1126,  1963,   292,   344,   795,     6,    26,    39,   382,   434,
     550,   558,   621,   634,   646,   709,   723,   795,   799,  1030,
    1031,  1039,  1041,  1046,  1047,  1050,  1052,  1053,  1054,  1055,
    1061,  1062,  1063,  1064,  1067,  1073,  1074,  1076,  2067,  2109,
    1956,  2068,  1944,  1946,   994,   996,  2135,    22,   483,  1959,
    1365,   720,   829,  1367,  1368,   630,   860,  1358,   276,   284,
     523,   525,  1863,  1865,  1866,  1868,  1869,    75,   531,  2097,
    2097,   442,  1992,  1996,  2016,  2083,  2083,  2083,    55,   224,
    1413,   106,   219,   836,    13,  2055,   410,   720,  1396,   134,
     803,   808,  1863,   894,   893,  1982,  1863,   410,  2088,   742,
     742,   531,  1391,   480,  2100,  2100,   442,  1981,  2003,  2002,
    1932,   792,  1932,   792,   792,   571,   792,  1974,  1932,   792,
     792,   792,  1932,   792,  1997,  1932,   792,  2100,   528,   721,
    1885,  1887,  1889,  2012,  2013,  1939,   793,   792,   792,   790,
    2041,  1423,  2014,   654,  1424,   790,  1952,  1433,   492,   598,
    1437,    36,  1463,  2127,   659,   637,  1885,  2083,   328,  2074,
     397,   526,  1906,  1907,  1909,  1911,   410,  1514,  1505,  1383,
     166,   167,   588,   883,  1496,  1950,  1494,   499,  1532,  2084,
     434,   680,  2083,   270,   272,  1343,  1344,  2072,  2123,  2084,
     219,   836,  2083,  1532,  1503,  2002,  1511,  1517,   343,  1885,
     343,  1439,    24,   782,  1555,   551,  1553,   895,  1925,  2123,
    2101,  1577,   244,  1578,  1381,  2123,   640,  1584,  2123,  2002,
    2002,  2002,  2002,  2002,   790,    82,  1607,  1609,  1984,    15,
     107,   420,   814,  1604,  1605,  1982,  2000,  2002,  2002,  2002,
    1047,  1752,  2067,    37,   199,  1207,   220,  1337,  2002,  2014,
    1968,  1965,  1885,   343,  2127,  1679,     1,    57,    58,    59,
      60,    62,   141,   314,   315,   316,   317,   318,   319,   320,
     322,   442,   577,   599,  1159,  1160,  1161,  1162,  1163,  1189,
    1978,  2003,  1159,  2076,  1660,  1655,  1656,   289,  1662,  1917,
    1663,  1664,  2002,  1952,  1831,  1675,  2002,  2002,   233,   521,
    1895,  1898,  1900,  1709,  1710,  2127,  1381,   876,   876,  1718,
    1719,  1830,   225,   230,   278,  2002,  1982,   540,   895,  1993,
    1994,  1996,  2014,  1981,   539,  1969,  1731,  1731,  1731,  1731,
    1731,  1731,  1731,  1731,  1736,   514,   524,  1760,  1761,  1762,
    1926,  2043,  1906,   425,   720,  2142,   742,  2117,  2117,  1933,
     792,  1933,  1779,  2127,  2064,  2002,  1997,  1885,   343,  1380,
    2014,   790,    15,  1800,  1801,  2037,  1803,  2002,  1779,  1803,
    1679,    12,  2054,  2002,   592,  1129,  1226,   795,   689,   680,
    1287,  2085,   860,   344,   782,  1165,   425,   533,   715,   829,
    2113,   829,  2113,   829,  2113,   829,  2113,   829,  2113,   876,
    2125,  2100,   663,  2111,   221,  1198,  2014,   445,  1184,  2003,
     398,   422,   663,  1214,  2083,  1176,   292,   293,   374,   420,
     814,    36,   335,  1170,  2059,   344,  2014,  1051,  2085,  2085,
     883,  1960,  1961,   542,   732,  2136,   442,  1956,  1962,  2014,
     864,  2083,   299,   373,   883,  1110,  1963,  2095,  2109,   790,
     854,  2124,   704,  2083,  2065,   197,  2088,  2088,   498,  1075,
    2014,  2127,   742,   450,   703,    56,  2062,  1088,  1089,  1946,
    1946,   995,   996,  2011,  2083,  2083,  1959,  2083,   720,  1368,
     546,  1369,   191,  1371,  1867,  1868,  1409,  1410,  2000,  2002,
    1864,  1865,  1381,  1991,  1991,  1991,  1991,  1991,  2083,  1915,
    1994,  1915,  1992,  2076,  2083,   792,   792,  1396,  1977,  1977,
    1863,   720,  1401,  1402,  1404,  1946,  1946,  2045,   764,  2027,
     764,  2027,   792,  1956,  2027,  2027,  2027,  1974,  2045,   420,
     814,  2027,  2003,  1381,  1381,  1888,  1889,  1886,  1887,  2013,
    1885,   792,  1933,  2027,  2027,  1987,  2002,  2019,  1424,  1966,
    1913,   498,  2047,  1933,  1381,  1381,  1910,  1911,  1908,  1909,
    1988,  1520,   192,   372,   788,   821,  1444,  1506,  1507,  1508,
    1384,  1991,  1991,   434,   680,   233,  1991,  1915,  1915,  1991,
      76,    78,   357,   438,   578,   708,   810,  1404,  1522,  1523,
    1524,  1525,  1526,  1528,  1529,  1530,  1531,  2127,  1522,  1939,
    1940,  1939,  1940,  1440,  1441,  1442,  2064,  1554,  1918,   242,
     795,  1381,  2002,  1381,   229,   860,  1585,  1586,  1587,   779,
    1588,  2121,   860,  1985,  2007,  2019,  1982,    82,    13,    54,
    1610,  1611,  1612,  1605,  1610,   326,   165,  1916,  1625,  2127,
    1748,   703,  2088,   379,   549,  2078,  1939,    15,   498,   616,
    1077,  1944,   795,  2002,  1381,   248,   795,  1658,    13,   335,
      12,   378,  1676,  1677,  1678,  1680,  1683,  1710,  2127,   181,
     503,  1692,  1694,  1696,  1381,  1381,  1899,  1900,  1898,  1906,
     450,   498,  1918,  1917,  1718,   640,  1727,  1969,  1926,  2002,
    1927,  1928,  1929,  1930,  1933,  1767,  1969,  1767,   792,   527,
     806,  1890,  1892,  1894,   577,   720,  1778,  1969,  2045,  2045,
    1939,   795,  1983,  1987,   548,  1982,   197,  1804,   769,  1805,
    1709,   559,  1950,  1991,  1916,  2083,    93,  1130,  1131,  1148,
    1228,   795,   689,    27,   290,   548,   792,   794,   803,   804,
     807,   808,  1158,  1173,  1219,  1220,  2083,  2008,   895,     7,
      52,    64,   108,   131,   185,   265,   324,   332,   412,   419,
     441,   467,   607,   611,   683,   743,   756,   780,   818,   863,
     875,   881,   883,  1298,    88,  1200,   824,   790,  1196,  1185,
    2083,     1,   883,  1188,    36,  1179,  2064,   701,  2048,  2048,
     883,  1219,   790,  1107,   326,  1123,  1961,   438,  2092,   790,
    2066,  2076,   294,   309,   676,   822,   857,   862,  1120,  1121,
    2083,  2083,  2083,  2088,   221,   292,  1036,   634,  1062,  2083,
    2083,  2083,  2083,    31,    32,  2058,  1077,  2083,  2095,   539,
    1048,   704,  1089,   580,  1090,    24,  1959,  1959,    43,   192,
    2014,  2083,   883,   795,  1359,  1381,  1988,   434,   438,  2137,
    1410,  1993,   792,  2083,  1402,  1885,   335,  2049,   792,  1438,
     206,   688,   787,  1515,  2084,  2084,  2084,  2084,  1969,  1508,
    2100,   379,   398,  2087,  1991,  2057,  1523,   100,   422,   663,
    1527,   498,  1532,  1885,   636,  1885,   636,  1441,   542,  1367,
     154,   630,   860,  1577,  1586,   181,  2071,  2123,  1610,  1610,
    1985,   801,  2081,  2081,  1612,  1611,  2076,  1616,   480,  1845,
     202,   387,   854,  1753,  1750,   549,  1885,   551,  2104,  2104,
     544,    17,   326,   658,   882,  1646,  1647,  1651,  2127,  2128,
     248,  1657,  1664,  1969,  2101,   450,   410,  1684,  1682,  1683,
    2127,   377,   409,   874,  1381,  1381,  1695,  1696,  1693,  1694,
     450,  1381,  1381,   434,  2089,  1381,  1381,  1893,  1894,  1891,
    1892,  2083,  1885,  2049,  1885,   847,  1377,  1810,  1801,  2076,
    1916,  2076,  1890,   271,   522,  1832,  1901,  1903,  1905,  1907,
     434,   438,  2090,  1837,  1659,  1963,  1148,  1148,  1230,   795,
     782,  1218,  1220,   494,   737,  1158,  1171,  1172,  1173,   194,
     310,   414,   466,   722,   813,  1190,   443,  1191,  2076,   335,
    1197,   787,  2122,  2014,  2122,   720,   883,  1186,   442,  1988,
    2065,  2014,   107,   626,  1103,  1955,  1114,  2014,  2115,   442,
    1111,  2008,  1111,   221,   614,   704,  1040,  2083,    89,    90,
      91,   204,   205,   206,   387,   388,   411,   434,   458,   553,
     585,   588,   589,   614,   771,  1032,  1033,  1034,  1035,  1962,
    1065,  1956,  1956,  1956,  2083,  1956,  1049,  2074,  2083,  2014,
     996,  2074,  2074,    43,  1372,  1373,  1993,   381,  2079,   200,
    2050,  1443,  1444,  1522,  1969,  1969,  1969,  1969,  2002,  2084,
     788,   682,   886,  1940,  1940,  1970,  1971,  2001,  2003,  1381,
      82,   192,  1610,  1969,  1969,  2002,  1845,  2100,   759,  1618,
     592,  1944,   343,   553,  1754,  2008,    15,   498,   617,   838,
     888,  1649,  1650,  1651,   498,    79,   450,   460,  1648,    82,
    1665,   450,  2083,  1685,  1896,  1898,  1900,  1906,   450,   450,
    2002,  2050,  1811,   795,  2002,  2002,  1381,  1381,  1904,  1905,
    1902,  1903,  1839,  1840,  1841,  2127,  1862,  1132,  1231,  1288,
    1158,   876,  1221,  2126,  2100,  1173,   883,  2014,   790,  1196,
     198,   198,  1199,  2083,    76,   810,  1115,  1116,  1117,  1118,
    2127,  2066,   335,  1106,  2075,  1956,   442,  1038,  2008,  1038,
      12,  1038,  1038,  1038,   442,  1037,  2008,    61,   437,   733,
     883,  1066,   726,   796,  1068,  1069,  1956,  1957,    46,   288,
     459,  1056,  2083,    22,   483,  1051,   483,    22,  1373,  1993,
    2079,  1885,   284,   525,   806,  1446,  1448,  1450,    10,   394,
     516,   542,   805,  1445,  2082,  2002,  2084,  1885,  1885,  1969,
    1618,  1846,  1847,  2002,  1619,  1620,  2002,   284,   525,  1880,
    1882,  1884,  2083,  1944,   592,   552,  2105,   659,   442,  1972,
    1976,  1979,  2002,  2023,   835,  2002,  1899,  1897,  1898,  1885,
      40,   223,   285,   344,  1812,  1813,  1814,  1816,  1820,  1822,
    1823,  1824,  1825,  2060,  2074,   481,  1843,  1841,    42,   232,
     891,  1842,   284,   525,  1875,  1877,  1879,  1133,   615,  1232,
    1153,  1168,  1289,  1290,  1291,   705,   798,  2002,   385,  1201,
    2101,  2101,   382,  1202,  1204,  1205,  1206,  1207,  1209,  2014,
    1962,  1962,   325,  1107,  2014,  1068,  2073,  2083,   566,  1042,
    1043,  1070,  1071,   882,  1057,  1059,  2127,  1060,  2127,  1057,
    1051,  2083,  2083,  1051,  2083,  2083,   233,   790,  1993,  1381,
    1381,  1381,  1449,  1450,  1447,  1448,  2100,  2002,  1969,  1617,
    1847,  2083,  1620,  1381,  1381,  1883,  1884,  1881,  1882,  1659,
    2083,  2003,  2020,  2023,  1917,    54,  2055,  1824,  1671,   596,
     189,  2083,  1845,  1986,  1381,  1381,  1878,  1879,  1876,  1877,
     795,  1134,  2074,  1948,     1,  1155,  1291,   795,   790,  2083,
    1202,  1956,  1956,  2064,  1209,  1205,  2088,  1203,  2060,  2066,
    2038,  2127,  1044,  2038,  1072,  1956,  1072,   450,  2093,   666,
     460,  1051,  1051,  1959,  1959,   191,  1993,  2002,  1880,  2019,
    1659,   642,   732,  2116,   799,  1945,   233,   737,  1821,  2101,
    1986,   821,  1848,   553,  2081,  1233,   795,  1292,  1293,   795,
    1158,  1202,  1210,  1211,  2009,  2083,  1204,  1962,   198,  1105,
    1042,   220,   498,  2083,   543,  2074,  2074,   795,  2002,   275,
     284,  2134,  1946,   799,    15,  1817,  1818,  2003,   482,  1844,
    2100,   759,  1853,   200,   275,   742,   760,   777,  1139,  1140,
    1141,   387,   405,     1,  1234,  1295,   536,  1297,  1298,  1211,
    1958,  1959,  2101,   758,  1045,   220,  1956,   478,   621,   626,
    2138,   483,    22,   592,  2106,  1946,   539,   593,   631,  1819,
    1818,  2083,  1849,  1850,  2002,   283,   878,  1854,  1855,  2002,
    1838,   165,   768,  2083,   410,   410,   200,   425,  1141,   233,
     469,   742,   760,   777,  1135,  1136,  1137,  1138,  2002,  2094,
    2118,   233,   469,   742,   777,  1142,  1143,  1144,  1145,  2002,
    2118,   795,   158,   159,   559,   795,  1235,  1236,  1241,  2083,
    2127,  2150,    43,    48,    49,    50,    51,    55,    73,    74,
      92,   113,   121,   168,   274,   327,   335,   337,   354,   366,
     423,   434,   438,   455,   498,   538,   556,   602,   645,   690,
     720,   732,   765,   790,   801,   825,   854,  1183,  1187,  1192,
    1212,  1215,  1217,  1296,  1354,  1355,  1411,  1412,  2083,  2132,
    1299,  2002,  2014,  1294,  1959,  1956,   876,  2138,  1058,  1059,
    2083,  2083,  2101,   539,  2003,  1986,  1850,  2083,    41,   154,
     228,   512,   533,  1852,  1856,   368,   457,   728,   895,  1857,
    2022,  1855,  1875,  2083,   538,  2002,  2083,  2083,  2083,  2083,
     410,   184,   785,   410,   425,  1137,  2002,   165,   609,   726,
     750,   751,   752,   410,   184,   785,   410,   425,  1144,  2002,
     772,  2085,  2085,   430,   431,  2091,  1252,   344,   117,   306,
    1237,  1238,  1239,  1240,  2002,  2083,   434,   680,  1352,  2097,
    1350,  2097,  2083,  2084,  1343,  1344,  2083,  1995,  1350,  2084,
    2014,   106,  2084,  2002,  2002,   344,  1352,  1301,  1302,    15,
     728,   895,   997,  1058,  1051,  1051,   291,   387,   405,   553,
    1815,  1944,  1852,  1851,  1852,  1851,   548,  2002,  2014,  2002,
    2002,  2002,  2002,  2083,  2083,  2083,  2083,  2083,  2002,  2083,
    2083,  2083,  2083,  2083,  2083,  2083,  2083,  2083,  2083,  2083,
    2002,  2083,  2085,  2085,   442,   895,  1242,  1243,  1244,  2002,
    2017,  1153,  1253,  2083,  1239,  1240,  1991,  2083,  2083,  1991,
    2000,  2014,  1991,  1347,  1991,  2122,  2083,  2000,  2014,  1295,
    2021,  2022,  2122,  2002,  2002,  2002,  2002,  2002,  2002,  2002,
    2002,  2002,  2002,  2002,  2002,  2002,  2002,  2002,  2002,  2002,
    2002,  2002,  1245,   434,   438,  2090,  2130,  2137,     1,  1155,
    1156,  1988,   470,   576,   803,   808,  1345,  1346,  1353,  1345,
    1346,  1351,    94,   574,  1348,  1349,  1988,  1208,  1209,  2002,
    2002,   308,   325,   359,   416,   436,  1246,  1247,  1248,  1249,
    1250,  1251,  1243,  1244,   795,  1254,  1991,  1991,  2002,  2002,
     188,   202,  2143,  2083,  2083,   104,   158,  2143,  2144,  2083,
    1255,  2002,  2083,  1244,  1244,   359,  2083,  2083,  1244,     4,
     120,   124,   355,   434,   496,   538,   583,   726,   757,   795,
     821,   864,  1183,  1187,  1192,  1193,  1212,  1215,  1217,  1256,
    1257,  1262,  1265,  1268,  1269,  1272,  1273,  1274,  1278,  1279,
    1285,  1286,  2129,  2130,  2131,  2002,  1244,  1244,  1244,   383,
    2080,   531,  2098,  2083,  2014,  2083,  2100,  2083,  2002,    13,
      54,   408,   876,   497,   576,   808,  1275,  1276,  1277,  2017,
    1282,  1283,  1284,  1345,  2017,   531,   532,  2099,  2002,   398,
     494,   497,   559,   576,   808,  1263,  1264,  2014,  1196,  1975,
    1973,  1975,   103,   158,   559,   569,   570,   641,   661,   662,
    1258,  2143,  2144,  2145,  2146,  2147,  2148,  2149,   335,   493,
    2096,  2096,    13,    54,  1917,  2017,  2017,  1275,  1283,  2017,
      99,   422,   663,  1280,  1281,  2002,  2014,  2014,  2122,  2045,
     647,   839,  1266,  1975,   325,   325,   359,   325,   359,   326,
     543,  2102,  2102,  1975,   548,   559,  1270,  1271,  2002,  1270,
    2096,  2096,  2085,  2002,  1199,  2101,  2002,   306,  1259,  2002,
      15,   306,  1261,  2002,    82,  1270,   559,   559,   746,  1194,
     306,  1267,  2002,   548,  1260,  1260,  1260,  1260,  1975,  2014,
     559
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   897,   899,   898,   900,   900,   902,   901,   903,   903,
     904,   904,   906,   905,   907,   908,   909,   909,   910,   910,
     912,   911,   914,   913,   916,   917,   915,   918,   918,   919,
     919,   921,   922,   920,   924,   923,   925,   925,   926,   926,
     927,   927,   928,   928,   929,   929,   929,   929,   930,   930,
     931,   931,   932,   932,   933,   934,   934,   935,   935,   935,
     935,   936,   936,   937,   937,   938,   938,   938,   939,   939,
     940,   940,   940,   940,   941,   942,   942,   943,   944,   944,
     945,   945,   946,   946,   947,   947,   947,   947,   947,   949,
     948,   950,   950,   951,   951,   953,   952,   954,   954,   954,
     954,   955,   955,   956,   956,   956,   956,   957,   958,   960,
     959,   961,   961,   961,   961,   961,   961,   962,   963,   964,
     964,   964,   964,   965,   965,   967,   966,   968,   968,   968,
     969,   969,   970,   970,   970,   970,   970,   971,   971,   972,
     973,   974,   974,   975,   975,   975,   975,   975,   975,   975,
     975,   975,   975,   975,   975,   975,   977,   976,   978,   978,
     978,   978,   979,   979,   980,   981,   981,   983,   982,   985,
     984,   986,   984,   987,   987,   988,   989,   989,   989,   989,
     989,   990,   990,   990,   990,   991,   991,   991,   992,   992,
     993,   993,   994,   993,   995,   995,   996,   996,   996,   996,
     996,   996,   997,   997,   998,   999,   999,  1000,  1001,  1001,
    1002,  1003,  1003,  1004,  1004,  1005,  1006,  1006,  1007,  1008,
    1009,  1009,  1010,  1010,  1011,  1011,  1011,  1012,  1012,  1013,
    1014,  1015,  1015,  1016,  1017,  1018,  1019,  1020,  1021,  1022,
    1023,  1023,  1024,  1024,  1025,  1025,  1027,  1026,  1028,  1028,
    1029,  1029,  1030,  1030,  1030,  1030,  1030,  1030,  1030,  1030,
    1030,  1030,  1030,  1030,  1030,  1030,  1031,  1031,  1031,  1031,
    1031,  1031,  1032,  1032,  1032,  1033,  1033,  1033,  1033,  1034,
    1034,  1034,  1034,  1034,  1034,  1034,  1035,  1035,  1036,  1036,
    1036,  1037,  1037,  1038,  1038,  1038,  1039,  1040,  1040,  1040,
    1041,  1042,  1042,  1044,  1043,  1045,  1045,  1045,  1046,  1048,
    1047,  1049,  1049,  1049,  1049,  1049,  1049,  1050,  1051,  1052,
    1053,  1053,  1053,  1055,  1054,  1056,  1056,  1056,  1057,  1057,
    1057,  1057,  1058,  1058,  1059,  1060,  1060,  1061,  1061,  1062,
    1062,  1062,  1062,  1063,  1065,  1064,  1066,  1066,  1066,  1066,
    1067,  1068,  1068,  1069,  1069,  1071,  1070,  1070,  1072,  1073,
    1074,  1075,  1075,  1076,  1077,  1077,  1077,  1078,  1078,  1079,
    1080,  1080,  1080,  1081,  1081,  1082,  1082,  1082,  1083,  1084,
    1084,  1084,  1084,  1085,  1087,  1086,  1088,  1088,  1089,  1090,
    1090,  1092,  1091,  1093,  1093,  1094,  1094,  1095,  1095,  1096,
    1098,  1097,  1097,  1099,  1099,  1100,  1100,  1101,  1101,  1101,
    1101,  1101,  1101,  1101,  1101,  1101,  1101,  1101,  1102,  1103,
    1103,  1103,  1104,  1104,  1104,  1105,  1105,  1106,  1106,  1107,
    1107,  1108,  1109,  1109,  1110,  1110,  1111,  1111,  1112,  1113,
    1114,  1114,  1115,  1115,  1115,  1116,  1117,  1118,  1119,  1120,
    1120,  1120,  1120,  1120,  1121,  1121,  1122,  1123,  1123,  1124,
    1125,  1125,  1126,  1126,  1127,  1128,  1127,  1129,  1129,  1130,
    1132,  1131,  1133,  1133,  1134,  1134,  1134,  1135,  1135,  1135,
    1136,  1136,  1137,  1137,  1137,  1137,  1137,  1137,  1137,  1137,
    1137,  1137,  1137,  1138,  1139,  1139,  1140,  1140,  1141,  1141,
    1141,  1141,  1141,  1141,  1141,  1142,  1142,  1142,  1143,  1143,
    1144,  1144,  1144,  1144,  1144,  1144,  1145,  1146,  1147,  1146,
    1148,  1149,  1148,  1150,  1150,  1151,  1151,  1151,  1152,  1151,
    1151,  1153,  1154,  1154,  1155,  1155,  1156,  1157,  1157,  1158,
    1158,  1158,  1159,  1159,  1159,  1159,  1159,  1159,  1159,  1159,
    1159,  1159,  1159,  1159,  1159,  1159,  1159,  1160,  1160,  1161,
    1161,  1162,  1162,  1162,  1163,  1163,  1164,  1165,  1165,  1167,
    1166,  1168,  1169,  1168,  1170,  1170,  1171,  1171,  1171,  1172,
    1172,  1173,  1173,  1173,  1173,  1173,  1173,  1173,  1173,  1173,
    1173,  1174,  1174,  1175,  1175,  1176,  1176,  1176,  1176,  1176,
    1176,  1176,  1176,  1176,  1176,  1176,  1176,  1176,  1176,  1176,
    1176,  1177,  1178,  1179,  1179,  1180,  1180,  1181,  1182,  1183,
    1184,  1184,  1185,  1185,  1186,  1187,  1187,  1187,  1187,  1188,
    1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,
    1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,
    1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,
    1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,
    1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,  1188,  1189,
    1189,  1190,  1190,  1190,  1190,  1190,  1190,  1190,  1191,  1191,
    1192,  1192,  1193,  1194,  1194,  1195,  1195,  1195,  1196,  1196,
    1197,  1197,  1198,  1198,  1199,  1199,  1200,  1200,  1201,  1201,
    1202,  1202,  1203,  1202,  1202,  1202,  1204,  1205,  1205,  1206,
    1207,  1207,  1208,  1208,  1209,  1210,  1210,  1211,  1212,  1213,
    1214,  1214,  1214,  1215,  1216,  1218,  1217,  1219,  1219,  1220,
    1220,  1221,  1221,  1222,  1222,  1223,  1224,  1225,  1226,  1225,
    1227,  1228,  1227,  1229,  1230,  1229,  1231,  1231,  1233,  1232,
    1234,  1234,  1234,  1235,  1235,  1235,  1235,  1236,  1237,  1237,
    1237,  1238,  1239,  1239,  1240,  1241,  1242,  1242,  1242,  1243,
    1244,  1244,  1245,  1245,  1246,  1246,  1246,  1246,  1246,  1246,
    1247,  1248,  1249,  1250,  1251,  1252,  1252,  1254,  1253,  1253,
    1255,  1255,  1256,  1256,  1256,  1256,  1256,  1256,  1256,  1256,
    1256,  1256,  1256,  1256,  1256,  1256,  1256,  1256,  1257,  1258,
    1258,  1258,  1258,  1258,  1258,  1258,  1259,  1259,  1259,  1260,
    1260,  1261,  1261,  1261,  1261,  1262,  1263,  1263,  1263,  1263,
    1264,  1264,  1264,  1265,  1266,  1266,  1266,  1267,  1267,  1268,
    1268,  1268,  1268,  1268,  1269,  1269,  1270,  1270,  1271,  1271,
    1271,  1272,  1273,  1274,  1275,  1275,  1276,  1276,  1276,  1276,
    1277,  1278,  1279,  1280,  1280,  1281,  1281,  1281,  1282,  1282,
    1283,  1283,  1284,  1285,  1286,  1287,  1288,  1287,  1289,  1289,
    1290,  1290,  1291,  1292,  1291,  1293,  1294,  1291,  1291,  1295,
    1295,  1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,
    1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,
    1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,
    1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,
    1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,  1296,
    1297,  1297,  1298,  1298,  1298,  1298,  1298,  1298,  1298,  1298,
    1298,  1298,  1298,  1298,  1298,  1298,  1298,  1298,  1299,  1299,
    1300,  1300,  1301,  1301,  1302,  1302,  1303,  1303,  1304,  1304,
    1305,  1305,  1306,  1306,  1306,  1306,  1306,  1306,  1306,  1306,
    1306,  1306,  1306,  1306,  1306,  1306,  1307,  1307,  1307,  1307,
    1307,  1307,  1307,  1307,  1307,  1307,  1307,  1307,  1307,  1307,
    1307,  1308,  1308,  1308,  1308,  1308,  1308,  1308,  1308,  1308,
    1308,  1309,  1309,  1310,  1310,  1310,  1310,  1310,  1311,  1312,
    1312,  1312,  1312,  1312,  1312,  1312,  1312,  1312,  1312,  1312,
    1312,  1312,  1312,  1312,  1313,  1313,  1313,  1313,  1313,  1313,
    1313,  1313,  1313,  1313,  1314,  1314,  1314,  1314,  1314,  1314,
    1314,  1314,  1314,  1314,  1315,  1315,  1316,  1316,  1317,  1317,
    1318,  1318,  1318,  1318,  1318,  1319,  1319,  1319,  1319,  1319,
    1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,  1319,
    1319,  1320,  1320,  1320,  1321,  1321,  1321,  1321,  1321,  1321,
    1321,  1321,  1322,  1322,  1322,  1322,  1322,  1322,  1323,  1323,
    1323,  1323,  1323,  1323,  1323,  1323,  1324,  1324,  1324,  1324,
    1325,  1325,  1325,  1326,  1326,  1326,  1326,  1326,  1326,  1327,
    1327,  1327,  1327,  1328,  1328,  1328,  1328,  1328,  1328,  1328,
    1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,
    1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,
    1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,
    1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,  1329,
    1329,  1329,  1329,  1329,  1329,  1329,  1329,  1330,  1330,  1330,
    1331,  1331,  1331,  1331,  1331,  1331,  1331,  1331,  1331,  1332,
    1332,  1332,  1332,  1332,  1332,  1332,  1332,  1332,  1332,  1332,
    1332,  1332,  1332,  1332,  1332,  1332,  1332,  1332,  1332,  1332,
    1332,  1332,  1333,  1334,  1335,  1335,  1335,  1335,  1335,  1335,
    1335,  1335,  1336,  1336,  1336,  1337,  1337,  1338,  1339,  1339,
    1340,  1340,  1341,  1341,  1342,  1342,  1342,  1343,  1343,  1344,
    1344,  1345,  1345,  1346,  1346,  1347,  1348,  1348,  1349,  1349,
    1350,  1351,  1351,  1351,  1352,  1353,  1353,  1353,  1354,  1355,
    1356,  1357,  1358,  1359,  1356,  1360,  1356,  1361,  1362,  1361,
    1363,  1361,  1364,  1364,  1365,  1366,  1366,  1366,  1367,  1367,
    1367,  1367,  1367,  1367,  1368,  1369,  1369,  1370,  1370,  1370,
    1371,  1372,  1371,  1373,  1373,  1374,  1374,  1374,  1374,  1374,
    1376,  1375,  1377,  1377,  1378,  1379,  1380,  1380,  1382,  1383,
    1381,  1385,  1384,  1384,  1386,  1386,  1386,  1386,  1386,  1386,
    1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,
    1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,
    1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,
    1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,
    1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,
    1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1386,  1388,
    1387,  1390,  1389,  1391,  1389,  1389,  1389,  1389,  1389,  1389,
    1389,  1389,  1389,  1389,  1389,  1389,  1389,  1389,  1389,  1389,
    1389,  1389,  1389,  1389,  1389,  1389,  1389,  1389,  1392,  1392,
    1394,  1393,  1395,  1395,  1395,  1396,  1396,  1396,  1397,  1397,
    1398,  1398,  1399,  1399,  1400,  1400,  1400,  1400,  1400,  1401,
    1401,  1402,  1402,  1403,  1403,  1404,  1404,  1404,  1405,  1406,
    1407,  1408,  1408,  1408,  1408,  1408,  1408,  1408,  1408,  1408,
    1408,  1408,  1408,  1408,  1408,  1408,  1408,  1408,  1408,  1408,
    1408,  1408,  1408,  1408,  1408,  1408,  1408,  1408,  1408,  1408,
    1408,  1408,  1408,  1408,  1408,  1408,  1409,  1409,  1410,  1411,
    1411,  1411,  1412,  1412,  1412,  1413,  1413,  1414,  1414,  1416,
    1415,  1417,  1417,  1417,  1417,  1418,  1418,  1419,  1419,  1421,
    1420,  1422,  1422,  1423,  1423,  1424,  1424,  1426,  1425,  1427,
    1427,  1428,  1429,  1429,  1431,  1430,  1433,  1432,  1434,  1434,
    1434,  1434,  1434,  1435,  1435,  1436,  1436,  1437,  1437,  1438,
    1439,  1438,  1440,  1440,  1441,  1441,  1442,  1442,  1442,  1442,
    1443,  1443,  1443,  1443,  1443,  1444,  1444,  1445,  1445,  1446,
    1446,  1446,  1447,  1447,  1448,  1448,  1449,  1449,  1450,  1451,
    1451,  1453,  1452,  1454,  1454,  1455,  1455,  1457,  1456,  1458,
    1458,  1459,  1459,  1460,  1460,  1460,  1460,  1460,  1462,  1461,
    1463,  1463,  1465,  1464,  1466,  1467,  1467,  1468,  1469,  1471,
    1470,  1472,  1472,  1474,  1473,  1475,  1475,  1476,  1476,  1477,
    1477,  1479,  1478,  1480,  1481,  1481,  1482,  1482,  1482,  1482,
    1482,  1484,  1483,  1485,  1485,  1485,  1485,  1485,  1485,  1485,
    1485,  1485,  1485,  1485,  1486,  1486,  1487,  1487,  1489,  1488,
    1490,  1490,  1491,  1491,  1492,  1492,  1493,  1493,  1494,  1494,
    1494,  1494,  1494,  1495,  1495,  1495,  1495,  1496,  1496,  1498,
    1497,  1499,  1501,  1500,  1502,  1502,  1503,  1503,  1503,  1505,
    1504,  1506,  1506,  1507,  1507,  1508,  1508,  1508,  1508,  1508,
    1510,  1511,  1509,  1512,  1512,  1514,  1515,  1513,  1517,  1516,
    1518,  1518,  1518,  1519,  1519,  1520,  1520,  1521,  1521,  1521,
    1522,  1522,  1523,  1523,  1523,  1523,  1523,  1523,  1523,  1523,
    1524,  1525,  1526,  1526,  1526,  1527,  1527,  1527,  1527,  1528,
    1528,  1529,  1529,  1530,  1531,  1532,  1532,  1532,  1532,  1532,
    1532,  1532,  1532,  1532,  1532,  1532,  1532,  1532,  1532,  1532,
    1532,  1532,  1532,  1532,  1532,  1532,  1532,  1532,  1533,  1533,
    1535,  1534,  1536,  1536,  1536,  1536,  1536,  1537,  1537,  1539,
    1538,  1541,  1540,  1542,  1544,  1543,  1545,  1546,  1546,  1547,
    1547,  1547,  1548,  1548,  1549,  1549,  1550,  1550,  1550,  1551,
    1551,  1551,  1552,  1552,  1553,  1553,  1554,  1554,  1554,  1554,
    1554,  1555,  1555,  1556,  1556,  1558,  1557,  1559,  1559,  1559,
    1559,  1559,  1559,  1559,  1560,  1560,  1562,  1561,  1563,  1565,
    1564,  1566,  1568,  1567,  1569,  1570,  1570,  1571,  1573,  1572,
    1574,  1574,  1574,  1575,  1575,  1576,  1577,  1578,  1578,  1580,
    1579,  1581,  1582,  1582,  1583,  1583,  1583,  1584,  1584,  1585,
    1585,  1586,  1587,  1587,  1587,  1587,  1587,  1587,  1587,  1588,
    1588,  1590,  1589,  1591,  1591,  1593,  1592,  1594,  1594,  1596,
    1595,  1597,  1598,  1598,  1598,  1599,  1599,  1599,  1599,  1601,
    1600,  1602,  1603,  1604,  1604,  1605,  1605,  1605,  1605,  1605,
    1605,  1606,  1606,  1607,  1607,  1608,  1608,  1608,  1608,  1608,
    1609,  1610,  1610,  1610,  1610,  1610,  1611,  1612,  1614,  1613,
    1616,  1617,  1615,  1618,  1618,  1619,  1619,  1620,  1621,  1621,
    1623,  1622,  1624,  1625,  1625,  1627,  1626,  1629,  1628,  1630,
    1630,  1631,  1631,  1633,  1632,  1634,  1634,  1636,  1635,  1637,
    1637,  1638,  1638,  1640,  1639,  1641,  1641,  1642,  1643,  1643,
    1644,  1644,  1644,  1644,  1645,  1645,  1646,  1646,  1646,  1646,
    1647,  1647,  1648,  1648,  1648,  1649,  1649,  1649,  1650,  1650,
    1650,  1651,  1651,  1653,  1652,  1654,  1655,  1654,  1656,  1654,
    1657,  1657,  1658,  1658,  1659,  1659,  1660,  1660,  1660,  1660,
    1660,  1661,  1661,  1662,  1662,  1663,  1663,  1664,  1665,  1665,
    1667,  1666,  1669,  1668,  1670,  1670,  1671,  1673,  1672,  1674,
    1675,  1675,  1676,  1676,  1676,  1676,  1677,  1677,  1678,  1678,
    1679,  1679,  1680,  1681,  1681,  1681,  1682,  1682,  1683,  1683,
    1683,  1684,  1684,  1685,  1685,  1686,  1686,  1687,  1689,  1688,
    1690,  1691,  1691,  1692,  1692,  1692,  1693,  1693,  1694,  1695,
    1695,  1696,  1697,  1697,  1699,  1698,  1700,  1701,  1703,  1702,
    1704,  1705,  1705,  1707,  1706,  1708,  1709,  1709,  1710,  1710,
    1711,  1711,  1712,  1714,  1713,  1715,  1715,  1716,  1716,  1717,
    1717,  1718,  1718,  1719,  1720,  1720,  1722,  1721,  1723,  1723,
    1724,  1724,  1725,  1726,  1726,  1726,  1726,  1727,  1727,  1729,
    1728,  1730,  1730,  1730,  1730,  1730,  1730,  1730,  1730,  1731,
    1731,  1732,  1732,  1733,  1734,  1735,  1735,  1736,  1736,  1736,
    1736,  1736,  1736,  1736,  1736,  1737,  1737,  1737,  1738,  1739,
    1739,  1740,  1741,  1741,  1742,  1742,  1743,  1744,  1746,  1745,
    1748,  1747,  1749,  1749,  1750,  1750,  1751,  1751,  1752,  1752,
    1753,  1753,  1753,  1754,  1754,  1754,  1756,  1755,  1757,  1758,
    1758,  1759,  1759,  1759,  1759,  1760,  1760,  1760,  1760,  1760,
    1760,  1761,  1762,  1762,  1763,  1763,  1765,  1764,  1764,  1764,
    1766,  1766,  1766,  1766,  1766,  1767,  1767,  1768,  1768,  1769,
    1769,  1769,  1769,  1771,  1770,  1772,  1774,  1773,  1775,  1775,
    1776,  1777,  1777,  1778,  1778,  1779,  1779,  1780,  1780,  1782,
    1781,  1783,  1783,  1783,  1783,  1784,  1784,  1785,  1786,  1786,
    1788,  1787,  1789,  1789,  1791,  1790,  1792,  1794,  1793,  1795,
    1797,  1796,  1798,  1799,  1799,  1800,  1800,  1801,  1802,  1802,
    1803,  1804,  1804,  1805,  1805,  1806,  1806,  1808,  1807,  1809,
    1809,  1811,  1810,  1812,  1812,  1812,  1812,  1812,  1813,  1814,
    1814,  1815,  1815,  1815,  1815,  1815,  1816,  1817,  1817,  1818,
    1818,  1818,  1819,  1819,  1819,  1819,  1820,  1821,  1821,  1822,
    1823,  1823,  1824,  1824,  1825,  1825,  1827,  1826,  1828,  1829,
    1829,  1830,  1830,  1830,  1830,  1831,  1831,  1832,  1832,  1832,
    1833,  1833,  1835,  1834,  1837,  1838,  1836,  1839,  1839,  1840,
    1840,  1841,  1842,  1842,  1842,  1843,  1843,  1844,  1844,  1845,
    1845,  1846,  1846,  1847,  1848,  1848,  1849,  1849,  1850,  1851,
    1851,  1852,  1852,  1852,  1853,  1853,  1854,  1854,  1855,  1855,
    1855,  1856,  1856,  1856,  1857,  1857,  1858,  1858,  1860,  1859,
    1862,  1861,  1863,  1863,  1863,  1864,  1864,  1865,  1866,  1866,
    1867,  1867,  1868,  1869,  1869,  1870,  1870,  1870,  1871,  1871,
    1872,  1873,  1873,  1874,  1875,  1875,  1875,  1876,  1876,  1877,
    1878,  1878,  1879,  1880,  1880,  1880,  1881,  1881,  1882,  1883,
    1883,  1884,  1885,  1885,  1885,  1886,  1886,  1887,  1888,  1888,
    1889,  1890,  1890,  1890,  1891,  1891,  1892,  1893,  1893,  1894,
    1895,  1895,  1896,  1896,  1897,  1897,  1898,  1899,  1899,  1900,
    1901,  1901,  1902,  1902,  1903,  1904,  1904,  1905,  1906,  1906,
    1907,  1907,  1908,  1908,  1909,  1910,  1910,  1911,  1912,  1912,
    1913,  1913,  1914,  1914,  1915,  1915,  1916,  1916,  1917,  1917,
    1918,  1920,  1919,  1921,  1921,  1922,  1922,  1922,  1922,  1922,
    1922,  1922,  1922,  1922,  1922,  1922,  1922,  1922,  1922,  1923,
    1923,  1924,  1925,  1925,  1925,  1925,  1925,  1925,  1925,  1925,
    1925,  1925,  1925,  1925,  1925,  1925,  1926,  1926,  1927,  1927,
    1928,  1928,  1929,  1930,  1931,  1931,  1932,  1932,  1932,  1933,
    1933,  1933,  1934,  1934,  1934,  1935,  1935,  1936,  1936,  1936,
    1937,  1937,  1938,  1938,  1938,  1938,  1938,  1938,  1939,  1939,
    1940,  1941,  1942,  1942,  1943,  1944,  1944,  1945,  1945,  1946,
    1947,  1948,  1949,  1949,  1950,  1951,  1951,  1952,  1953,  1953,
    1953,  1954,  1955,  1955,  1956,  1957,  1957,  1958,  1958,  1959,
    1960,  1960,  1961,  1962,  1962,  1963,  1963,  1964,  1965,  1965,
    1966,  1966,  1966,  1967,  1967,  1968,  1968,  1969,  1969,  1970,
    1970,  1971,  1971,  1971,  1971,  1971,  1971,  1971,  1971,  1971,
    1971,  1971,  1972,  1973,  1973,  1974,  1974,  1974,  1975,  1975,
    1975,  1975,  1975,  1975,  1975,  1976,  1976,  1976,  1976,  1976,
    1976,  1977,  1978,  1979,  1980,  1980,  1981,  1981,  1982,  1983,
    1984,  1984,  1985,  1985,  1986,  1986,  1986,  1987,  1987,  1988,
    1988,  1989,  1989,  1989,  1990,  1990,  1990,  1991,  1991,  1991,
    1992,  1992,  1993,  1993,  1994,  1994,  1995,  1995,  1995,  1996,
    1997,  1998,  1998,  1999,  2000,  2001,  2002,  2003,  2003,  2003,
    2003,  2004,  2004,  2005,  2005,  2006,  2006,  2006,  2006,  2007,
    2008,  2008,  2010,  2009,  2011,  2011,  2012,  2013,  2013,  2014,
    2015,  2016,  2017,  2017,  2018,  2018,  2018,  2018,  2018,  2018,
    2018,  2019,  2019,  2020,  2020,  2021,  2021,  2021,  2021,  2021,
    2021,  2021,  2022,  2022,  2022,  2022,  2023,  2023,  2023,  2023,
    2023,  2023,  2023,  2023,  2023,  2023,  2023,  2023,  2023,  2023,
    2024,  2024,  2025,  2025,  2025,  2025,  2026,  2026,  2026,  2026,
    2026,  2027,  2027,  2027,  2028,  2028,  2028,  2029,  2029,  2029,
    2031,  2030,  2032,  2032,  2033,  2033,  2034,  2034,  2035,  2035,
    2036,  2037,  2037,  2038,  2038,  2038,  2039,  2039,  2040,  2040,
    2041,  2041,  2042,  2042,  2042,  2043,  2043,  2044,  2044,  2044,
    2045,  2045,  2046,  2046,  2047,  2047,  2047,  2047,  2047,  2047,
    2047,  2047,  2048,  2048,  2049,  2049,  2050,  2050,  2051,  2051,
    2051,  2051,  2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,
    2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,
    2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,
    2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,
    2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,  2052,
    2052,  2052,  2052,  2053,  2053,  2053,  2053,  2053,  2053,  2053,
    2053,  2053,  2053,  2053,  2053,  2053,  2053,  2053,  2053,  2053,
    2053,  2053,  2053,  2053,  2053,  2053,  2054,  2054,  2055,  2055,
    2056,  2056,  2057,  2057,  2058,  2058,  2058,  2059,  2059,  2060,
    2060,  2061,  2061,  2062,  2062,  2063,  2063,  2064,  2064,  2065,
    2065,  2066,  2066,  2067,  2067,  2068,  2068,  2069,  2069,  2070,
    2070,  2071,  2071,  2072,  2072,  2073,  2073,  2074,  2074,  2075,
    2075,  2076,  2076,  2077,  2077,  2077,  2078,  2078,  2078,  2079,
    2079,  2080,  2080,  2081,  2081,  2082,  2082,  2083,  2083,  2084,
    2084,  2084,  2085,  2085,  2085,  2086,  2086,  2086,  2086,  2087,
    2087,  2087,  2088,  2088,  2089,  2089,  2090,  2090,  2090,  2091,
    2091,  2091,  2092,  2092,  2093,  2093,  2094,  2094,  2095,  2095,
    2096,  2096,  2097,  2097,  2098,  2098,  2098,  2099,  2099,  2099,
    2100,  2100,  2101,  2101,  2102,  2102,  2102,  2103,  2103,  2103,
    2103,  2104,  2104,  2105,  2105,  2106,  2106,  2107,  2107,  2108,
    2108,  2109,  2109,  2110,  2110,  2110,  2111,  2111,  2112,  2112,
    2113,  2113,  2114,  2114,  2114,  2115,  2115,  2116,  2116,  2117,
    2117,  2118,  2118,  2119,  2119,  2120,  2120,  2121,  2121,  2122,
    2122,  2123,  2123,  2124,  2124,  2124,  2125,  2125,  2126,  2126,
    2127,  2127,  2128,  2128,  2128,  2129,  2129,  2130,  2130,  2131,
    2131,  2132,  2132,  2132,  2132,  2133,  2133,  2134,  2134,  2135,
    2135,  2136,  2136,  2137,  2137,  2138,  2138,  2139,  2139,  2139,
    2140,  2140,  2141,  2141,  2142,  2142,  2143,  2143,  2144,  2144,
    2145,  2145,  2146,  2146,  2147,  2147,  2148,  2148,  2149,  2149,
    2150,  2150
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     0,     2,     1,     2,
       1,     1,     0,     2,     5,     5,     0,     1,     1,     2,
       0,     4,     0,     4,     0,     0,     5,     0,     3,     1,
       1,     0,     0,     8,     0,     6,     1,     1,     1,     1,
       0,     2,     0,     3,     1,     1,     1,     1,     2,     2,
       1,     1,     0,     3,     5,     0,     3,     1,     1,     1,
       1,     0,     5,     0,     3,     1,     1,     1,     0,     4,
       1,     1,     1,     1,     3,     0,     3,     2,     0,     3,
       0,     1,     1,     2,     1,     1,     1,     1,     1,     0,
       4,     0,     3,     0,     3,     0,     4,     0,     2,     3,
       2,     1,     2,     1,     1,     1,     1,     5,     2,     0,
       4,     2,     3,     4,     4,     8,     8,     3,     4,     1,
       1,     1,     1,     1,     2,     0,     4,     0,     2,     3,
       1,     2,     3,     3,     3,     3,     3,     1,     2,     2,
       2,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     3,     2,     3,
       3,     1,     0,     1,     1,     3,     4,     0,     4,     0,
       4,     0,     4,     0,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     1,     2,
       1,     3,     0,     4,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     0,     2,     3,     1,     2,
       3,     1,     2,     1,     2,     3,     1,     2,     3,     6,
       1,     2,     1,     3,     0,     2,     2,     0,     2,     4,
       5,     0,     3,     3,     5,     3,     4,     3,     3,     4,
       0,     3,     0,     2,     0,     2,     0,     5,     2,     2,
       0,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     5,     5,     5,     5,
       5,     5,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     3,     0,     1,
       1,     1,     1,     0,     1,     1,     4,     1,     1,     1,
       9,     0,     1,     0,     4,     0,     4,     3,     1,     0,
       4,     2,     3,     4,     4,     8,     8,     6,     1,     5,
       0,     1,     1,     0,     5,     2,     2,     2,     0,     5,
       6,     1,     0,     1,     2,     0,     2,     3,     1,     1,
       3,     1,     2,     4,     0,     5,     1,     1,     1,     1,
       7,     0,     2,     1,     2,     0,     2,     2,     1,     4,
       3,     1,     1,     3,     2,     2,     2,     0,     2,     2,
       0,     2,     3,     1,     2,     1,     1,     1,     5,     0,
       1,     1,     1,     4,     0,     6,     1,     2,     2,     0,
       2,     0,    10,     0,     3,     0,     3,     0,     2,     2,
       0,     5,     3,     1,     1,     0,     2,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     5,     0,
       1,     1,     4,     6,     9,     0,     3,     0,     2,     0,
       2,     3,     5,     5,     1,     1,     1,     1,     3,     5,
       0,     2,     1,     1,     1,     4,     2,     2,     4,     1,
       1,     1,     1,     1,     1,     1,     4,     0,     2,     2,
       2,     2,     1,     2,     0,     0,     5,     0,     2,     2,
       0,     5,     0,     2,     4,     3,     4,     0,     1,     1,
       1,     2,     4,     4,     4,     4,     4,     4,     4,     4,
       4,     4,     4,    11,     0,     1,     1,     2,     4,     4,
       4,     6,     4,     3,     4,     0,     1,     1,     1,     2,
       4,     4,     4,     4,     4,     4,     6,     0,     0,     5,
       0,     0,     2,     2,     3,     1,     1,     1,     0,     4,
       3,     2,     0,     1,     1,     1,     1,     0,     2,     1,
       3,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     5,     0,     2,     0,
       4,     5,     0,     7,     2,     2,     1,     3,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     3,     0,     2,     0,     1,     2,     1,     2,
       0,     5,     0,     2,     1,     1,     3,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       4,     3,     3,     3,     4,     3,     3,     1,     1,     1,
       1,     1,     1,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     1,     1,     1,     1,     0,     1,
       3,     3,     6,     0,     2,     6,     8,     7,     0,     2,
       0,     2,     0,     2,     0,     3,     0,     3,     0,     1,
       0,     2,     0,     3,     1,     1,     1,     1,     2,     4,
       1,     1,     0,     1,     3,     1,     2,     1,     2,     2,
       0,     1,     1,     3,     1,     0,     5,     1,     2,     3,
       1,     0,     4,     2,     2,     2,     4,     0,     0,     5,
       0,     0,     5,     0,     0,     5,     0,     2,     0,     6,
       0,     2,     2,     2,     4,     1,     1,     2,     2,     1,
       1,     1,     1,     2,     1,     4,     2,     1,     3,     2,
       1,     1,     0,     2,     1,     1,     1,     1,     1,     3,
       3,     4,     4,     4,     3,     0,     2,     0,     5,     3,
       0,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     1,
       1,     3,     3,     1,     1,     1,     0,     2,     2,     0,
       2,     0,     2,     2,     1,     3,     1,     2,     2,     1,
       1,     1,     1,     4,     0,     3,     2,     1,     1,     3,
       4,     5,     4,     5,     1,     1,     0,     2,     1,     1,
       1,     6,     2,     2,     0,     2,     1,     1,     2,     2,
       1,     2,     4,     0,     1,     1,     1,     1,     2,     1,
       1,     2,     1,     4,     2,     0,     0,     5,     0,     1,
       2,     3,     1,     0,     4,     0,     0,     7,     3,     0,
       2,     2,     2,     1,     1,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     3,     3,     3,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     1,
       1,     2,     3,     2,     2,     2,     3,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     2,     2,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     1,     3,     3,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       3,     3,     1,     2,     3,     3,     3,     1,     2,     1,
       2,     1,     1,     1,     1,     2,     1,     1,     0,     1,
       4,     0,     1,     1,     4,     0,     1,     1,     3,     2,
       0,     0,     0,     0,    11,     0,     4,     0,     0,     3,
       0,     3,     1,     2,     4,     0,     2,     2,     0,     3,
       3,     4,     2,     1,     3,     0,     1,     0,     2,     2,
       0,     0,     7,     0,     2,     1,     1,     2,     1,     1,
       0,     6,     0,     2,     2,     1,     0,     1,     0,     0,
       3,     0,     2,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     2,     0,
       4,     0,     4,     0,     5,     3,     3,     3,     3,     4,
       3,     4,     3,     3,     4,     4,     4,     3,     4,     3,
       4,     5,     3,     4,     3,     3,     2,     3,     1,     1,
       0,     3,     5,     4,     4,     1,     3,     3,     1,     1,
       0,     1,     1,     2,     1,     1,     1,     2,     3,     1,
       2,     1,     3,     1,     2,     2,     2,     2,     3,     3,
       3,     1,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       1,     1,     1,     1,     4,     1,     2,     1,     1,     3,
       3,     3,     3,     3,     3,     4,     0,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     0,
       4,     4,     5,     6,     8,     0,     2,     0,     1,     0,
       3,     4,     5,     0,     2,     0,     2,     0,     3,     1,
       2,     4,     0,     2,     0,     4,     0,     8,     0,     1,
       1,     1,     1,     1,     2,     0,     2,     1,     1,     0,
       0,     3,     1,     2,     2,     3,     0,     2,     2,     2,
       0,     3,     2,     2,     4,     1,     1,     1,     1,     0,
       2,     2,     0,     1,     2,     2,     0,     1,     2,     0,
       1,     0,     3,     1,     2,     1,     1,     0,     3,     1,
       1,     2,     3,     0,     1,     3,     3,     2,     0,     4,
       0,     3,     0,     4,     4,     0,     1,     1,     1,     0,
       3,     2,     1,     0,     4,     4,     2,     1,     2,     0,
       1,     0,     3,     3,     0,     3,     0,     2,     1,     2,
       1,     0,     4,     3,     3,     3,     3,     2,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     2,     0,     3,
       1,     1,     0,     2,     1,     2,     1,     2,     1,     2,
       1,     1,     2,     2,     2,     2,     2,     1,     1,     0,
       3,     2,     0,     3,     1,     2,     1,     1,     1,     0,
       5,     0,     1,     1,     2,     3,     3,     3,     3,     2,
       0,     0,     5,     1,     1,     0,     0,     7,     0,     5,
       1,     1,     1,     0,     1,     0,     2,     1,     2,     1,
       1,     2,     1,     2,     1,     5,     1,     1,     1,     2,
       1,     1,     0,     1,     1,     1,     1,     0,     1,     3,
       3,     1,     1,     4,     3,     1,     2,     2,     1,     1,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     3,     3,     3,     3,     3,     0,     1,
       0,     4,     4,     6,     6,     8,     8,     0,     1,     0,
       3,     0,     3,     3,     0,     4,     2,     1,     3,     1,
       1,     1,     2,     1,     1,     2,     2,     2,     2,     3,
       3,     3,     2,     3,     1,     3,     2,     1,     1,     1,
       1,     0,     2,     0,     1,     0,     3,     0,     2,     1,
       2,     1,     1,     1,     0,     2,     0,     3,     1,     0,
       3,     1,     0,     3,     3,     0,     3,     2,     0,     6,
       5,     3,     2,     0,     1,     0,     0,     0,     1,     0,
       3,     5,     0,     2,     0,     3,     3,     0,     2,     1,
       2,     4,     1,     1,     1,     1,     1,     1,     1,     0,
       3,     0,     3,     1,     2,     0,     3,     2,     2,     0,
       3,     2,     1,     1,     1,     2,     1,     1,     1,     0,
       3,     2,     5,     1,     2,     2,     2,     1,     1,     1,
       2,     1,     2,     4,     2,     0,     1,     1,     1,     1,
       4,     0,     1,     1,     2,     2,     3,     3,     0,     5,
       0,     0,     9,     0,     2,     1,     2,     1,     0,     1,
       0,     5,     7,     0,     2,     0,     3,     0,     4,     2,
       2,     0,     1,     0,     3,     3,     4,     0,     4,     4,
       6,     0,     1,     0,     3,     1,     2,     6,     0,     1,
       1,     1,     1,     1,     0,     3,     0,     1,     1,     2,
       2,     2,     1,     1,     1,     2,     1,     1,     1,     1,
       1,     3,     1,     0,     3,     4,     0,     6,     0,     5,
       0,     1,     1,     1,     1,     3,     0,     2,     1,     3,
       3,     0,     3,     1,     1,     1,     3,     6,     0,     2,
       0,     3,     0,     3,     2,     1,     1,     0,     4,     7,
       0,     2,     0,     1,     2,     1,     2,     3,     3,     1,
       0,     1,     1,     4,     4,     2,     0,     1,     1,     3,
       2,     0,     3,     1,     1,     0,     1,     1,     0,     4,
       5,     1,     1,     0,     2,     2,     0,     1,     2,     0,
       1,     2,     0,     1,     0,     3,     2,     1,     0,     4,
       4,     0,     1,     0,     4,     5,     0,     1,     2,     3,
       0,     1,     1,     0,     4,     4,     6,     0,     2,     0,
       2,     1,     2,     3,     0,     1,     0,     3,     2,     5,
       0,     1,     2,     2,     2,     2,     2,     0,     2,     0,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     4,     3,     1,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     4,     3,     5,     4,     1,
       2,     3,     1,     2,     3,     3,     4,     4,     0,     3,
       0,     7,     0,     5,     0,     2,     0,     3,     0,     1,
       0,     2,     4,     0,     2,     4,     0,     4,     4,     0,
       3,     0,     4,     1,     1,     1,     2,     2,     2,     2,
       1,     1,     2,     1,     0,     1,     0,     4,     2,     2,
       0,     2,     1,     4,     4,     0,     1,     1,     1,     1,
       1,     1,     1,     0,     4,     5,     0,     2,     1,     2,
       2,     0,     3,     1,     1,     0,     4,     0,     1,     0,
       4,     4,     6,     6,     8,     0,     1,     2,     0,     1,
       0,     3,     1,     2,     0,     3,     5,     0,     3,     2,
       0,     4,     6,     0,     3,     1,     3,     2,     2,     2,
       3,     0,     3,     0,     3,     0,     1,     0,     3,     1,
       2,     0,     3,     1,     1,     1,     1,     1,     7,     0,
       1,     1,     1,     1,     1,     1,     4,     1,     2,     1,
       2,     3,     0,     1,     2,     1,     3,     1,     1,     4,
       1,     2,     2,     3,     1,     1,     0,     4,     6,     0,
       2,     0,     4,     3,     3,     1,     1,     0,     1,     1,
       0,     1,     0,     5,     0,     0,    12,     0,     1,     1,
       2,     2,     2,     1,     1,     0,     4,     0,     3,     0,
       3,     1,     2,     3,     0,     3,     1,     2,     3,     0,
       1,     1,     1,     1,     0,     2,     1,     2,     1,     2,
       2,     2,     2,     1,     1,     3,     0,     1,     0,     5,
       0,     7,     0,     2,     2,     0,     1,     3,     1,     1,
       0,     1,     2,     1,     1,     0,     2,     2,     0,     1,
       2,     0,     1,     2,     0,     2,     2,     0,     1,     2,
       0,     1,     2,     0,     2,     2,     0,     1,     2,     0,
       1,     2,     0,     2,     2,     0,     1,     2,     0,     1,
       2,     0,     2,     2,     0,     1,     2,     0,     1,     2,
       2,     2,     2,     2,     0,     1,     2,     0,     1,     2,
       2,     2,     0,     1,     2,     0,     1,     2,     0,     1,
       2,     2,     0,     1,     2,     0,     1,     2,     0,     2,
       0,     3,     2,     1,     0,     2,     0,     3,     1,     1,
       1,     0,     2,     1,     2,     1,     2,     3,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     3,     0,     1,     1,     3,
       3,     1,     3,     3,     1,     3,     1,     2,     2,     1,
       3,     1,     1,     3,     1,     3,     1,     3,     1,     2,
       2,     1,     1,     2,     1,     1,     2,     2,     3,     1,
       1,     1,     1,     2,     1,     0,     2,     1,     1,     1,
       3,     1,     1,     2,     1,     0,     1,     1,     2,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     3,     0,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     2,     2,     2,     4,     3,     5,
       5,     1,     2,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     2,     2,     2,     1,     1,     1,     2,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     3,     2,     2,
       1,     1,     2,     1,     1,     3,     2,     2,     1,     1,
       1,     3,     0,     2,     1,     3,     3,     4,     5,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     5,     5,     5,
       4,     5,     4,     5,     5,     5,     5,     5,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     4,     5,     0,     3,     2,     1,     3,     3,
       0,     2,     1,     3,     1,     3,     1,     3,     1,     3,
       0,     0,     1,     0,     3,     2,     0,     1,     0,     2,
       0,     2,     0,     1,     1,     0,     1,     0,     1,     2,
       0,     2,     0,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     2,     0,     5,     0,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     3,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     1,     0,     1,     2,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     1,     0,     1,     1,     0,     1,     1,     1,     0,
       1,     1,     0,     1,     0,     1,     0,     1,     1,     0,
       2,     2,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     0,     2,     1,     0,     1,     1,
       0,     1,     0,     1,     0,     1,     1,     0,     2,     1,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     1,     0,     1,     0,     1,
       0,     1,     0,     1,     2,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     1,     0,     1,     0,     3,
       0,     1,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     1,     1,     1,     1,     1,     1,     2,     1,
       2,     1,     2,     1,     2,     1,     2,     1,     2,     1,
       2,     2
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 2997 "parser.y" /* yacc.c:1646  */
    {
	clear_initial_values ();
	current_program = NULL;
	defined_prog_list = NULL;
	cobc_cs_check = 0;
	main_flag_set = 0;
	current_program = cb_build_program (NULL, 0);
	cb_set_intr_when_compiled ();
	cb_build_registers ();
	cb_add_external_defined_registers ();
  }
#line 11630 "parser.c" /* yacc.c:1646  */
    break;

  case 3:
#line 3009 "parser.y" /* yacc.c:1646  */
    {
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
	if (depth > 1) {
		cb_error (_("multiple PROGRAM-ID's without matching END PROGRAM"));
	}
	if (cobc_flag_main && !main_flag_set) {
		cb_error (_("executable requested but no program found"));
	}
	if (errorcount > 0) {
		YYABORT;
	}
	if (!current_program->entry_list) {
		backup_current_pos ();
		emit_entry (current_program->program_id, 0, NULL, NULL);
	}
  }
#line 11654 "parser.c" /* yacc.c:1646  */
    break;

  case 6:
#line 3036 "parser.y" /* yacc.c:1646  */
    {
	first_prog = 1;
	depth = 0;
	setup_from_identification = 0;
  }
#line 11664 "parser.c" /* yacc.c:1646  */
    break;

  case 12:
#line 3055 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	current_section = NULL;
	current_paragraph = NULL;
	l = cb_build_alphanumeric_literal (demangle_name,
					   strlen (demangle_name));
	current_program->program_name = (char *)CB_LITERAL (l)->data;
	current_program->program_id
		= cb_build_program_id (current_program->program_name, 0);
	current_program->prog_type = COB_MODULE_TYPE_PROGRAM;
	if (!main_flag_set) {
		main_flag_set = 1;
		current_program->flag_main = cobc_flag_main;
	}
	check_relaxed_syntax (COBC_HD_PROGRAM_ID);
  }
#line 11686 "parser.c" /* yacc.c:1646  */
    break;

  case 13:
#line 3074 "parser.y" /* yacc.c:1646  */
    {
	backup_current_pos ();
	clean_up_program (NULL, COB_MODULE_TYPE_PROGRAM);
  }
#line 11695 "parser.c" /* yacc.c:1646  */
    break;

  case 16:
#line 3102 "parser.y" /* yacc.c:1646  */
    {
	backup_current_pos ();
	clean_up_program (NULL, COB_MODULE_TYPE_PROGRAM);
  }
#line 11704 "parser.c" /* yacc.c:1646  */
    break;

  case 20:
#line 3116 "parser.y" /* yacc.c:1646  */
    {
	backup_current_pos ();
  }
#line 11712 "parser.c" /* yacc.c:1646  */
    break;

  case 21:
#line 3120 "parser.y" /* yacc.c:1646  */
    {
	first_nested_program = 0;
	clean_up_program ((yyvsp[-1]), COB_MODULE_TYPE_PROGRAM);
  }
#line 11721 "parser.c" /* yacc.c:1646  */
    break;

  case 22:
#line 3128 "parser.y" /* yacc.c:1646  */
    {
	backup_current_pos ();
  }
#line 11729 "parser.c" /* yacc.c:1646  */
    break;

  case 23:
#line 3132 "parser.y" /* yacc.c:1646  */
    {
	clean_up_program ((yyvsp[-1]), COB_MODULE_TYPE_FUNCTION);
  }
#line 11737 "parser.c" /* yacc.c:1646  */
    break;

  case 24:
#line 3141 "parser.y" /* yacc.c:1646  */
    {
	cb_validate_program_environment (current_program);
  }
#line 11745 "parser.c" /* yacc.c:1646  */
    break;

  case 25:
#line 3145 "parser.y" /* yacc.c:1646  */
    {
	/* note:
	   we also validate all references we found so far here */
	cb_validate_program_data (current_program);
  }
#line 11755 "parser.c" /* yacc.c:1646  */
    break;

  case 28:
#line 3158 "parser.y" /* yacc.c:1646  */
    {
	setup_program_start ();
	setup_from_identification = 1;
  }
#line 11764 "parser.c" /* yacc.c:1646  */
    break;

  case 31:
#line 3171 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_id = 1;
  }
#line 11772 "parser.c" /* yacc.c:1646  */
    break;

  case 32:
#line 3175 "parser.y" /* yacc.c:1646  */
    {
	if (setup_program ((yyvsp[-1]), (yyvsp[0]), COB_MODULE_TYPE_PROGRAM)) {
		YYABORT;
	}

	setup_prototype ((yyvsp[-1]), (yyvsp[0]), COB_MODULE_TYPE_PROGRAM, 1);
  }
#line 11784 "parser.c" /* yacc.c:1646  */
    break;

  case 33:
#line 3183 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
#line 11793 "parser.c" /* yacc.c:1646  */
    break;

  case 34:
#line 3191 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_id = 1;
  }
#line 11801 "parser.c" /* yacc.c:1646  */
    break;

  case 35:
#line 3195 "parser.y" /* yacc.c:1646  */
    {
	if (setup_program ((yyvsp[-2]), (yyvsp[-1]), COB_MODULE_TYPE_FUNCTION)) {
		YYABORT;
	}
	setup_prototype ((yyvsp[-2]), (yyvsp[-1]), COB_MODULE_TYPE_FUNCTION, 1);
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
#line 11814 "parser.c" /* yacc.c:1646  */
    break;

  case 36:
#line 3207 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REFERENCE_P ((yyvsp[0])) && CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
	}
	/*
	  The program name is a key part of defining the current_program, so we
	  mustn't lose it (unlike in undefined_word).
	*/
	(yyval) = (yyvsp[0]);
  }
#line 11829 "parser.c" /* yacc.c:1646  */
    break;

  case 37:
#line 3218 "parser.y" /* yacc.c:1646  */
    {
	cb_trim_program_id ((yyvsp[0]));
  }
#line 11837 "parser.c" /* yacc.c:1646  */
    break;

  case 39:
#line 3226 "parser.y" /* yacc.c:1646  */
    {
	cb_trim_program_id ((yyvsp[0]));
  }
#line 11845 "parser.c" /* yacc.c:1646  */
    break;

  case 40:
#line 3232 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 11851 "parser.c" /* yacc.c:1646  */
    break;

  case 41:
#line 3233 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 11857 "parser.c" /* yacc.c:1646  */
    break;

  case 44:
#line 3242 "parser.y" /* yacc.c:1646  */
    {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
#line 11870 "parser.c" /* yacc.c:1646  */
    break;

  case 45:
#line 3251 "parser.y" /* yacc.c:1646  */
    {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
#line 11883 "parser.c" /* yacc.c:1646  */
    break;

  case 47:
#line 3261 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("CALL prototypes"));
  }
#line 11891 "parser.c" /* yacc.c:1646  */
    break;

  case 50:
#line 3273 "parser.y" /* yacc.c:1646  */
    {
	current_program->flag_initial = 1;
  }
#line 11899 "parser.c" /* yacc.c:1646  */
    break;

  case 51:
#line 3277 "parser.y" /* yacc.c:1646  */
    {
	current_program->flag_recursive = 1;
  }
#line 11907 "parser.c" /* yacc.c:1646  */
    break;

  case 53:
#line 3286 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 11915 "parser.c" /* yacc.c:1646  */
    break;

  case 57:
#line 3306 "parser.y" /* yacc.c:1646  */
    {
/* FIXME: the IBM-compatible ARITHMETIC should only be disabled
          for the specified program (and its nested programs)
   note: ibm-strict.conf has no OPTIONS paragraph, but ibm.conf does */
	cb_arithmetic_osvs = 0;
  }
#line 11926 "parser.c" /* yacc.c:1646  */
    break;

  case 58:
#line 3313 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("STANDARD ARITHMETIC");
  }
#line 11934 "parser.c" /* yacc.c:1646  */
    break;

  case 59:
#line 3317 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("STANDARD-BINARY ARITHMETIC");
  }
#line 11942 "parser.c" /* yacc.c:1646  */
    break;

  case 60:
#line 3321 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("STANDARD-DECIMAL ARITHMETIC");
  }
#line 11950 "parser.c" /* yacc.c:1646  */
    break;

  case 61:
#line 3336 "parser.y" /* yacc.c:1646  */
    {
	default_rounded_mode = cb_int (COB_STORE_ROUND);
  }
#line 11958 "parser.c" /* yacc.c:1646  */
    break;

  case 62:
#line 3340 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		default_rounded_mode = (yyvsp[0]);
	} else {
		default_rounded_mode = cb_int (COB_STORE_ROUND);
	}
  }
#line 11970 "parser.c" /* yacc.c:1646  */
    break;

  case 64:
#line 3352 "parser.y" /* yacc.c:1646  */
    {
	current_program->entry_convention = (yyvsp[0]);
  }
#line 11978 "parser.c" /* yacc.c:1646  */
    break;

  case 65:
#line 3359 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_COBOL);
  }
#line 11986 "parser.c" /* yacc.c:1646  */
    break;

  case 66:
#line 3363 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (0);
  }
#line 11994 "parser.c" /* yacc.c:1646  */
    break;

  case 67:
#line 3367 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_STDCALL);
  }
#line 12002 "parser.c" /* yacc.c:1646  */
    break;

  case 69:
#line 3375 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("INTERMEDIATE ROUNDING");
  }
#line 12010 "parser.c" /* yacc.c:1646  */
    break;

  case 70:
#line 3382 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_AWAY_FROM_ZERO);
  }
#line 12018 "parser.c" /* yacc.c:1646  */
    break;

  case 71:
#line 3386 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_EVEN);
  }
#line 12026 "parser.c" /* yacc.c:1646  */
    break;

  case 72:
#line 3390 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_PROHIBITED);
  }
#line 12034 "parser.c" /* yacc.c:1646  */
    break;

  case 73:
#line 3394 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TRUNCATION);
  }
#line 12042 "parser.c" /* yacc.c:1646  */
    break;

  case 76:
#line 3409 "parser.y" /* yacc.c:1646  */
    {
	header_check |= COBC_HD_ENVIRONMENT_DIVISION;
  }
#line 12050 "parser.c" /* yacc.c:1646  */
    break;

  case 79:
#line 3423 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_CONFIGURATION_SECTION;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "CONFIGURATION SECTION");
	}
  }
#line 12062 "parser.c" /* yacc.c:1646  */
    break;

  case 89:
#line 3454 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_SOURCE_COMPUTER);
	set_conf_section_part (COBC_HD_SOURCE_COMPUTER);
  }
#line 12073 "parser.c" /* yacc.c:1646  */
    break;

  case 94:
#line 3470 "parser.y" /* yacc.c:1646  */
    {
	current_program->flag_debugging = 1;
	needs_debug_item = 1;
	cobc_cs_check = 0;
	cb_build_debug_item ();
  }
#line 12084 "parser.c" /* yacc.c:1646  */
    break;

  case 95:
#line 3482 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_OBJECT_COMPUTER);
	set_conf_section_part (COBC_HD_OBJECT_COMPUTER);
  }
#line 12095 "parser.c" /* yacc.c:1646  */
    break;

  case 96:
#line 3489 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 12103 "parser.c" /* yacc.c:1646  */
    break;

  case 107:
#line 3515 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_memory_size_clause, "MEMORY SIZE");
  }
#line 12111 "parser.c" /* yacc.c:1646  */
    break;

  case 108:
#line 3523 "parser.y" /* yacc.c:1646  */
    {
	current_program->collating_sequence = alphanumeric_collation;
	current_program->collating_sequence_n = national_collation;
  }
#line 12120 "parser.c" /* yacc.c:1646  */
    break;

  case 109:
#line 3531 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = national_collation = NULL;
  }
#line 12128 "parser.c" /* yacc.c:1646  */
    break;

  case 111:
#line 3539 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = (yyvsp[0]);
  }
#line 12136 "parser.c" /* yacc.c:1646  */
    break;

  case 112:
#line 3543 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = (yyvsp[-1]);
	CB_PENDING_X ((yyvsp[0]), "NATIONAL COLLATING SEQUENCE");
	national_collation = (yyvsp[0]);
  }
#line 12146 "parser.c" /* yacc.c:1646  */
    break;

  case 113:
#line 3549 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = (yyvsp[0]);
  }
#line 12154 "parser.c" /* yacc.c:1646  */
    break;

  case 114:
#line 3553 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING_X ((yyvsp[0]), "NATIONAL COLLATING SEQUENCE");
	national_collation = (yyvsp[0]);
  }
#line 12163 "parser.c" /* yacc.c:1646  */
    break;

  case 115:
#line 3559 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = (yyvsp[-4]);
	CB_PENDING_X ((yyvsp[0]), "NATIONAL COLLATING SEQUENCE");
	national_collation = (yyvsp[0]);
  }
#line 12173 "parser.c" /* yacc.c:1646  */
    break;

  case 116:
#line 3566 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING_X ((yyvsp[-4]), "NATIONAL COLLATING SEQUENCE");
	national_collation = (yyvsp[-4]);
	alphanumeric_collation = (yyvsp[0]);
  }
#line 12183 "parser.c" /* yacc.c:1646  */
    break;

  case 117:
#line 3575 "parser.y" /* yacc.c:1646  */
    {
	int segnum;

	if (cb_verify (cb_section_segments, "SEGMENT LIMIT")) {
		segnum = cb_get_int ((yyvsp[0]));
		if (segnum == 0 || segnum > 49) {
			cb_error (_("segment-number must be in range of values 1 to 49"));
			(yyval) = NULL;
		}
	}
	/* Ignore */
  }
#line 12200 "parser.c" /* yacc.c:1646  */
    break;

  case 118:
#line 3591 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->classification) {
		cb_error (_("duplicate CLASSIFICATION clause"));
	} else {
		current_program->classification = (yyvsp[0]);
	}
  }
#line 12212 "parser.c" /* yacc.c:1646  */
    break;

  case 119:
#line 3602 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12220 "parser.c" /* yacc.c:1646  */
    break;

  case 120:
#line 3606 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12228 "parser.c" /* yacc.c:1646  */
    break;

  case 121:
#line 3610 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 12236 "parser.c" /* yacc.c:1646  */
    break;

  case 122:
#line 3614 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 12244 "parser.c" /* yacc.c:1646  */
    break;

  case 125:
#line 3628 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_REPOSITORY);
	set_conf_section_part (COBC_HD_REPOSITORY);
  }
#line 12255 "parser.c" /* yacc.c:1646  */
    break;

  case 126:
#line 3635 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_repository = 0;
  }
#line 12263 "parser.c" /* yacc.c:1646  */
    break;

  case 129:
#line 3644 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 12271 "parser.c" /* yacc.c:1646  */
    break;

  case 132:
#line 3656 "parser.y" /* yacc.c:1646  */
    {
	functions_are_all = 1;
  }
#line 12279 "parser.c" /* yacc.c:1646  */
    break;

  case 133:
#line 3660 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-1]) != cb_error_node) {
		setup_prototype ((yyvsp[-1]), (yyvsp[0]), COB_MODULE_TYPE_FUNCTION, 0);
	}
  }
#line 12289 "parser.c" /* yacc.c:1646  */
    break;

  case 135:
#line 3667 "parser.y" /* yacc.c:1646  */
    {
	  if ((yyvsp[-1]) != cb_error_node
	      && cb_verify (cb_program_prototypes, _("PROGRAM phrase"))) {
		setup_prototype ((yyvsp[-1]), (yyvsp[0]), COB_MODULE_TYPE_PROGRAM, 0);
	}
  }
#line 12300 "parser.c" /* yacc.c:1646  */
    break;

  case 136:
#line 3674 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 12308 "parser.c" /* yacc.c:1646  */
    break;

  case 137:
#line 3681 "parser.y" /* yacc.c:1646  */
    {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, (yyvsp[0]));
  }
#line 12317 "parser.c" /* yacc.c:1646  */
    break;

  case 138:
#line 3686 "parser.y" /* yacc.c:1646  */
    {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, (yyvsp[0]));
  }
#line 12326 "parser.c" /* yacc.c:1646  */
    break;

  case 139:
#line 3697 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_SPECIAL_NAMES);
	set_conf_section_part (COBC_HD_SPECIAL_NAMES);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	}
  }
#line 12341 "parser.c" /* yacc.c:1646  */
    break;

  case 156:
#line 3738 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	check_duplicate = 0;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		save_tree = NULL;
	} else {
		/* lookup system name with special translation
		   note: result in NULL + raised error if not found */
		save_tree = get_system_name_translated ((yyvsp[0]));
	}
  }
#line 12360 "parser.c" /* yacc.c:1646  */
    break;

  case 158:
#line 3757 "parser.y" /* yacc.c:1646  */
    {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_DEVICE_CONSOLE) {
			cb_error_x (save_tree, _("invalid %s clause"), "");
		} else {
			current_program->flag_console_is_crt = 1;
		}
	}
  }
#line 12374 "parser.c" /* yacc.c:1646  */
    break;

  case 159:
#line 3768 "parser.y" /* yacc.c:1646  */
    {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_FEATURE_CONVENTION) {
			cb_error_x (save_tree, _("invalid %s clause"), "SPECIAL NAMES");
		} else if (CB_VALID_TREE ((yyvsp[0]))) {
			CB_SYSTEM_NAME(save_tree)->value = (yyvsp[-2]);
			cb_define ((yyvsp[0]), save_tree);
			CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
					(yyvsp[0]), save_tree);
			/* remove non-standard context-sensitive words when identical to mnemonic */
			if (strcasecmp (CB_NAME((yyvsp[0])), "EXTERN") == 0 ||
			    strcasecmp (CB_NAME((yyvsp[0])), "STDCALL") == 0 ||
			    strcasecmp (CB_NAME((yyvsp[0])), "STATIC") == 0) {
				remove_context_sensitivity (CB_NAME((yyvsp[0])), CB_CS_CALL);
			}
		}
	}
  }
#line 12397 "parser.c" /* yacc.c:1646  */
    break;

  case 160:
#line 3787 "parser.y" /* yacc.c:1646  */
    {
	if (save_tree && CB_VALID_TREE ((yyvsp[-1]))) {
		cb_define ((yyvsp[-1]), save_tree);
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
				(yyvsp[-1]), save_tree);
	}
  }
#line 12409 "parser.c" /* yacc.c:1646  */
    break;

  case 164:
#line 3803 "parser.y" /* yacc.c:1646  */
    {
	  check_on_off_duplicate = 0;
  }
#line 12417 "parser.c" /* yacc.c:1646  */
    break;

  case 165:
#line 3810 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name ((yyvsp[0]), save_tree, (yyvsp[-2]) == cb_int1);
	if (x) {
		if ((yyvsp[-2]) == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, (yyvsp[0]), x);
	}
  }
#line 12436 "parser.c" /* yacc.c:1646  */
    break;

  case 166:
#line 3825 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name ((yyvsp[0]), save_tree, (yyvsp[-2]) == cb_int1);
	if (x) {
		if ((yyvsp[-2]) == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, (yyvsp[0]), x);
	}
  }
#line 12455 "parser.c" /* yacc.c:1646  */
    break;

  case 167:
#line 3845 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		(yyval) = NULL;
	} else {
		/* Returns null on error */
		(yyval) = cb_build_alphabet_name ((yyvsp[0]));
	}
  }
#line 12472 "parser.c" /* yacc.c:1646  */
    break;

  case 168:
#line 3858 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-1])) {
		current_program->alphabet_name_list =
			cb_list_add (current_program->alphabet_name_list, (yyvsp[-1]));
	}
	cobc_cs_check = 0;
  }
#line 12484 "parser.c" /* yacc.c:1646  */
    break;

  case 169:
#line 3869 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if ((yyvsp[-1])) {
		CB_ALPHABET_NAME ((yyvsp[-1]))->alphabet_target = CB_ALPHABET_ALPHANUMERIC;
	}
  }
#line 12495 "parser.c" /* yacc.c:1646  */
    break;

  case 171:
#line 3877 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if ((yyvsp[-1])) {
		CB_ALPHABET_NAME((yyvsp[-1]))->alphabet_target = CB_ALPHABET_NATIONAL;
	}
  }
#line 12506 "parser.c" /* yacc.c:1646  */
    break;

  case 177:
#line 3898 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 12516 "parser.c" /* yacc.c:1646  */
    break;

  case 178:
#line 3904 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 12526 "parser.c" /* yacc.c:1646  */
    break;

  case 179:
#line 3910 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_EBCDIC;
	}
  }
#line 12536 "parser.c" /* yacc.c:1646  */
    break;

  case 180:
#line 3916 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 12546 "parser.c" /* yacc.c:1646  */
    break;

  case 182:
#line 3926 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_PENDING_X ((yyvsp[(-1) - (1)]), "ALPHABET UCS-4");
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_UCS_4;
	}
  }
#line 12557 "parser.c" /* yacc.c:1646  */
    break;

  case 183:
#line 3933 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_PENDING_X ((yyvsp[(-1) - (1)]), "ALPHABET UTF-8");
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_UTF_8;
	}
  }
#line 12568 "parser.c" /* yacc.c:1646  */
    break;

  case 184:
#line 3940 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_PENDING_X ((yyvsp[(-1) - (1)]), "ALPHABET UTF-16");
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_UTF_16;
	}
  }
#line 12579 "parser.c" /* yacc.c:1646  */
    break;

  case 185:
#line 3950 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_NATIVE;
	}
  }
#line 12589 "parser.c" /* yacc.c:1646  */
    break;

  case 186:
#line 3956 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (2)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (2)]))->alphabet_type = CB_ALPHABET_LOCALE;
		CB_ALPHABET_NAME ((yyvsp[(-1) - (2)]))->custom_list = (yyvsp[0]);
		CB_PENDING_X ((yyvsp[(-1) - (2)]), "LOCALE ALPHABET");
	}
  }
#line 12601 "parser.c" /* yacc.c:1646  */
    break;

  case 187:
#line 3964 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_CUSTOM;
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->custom_list = (yyvsp[0]);
	}
  }
#line 12612 "parser.c" /* yacc.c:1646  */
    break;

  case 188:
#line 3974 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 12620 "parser.c" /* yacc.c:1646  */
    break;

  case 189:
#line 3978 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 12628 "parser.c" /* yacc.c:1646  */
    break;

  case 190:
#line 3985 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12636 "parser.c" /* yacc.c:1646  */
    break;

  case 191:
#line 3989 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
  }
#line 12644 "parser.c" /* yacc.c:1646  */
    break;

  case 192:
#line 3993 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[-1]));
  }
#line 12652 "parser.c" /* yacc.c:1646  */
    break;

  case 193:
#line 3997 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 12660 "parser.c" /* yacc.c:1646  */
    break;

  case 194:
#line 4004 "parser.y" /* yacc.c:1646  */
    {
	cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 12668 "parser.c" /* yacc.c:1646  */
    break;

  case 195:
#line 4008 "parser.y" /* yacc.c:1646  */
    {
	cb_list_add ((yyvsp[-3]), (yyvsp[0]));
  }
#line 12676 "parser.c" /* yacc.c:1646  */
    break;

  case 196:
#line 4014 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 12682 "parser.c" /* yacc.c:1646  */
    break;

  case 197:
#line 4015 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 12688 "parser.c" /* yacc.c:1646  */
    break;

  case 198:
#line 4016 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 12694 "parser.c" /* yacc.c:1646  */
    break;

  case 199:
#line 4017 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 12700 "parser.c" /* yacc.c:1646  */
    break;

  case 200:
#line 4018 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_norm_high; }
#line 12706 "parser.c" /* yacc.c:1646  */
    break;

  case 201:
#line 4019 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_norm_low; }
#line 12712 "parser.c" /* yacc.c:1646  */
    break;

  case 202:
#line 4023 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 12718 "parser.c" /* yacc.c:1646  */
    break;

  case 203:
#line 4024 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 12724 "parser.c" /* yacc.c:1646  */
    break;

  case 204:
#line 4032 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else if ((yyvsp[-1])) {
		CB_CHAIN_PAIR (current_program->symbolic_char_list, (yyvsp[-1]), (yyvsp[0]));
	}
  }
#line 12739 "parser.c" /* yacc.c:1646  */
    break;

  case 205:
#line 4046 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12747 "parser.c" /* yacc.c:1646  */
    break;

  case 206:
#line 4050 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12755 "parser.c" /* yacc.c:1646  */
    break;

  case 207:
#line 4058 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12763 "parser.c" /* yacc.c:1646  */
    break;

  case 208:
#line 4065 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12771 "parser.c" /* yacc.c:1646  */
    break;

  case 209:
#line 4069 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
	} else {
		(yyval) = (yyvsp[-1]);
	}
  }
#line 12783 "parser.c" /* yacc.c:1646  */
    break;

  case 210:
#line 4080 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l1;
	cb_tree		l2;

	if (cb_list_length ((yyvsp[-2])) != cb_list_length ((yyvsp[0]))) {
		cb_error (_("invalid %s clause"), "SYMBOLIC");
		(yyval) = NULL;
	} else {
		l1 = (yyvsp[-2]);
		l2 = (yyvsp[0]);
		for (; l1; l1 = CB_CHAIN (l1), l2 = CB_CHAIN (l2)) {
			CB_PURPOSE (l1) = CB_VALUE (l2);
		}
		(yyval) = (yyvsp[-2]);
	}
  }
#line 12804 "parser.c" /* yacc.c:1646  */
    break;

  case 211:
#line 4100 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		(yyval) = NULL;
	} else {
		(yyval) = CB_LIST_INIT ((yyvsp[0]));
	}
  }
#line 12816 "parser.c" /* yacc.c:1646  */
    break;

  case 212:
#line 4108 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		(yyval) = (yyvsp[-1]);
	} else {
		(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
	}
  }
#line 12828 "parser.c" /* yacc.c:1646  */
    break;

  case 213:
#line 4118 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 12834 "parser.c" /* yacc.c:1646  */
    break;

  case 214:
#line 4119 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 12840 "parser.c" /* yacc.c:1646  */
    break;

  case 215:
#line 4128 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	}
	(void)cb_verify (cb_symbolic_constant, "SYMBOLIC CONSTANT");
  }
#line 12854 "parser.c" /* yacc.c:1646  */
    break;

  case 218:
#line 4146 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field *f;
	cb_tree v;

	v = CB_LIST_INIT ((yyvsp[0]));
	f = CB_FIELD (cb_build_constant ((yyvsp[-2]), v));
	f->flag_item_78 = 1;
	f->flag_constant = 1;
	f->flag_is_global = 1;
	f->level = 1;
	f->values = v;
	cb_needs_01 = 1;
	/* Ignore return value */
	(void)cb_validate_78_item (f, 0);
  }
#line 12874 "parser.c" /* yacc.c:1646  */
    break;

  case 219:
#line 4167 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		x = cb_build_class_name ((yyvsp[-4]), (yyvsp[-1]));
		if (x) {
			current_program->class_name_list =
				cb_list_add (current_program->class_name_list, x);
		}
	}
  }
#line 12896 "parser.c" /* yacc.c:1646  */
    break;

  case 220:
#line 4187 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 12902 "parser.c" /* yacc.c:1646  */
    break;

  case 221:
#line 4188 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 12908 "parser.c" /* yacc.c:1646  */
    break;

  case 222:
#line 4193 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12916 "parser.c" /* yacc.c:1646  */
    break;

  case 223:
#line 4197 "parser.y" /* yacc.c:1646  */
    {
	if (CB_TREE_CLASS ((yyvsp[-2])) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P ((yyvsp[-2])) && CB_LITERAL ((yyvsp[-2]))->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (CB_TREE_CLASS ((yyvsp[0])) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P ((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (literal_value ((yyvsp[-2])) <= literal_value ((yyvsp[0]))) {
		(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
	} else {
		(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-2]));
	}
  }
#line 12936 "parser.c" /* yacc.c:1646  */
    break;

  case 225:
#line 4217 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12944 "parser.c" /* yacc.c:1646  */
    break;

  case 226:
#line 4221 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING_X ((yyvsp[0]), "NATIONAL CLASS");
	(yyval) = cb_int0;
  }
#line 12953 "parser.c" /* yacc.c:1646  */
    break;

  case 228:
#line 4230 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING_X ((yyvsp[0]), _("CLASS IS integer IN alphabet-name"));
	(yyval) = (yyvsp[0]);
  }
#line 12962 "parser.c" /* yacc.c:1646  */
    break;

  case 229:
#line 4240 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		cb_tree	l = cb_build_locale_name ((yyvsp[-2]), (yyvsp[0]));
		if (l) {
			current_program->locale_list =
				cb_list_add (current_program->locale_list, l);
		}
	}
  }
#line 12982 "parser.c" /* yacc.c:1646  */
    break;

  case 230:
#line 4261 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		unsigned int	error_ind = 0;

		/* FIXME: actual allowed (depending on dialect), see FR #246 */
		check_repeated ("CURRENCY", SYN_CLAUSE_1, &check_duplicate);

		/* checks of CURRENCY SIGN (being currency string) when separate */
		if ((yyvsp[0])) {
			unsigned int	char_seen = 0;
			unsigned char	*s = CB_LITERAL ((yyvsp[-1]))->data;

			CB_PENDING_X ((yyvsp[-1]), _("separate currency symbol and currency string"));
			while (*s) {
				switch (*s) {
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
				case '+':
				case '-':
				case ',':
				case '.':
				case '*':
					error_ind = 1;
					break;
				case ' ':
					break;
				default:
					char_seen = 1;
					break;
				}
				s++;
			}
			if (!char_seen) {
				error_ind = 1;
			}
		}
		if (error_ind) {
			cb_error_x ((yyvsp[-1]), _("invalid CURRENCY SIGN '%s'"), (char*)CB_LITERAL ((yyvsp[-1]))->data);
		}
		if ((yyvsp[0])) {
			set_currency_picture_symbol ((yyvsp[0]));
		} else {
			if (!error_ind) {
				set_currency_picture_symbol ((yyvsp[-1]));
			}
		}
	}
  }
#line 13048 "parser.c" /* yacc.c:1646  */
    break;

  case 231:
#line 4327 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 13056 "parser.c" /* yacc.c:1646  */
    break;

  case 232:
#line 4331 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 13064 "parser.c" /* yacc.c:1646  */
    break;

  case 233:
#line 4340 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("DECIMAL-POINT", SYN_CLAUSE_2, &check_duplicate);
		current_program->decimal_point = ',';
		current_program->numeric_separator = '.';
	}
  }
#line 13081 "parser.c" /* yacc.c:1646  */
    break;

  case 234:
#line 4359 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		current_program->flag_trailing_separate = 1;
	}
  }
#line 13096 "parser.c" /* yacc.c:1646  */
    break;

  case 235:
#line 4375 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CURSOR", SYN_CLAUSE_3, &check_duplicate);
		current_program->cursor_pos = (yyvsp[0]);
	}
  }
#line 13112 "parser.c" /* yacc.c:1646  */
    break;

  case 236:
#line 4393 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CRT STATUS", SYN_CLAUSE_4, &check_duplicate);
		current_program->crt_status = (yyvsp[0]);
	}
  }
#line 13128 "parser.c" /* yacc.c:1646  */
    break;

  case 237:
#line 4411 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("SCREEN CONTROL", SYN_CLAUSE_5, &check_duplicate);
		CB_PENDING ("SCREEN CONTROL");
	}
  }
#line 13144 "parser.c" /* yacc.c:1646  */
    break;

  case 238:
#line 4428 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("EVENT STATUS", SYN_CLAUSE_6, &check_duplicate);
		CB_PENDING ("EVENT STATUS");
	}
  }
#line 13160 "parser.c" /* yacc.c:1646  */
    break;

  case 241:
#line 4452 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_INPUT_OUTPUT_SECTION;
  }
#line 13169 "parser.c" /* yacc.c:1646  */
    break;

  case 243:
#line 4462 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_FILE_CONTROL;
  }
#line 13179 "parser.c" /* yacc.c:1646  */
    break;

  case 246:
#line 4475 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_FILE_CONTROL, 0);
	check_duplicate = 0;
	if (CB_VALID_TREE ((yyvsp[0]))) {
		/* Build new file */
		current_file = build_file ((yyvsp[0]));
		current_file->optional = CB_INTEGER ((yyvsp[-1]))->val;

		/* Add file to current program list */
		CB_ADD_TO_CHAIN (CB_TREE (current_file),
				 current_program->file_list);
	} else if (current_program->file_list) {
		current_program->file_list
			= CB_CHAIN (current_program->file_list);
	}
	key_type = NO_KEY;
  }
#line 13203 "parser.c" /* yacc.c:1646  */
    break;

  case 247:
#line 4495 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	if (CB_VALID_TREE ((yyvsp[-2]))) {
		if (current_file->organization == COB_ORG_INDEXED
		    && key_type == RELATIVE_KEY) {
			cb_error_x (current_file->key,
				    _("cannot use RELATIVE KEY clause on INDEXED files"));
		} else if (current_file->organization == COB_ORG_RELATIVE
			   && key_type == RECORD_KEY) {
			cb_error_x (current_file->key,
				    _("cannot use RECORD KEY clause on RELATIVE files"));
		}

		validate_file (current_file, (yyvsp[-2]));
	}
  }
#line 13224 "parser.c" /* yacc.c:1646  */
    break;

  case 249:
#line 4516 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 13232 "parser.c" /* yacc.c:1646  */
    break;

  case 251:
#line 4523 "parser.y" /* yacc.c:1646  */
    {
	/* reset context-sensitive words for next clauses */
	cobc_cs_check = CB_CS_SELECT;
  }
#line 13241 "parser.c" /* yacc.c:1646  */
    break;

  case 266:
#line 4568 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
  }
#line 13250 "parser.c" /* yacc.c:1646  */
    break;

  case 267:
#line 4573 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_fileid = 1;
	}
  }
#line 13263 "parser.c" /* yacc.c:1646  */
    break;

  case 268:
#line 4582 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_fileid = 1;
	}
  }
#line 13277 "parser.c" /* yacc.c:1646  */
    break;

  case 269:
#line 4592 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_ext_assign = 0;
		current_file->assign =
			cb_build_alphanumeric_literal ("stdout", (size_t)6);
		current_file->special = COB_SELECT_STDOUT;
	}
  }
#line 13293 "parser.c" /* yacc.c:1646  */
    break;

  case 270:
#line 4604 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_ext_assign = 0;
		current_file->assign =
			cb_build_alphanumeric_literal ("stdin", (size_t)5);
		current_file->special = COB_SELECT_STDIN;
	}
  }
#line 13309 "parser.c" /* yacc.c:1646  */
    break;

  case 271:
#line 4616 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		/* RM/COBOL always expects an assignment name here - we ignore this
		   for PRINTER + PRINTER-1 as ACUCOBOL allows this for using as alias */
		current_file->flag_ext_assign = 0;
		if ((yyvsp[-1]) == cb_int0) {
			current_file->assign =
				cb_build_alphanumeric_literal ("PRINTER", (size_t)7);
		} else if ((yyvsp[-1]) == cb_int1) {
			current_file->assign =
				cb_build_alphanumeric_literal ("PRINTER-1", (size_t)9);
		} else {
			current_file->assign =
				cb_build_alphanumeric_literal ("LPT1", (size_t)4);
		}

	}
  }
#line 13336 "parser.c" /* yacc.c:1646  */
    break;

  case 272:
#line 4647 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 13342 "parser.c" /* yacc.c:1646  */
    break;

  case 273:
#line 4648 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 13348 "parser.c" /* yacc.c:1646  */
    break;

  case 274:
#line 4649 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int4; }
#line 13354 "parser.c" /* yacc.c:1646  */
    break;

  case 287:
#line 4673 "parser.y" /* yacc.c:1646  */
    {
	current_file->flag_line_adv = 1;
  }
#line 13362 "parser.c" /* yacc.c:1646  */
    break;

  case 289:
#line 4681 "parser.y" /* yacc.c:1646  */
    {
	current_file->flag_ext_assign = 1;
  }
#line 13370 "parser.c" /* yacc.c:1646  */
    break;

  case 293:
#line 4694 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 13378 "parser.c" /* yacc.c:1646  */
    break;

  case 296:
#line 4706 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ACCESS", SYN_CLAUSE_2, &check_duplicate);
  }
#line 13386 "parser.c" /* yacc.c:1646  */
    break;

  case 297:
#line 4712 "parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_SEQUENTIAL; }
#line 13392 "parser.c" /* yacc.c:1646  */
    break;

  case 298:
#line 4713 "parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_DYNAMIC; }
#line 13398 "parser.c" /* yacc.c:1646  */
    break;

  case 299:
#line 4714 "parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_RANDOM; }
#line 13404 "parser.c" /* yacc.c:1646  */
    break;

  case 300:
#line 4722 "parser.y" /* yacc.c:1646  */
    {
	struct cb_alt_key *p;
	struct cb_alt_key *l;

	cb_tree composite_key;

	p = cobc_parse_malloc (sizeof (struct cb_alt_key));
	p->key = (yyvsp[-4]);
	p->component_list = NULL;
	if ((yyvsp[-2])) {
		p->duplicates = CB_INTEGER ((yyvsp[-2]))->val;
	} else {
		/* note: we may add a compiler configuration here,
		         as at least ICOBOL defaults to WITH DUPLICATES
		         for ALTERNATE keys if not explicit deactivated
		*/
		p->duplicates = 0;
	}
	p->password = (yyvsp[-1]);
	if ((yyvsp[0])) {
		p->tf_suppress = 1;
		p->char_suppress = CB_INTEGER ((yyvsp[0]))->val;
	} else {
		p->tf_suppress = 0;
	}
	p->next = NULL;

	/* handle split keys */
	if ((yyvsp[-3])) {
		/* generate field (in w-s) for composite-key */
		composite_key = cb_build_field((yyvsp[-4]));
		if (composite_key == cb_error_node) {
			YYERROR;
		} else {
			composite_key->category = CB_CATEGORY_ALPHANUMERIC;
			((struct cb_field *)composite_key)->count = 1;
			p->key = cb_build_field_reference((struct cb_field *)composite_key, NULL);
			p->component_list = key_component_list;
		}
	}

	/* add to the end of list */
	if (current_file->alt_key_list == NULL) {
		current_file->alt_key_list = p;
	} else {
		l = current_file->alt_key_list;
		for (; l->next; l = l->next) { ; }
		l->next = p;
	}
  }
#line 13459 "parser.c" /* yacc.c:1646  */
    break;

  case 301:
#line 4776 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 13467 "parser.c" /* yacc.c:1646  */
    break;

  case 303:
#line 4784 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("PASSWORD clause");
  }
#line 13475 "parser.c" /* yacc.c:1646  */
    break;

  case 304:
#line 4788 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 13483 "parser.c" /* yacc.c:1646  */
    break;

  case 305:
#line 4809 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 13491 "parser.c" /* yacc.c:1646  */
    break;

  case 306:
#line 4813 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (literal_value ((yyvsp[0])));
  }
#line 13499 "parser.c" /* yacc.c:1646  */
    break;

  case 307:
#line 4817 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (literal_value ((yyvsp[0])));
  }
#line 13507 "parser.c" /* yacc.c:1646  */
    break;

  case 308:
#line 4827 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLLATING", SYN_CLAUSE_3, &check_duplicate);
	current_file->collating_sequence = alphanumeric_collation;
	current_file->collating_sequence_n = national_collation;
	CB_PENDING ("FILE COLLATING SEQUENCE");
  }
#line 13518 "parser.c" /* yacc.c:1646  */
    break;

  case 309:
#line 4837 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = national_collation = NULL;
  }
#line 13526 "parser.c" /* yacc.c:1646  */
    break;

  case 311:
#line 4845 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = (yyvsp[0]);
  }
#line 13534 "parser.c" /* yacc.c:1646  */
    break;

  case 312:
#line 4849 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = (yyvsp[-1]);
	CB_PENDING_X ((yyvsp[0]), "NATIONAL COLLATING SEQUENCE");
	national_collation = (yyvsp[0]);
  }
#line 13544 "parser.c" /* yacc.c:1646  */
    break;

  case 313:
#line 4855 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = (yyvsp[0]);
  }
#line 13552 "parser.c" /* yacc.c:1646  */
    break;

  case 314:
#line 4859 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING_X ((yyvsp[0]), "NATIONAL COLLATING SEQUENCE");
	national_collation = (yyvsp[0]);
  }
#line 13561 "parser.c" /* yacc.c:1646  */
    break;

  case 315:
#line 4865 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = (yyvsp[-4]);
	CB_PENDING_X ((yyvsp[0]), "NATIONAL COLLATING SEQUENCE");
	national_collation = (yyvsp[0]);
  }
#line 13571 "parser.c" /* yacc.c:1646  */
    break;

  case 316:
#line 4872 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING_X ((yyvsp[-4]), "NATIONAL COLLATING SEQUENCE");
	national_collation = (yyvsp[-4]);
	alphanumeric_collation = (yyvsp[0]);
  }
#line 13581 "parser.c" /* yacc.c:1646  */
    break;

  case 317:
#line 4881 "parser.y" /* yacc.c:1646  */
    {
	/* note: both entries must be resolved later on
	   and also attached to the correct key later, so just store in a list here: */
	current_file->collating_sequence_keys =
		cb_list_add(current_file->collating_sequence_keys, CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-2])));
	CB_PENDING ("KEY COLLATING SEQUENCE");
  }
#line 13593 "parser.c" /* yacc.c:1646  */
    break;

  case 318:
#line 4892 "parser.y" /* yacc.c:1646  */
    {
	if (CB_ALPHABET_NAME_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not an alphabet-name"),
			cb_name ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 13607 "parser.c" /* yacc.c:1646  */
    break;

  case 319:
#line 4907 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("STATUS", SYN_CLAUSE_4, &check_duplicate);
	current_file->file_status = (yyvsp[-1]);
	if ((yyvsp[0])) {
		/* add a compiler configuration if either */
		if (cb_std_define != CB_STD_IBM
		 && cb_std_define != CB_STD_MVS
		 && !cb_relaxed_syntax_checks) {
			cb_verify (CB_UNCONFORMABLE, "VSAM STATUS");
		} else {
			cb_warning (warningopt,
				_("%s ignored"), "VSAM STATUS");
		}
	}
  }
#line 13627 "parser.c" /* yacc.c:1646  */
    break;

  case 323:
#line 4933 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOCK", SYN_CLAUSE_5, &check_duplicate);
  }
#line 13635 "parser.c" /* yacc.c:1646  */
    break;

  case 325:
#line 4941 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MANUAL;
  }
#line 13643 "parser.c" /* yacc.c:1646  */
    break;

  case 326:
#line 4945 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_AUTOMATIC;
  }
#line 13651 "parser.c" /* yacc.c:1646  */
    break;

  case 327:
#line 4949 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_EXCLUSIVE;
  }
#line 13659 "parser.c" /* yacc.c:1646  */
    break;

  case 330:
#line 4958 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
  }
#line 13667 "parser.c" /* yacc.c:1646  */
    break;

  case 331:
#line 4962 "parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
  }
#line 13675 "parser.c" /* yacc.c:1646  */
    break;

  case 334:
#line 4973 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("WITH ROLLBACK");
  }
#line 13683 "parser.c" /* yacc.c:1646  */
    break;

  case 336:
#line 4980 "parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization == COB_ORG_INDEXED) {
		current_file->lock_mode |= COB_LOCK_EXCLUSIVE;
		/* TODO: pass extra flag to fileio */
		CB_PENDING ("WITH MASS-UPDATE");
	} else {
		cb_error (_("%s only valid with ORGANIZATION %s"), "MASS-UPDATE", "INDEXED");
	}
  }
#line 13697 "parser.c" /* yacc.c:1646  */
    break;

  case 339:
#line 5001 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_INDEXED, "INDEXED");
	current_file->organization = COB_ORG_INDEXED;
  }
#line 13707 "parser.c" /* yacc.c:1646  */
    break;

  case 340:
#line 5007 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_SEQUENTIAL, "SEQUENTIAL");
	current_file->organization = COB_ORG_SEQUENTIAL;
  }
#line 13717 "parser.c" /* yacc.c:1646  */
    break;

  case 341:
#line 5013 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_RELATIVE, "RELATIVE");
	current_file->organization = COB_ORG_RELATIVE;
  }
#line 13727 "parser.c" /* yacc.c:1646  */
    break;

  case 342:
#line 5019 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_LINE_SEQUENTIAL,
						"LINE SEQUENTIAL");
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
  }
#line 13738 "parser.c" /* yacc.c:1646  */
    break;

  case 343:
#line 5032 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PADDING", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_padding_character_clause, "PADDING CHARACTER");
  }
#line 13747 "parser.c" /* yacc.c:1646  */
    break;

  case 344:
#line 5042 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RECORD DELIMITER", SYN_CLAUSE_8, &check_duplicate);
	current_file->flag_delimiter = 1;
  }
#line 13756 "parser.c" /* yacc.c:1646  */
    break;

  case 346:
#line 5051 "parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER %s only allowed with SEQUENTIAL files"),
			  "STANDARD-1");
	}

	if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))) {
		cb_warning (warningopt,
			    _("%s ignored"), "RECORD DELIMITER STANDARD-1");
	}
  }
#line 13772 "parser.c" /* yacc.c:1646  */
    break;

  case 347:
#line 5063 "parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL
	    && current_file->organization != COB_ORG_LINE_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER %s only allowed with (LINE) SEQUENTIAL files"),
			  "LINE-SEQUENTIAL");
	}

	if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))
	    && cb_verify (cb_sequential_delimiters, _("LINE-SEQUENTIAL phrase"))) {
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	}
  }
#line 13789 "parser.c" /* yacc.c:1646  */
    break;

  case 348:
#line 5076 "parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER %s only allowed with SEQUENTIAL files"),
			  "BINARY-SEQUENTIAL");
	}

	if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))
	    && cb_verify (cb_sequential_delimiters, _("BINARY-SEQUENTIAL phrase"))) {
		current_file->organization = COB_ORG_SEQUENTIAL;
	}
  }
#line 13805 "parser.c" /* yacc.c:1646  */
    break;

  case 349:
#line 5088 "parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL
	    && current_file->organization != COB_ORG_LINE_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER clause only allowed with (LINE) SEQUENTIAL files"));
	}

	if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))) {
		cb_warning (warningopt,
			    _("Phrase in RECORD DELIMITER not recognized; will be ignored."));
	}
  }
#line 13821 "parser.c" /* yacc.c:1646  */
    break;

  case 350:
#line 5105 "parser.y" /* yacc.c:1646  */
    {
	cb_tree composite_key;

	check_repeated ("RECORD KEY", SYN_CLAUSE_9, &check_duplicate);
	current_file->key = (yyvsp[-3]);
	key_type = RECORD_KEY;

	/* handle split keys */
	if ((yyvsp[-2])) {
		/* generate field (in w-s) for composite-key */
		composite_key = cb_build_field ((yyvsp[-3]));
		if (composite_key == cb_error_node) {
			YYERROR;
		} else {
			composite_key->category = CB_CATEGORY_ALPHANUMERIC;
			((struct cb_field *)composite_key)->count = 1;
			current_file->key = cb_build_field_reference ((struct cb_field *)composite_key, NULL);
			current_file->component_list = key_component_list;
		}
	}
	current_file->password = (yyvsp[-1]);
	if ((yyvsp[0])) {
		/* note: we *may* add a compiler configuration here,
		         as most dialects do not allow this clause
		         on primary keys */
		if (CB_INTEGER ((yyvsp[0]))->val) {
			/* note: see ACUCOBOL docs for implementation notes, including [RE]WRITE rules
			         and "if the underlying (file) system does not support them OPEN
					 result in (sucessfull) io-status 0M" */
			CB_PENDING (_("DUPLICATES for primary keys"));
		};

	}
  }
#line 13860 "parser.c" /* yacc.c:1646  */
    break;

  case 351:
#line 5143 "parser.y" /* yacc.c:1646  */
    {
  	(yyval) = NULL;
  }
#line 13868 "parser.c" /* yacc.c:1646  */
    break;

  case 352:
#line 5147 "parser.y" /* yacc.c:1646  */
    {
  	(yyval) = cb_int0;
  }
#line 13876 "parser.c" /* yacc.c:1646  */
    break;

  case 355:
#line 5158 "parser.y" /* yacc.c:1646  */
    {
	key_component_list = NULL;
  }
#line 13884 "parser.c" /* yacc.c:1646  */
    break;

  case 358:
#line 5168 "parser.y" /* yacc.c:1646  */
    {
	struct cb_key_component *c;
	struct cb_key_component *comp = cobc_main_malloc (sizeof(struct cb_key_component));
	comp->next = NULL;
	comp->component = (yyvsp[0]);
	if (key_component_list == NULL) {
		key_component_list = comp;
	} else {
		for (c = key_component_list; c->next != NULL; c = c->next);
		c->next = comp;
	}
  }
#line 13901 "parser.c" /* yacc.c:1646  */
    break;

  case 359:
#line 5186 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RELATIVE KEY", SYN_CLAUSE_10, &check_duplicate);
	current_file->key = (yyvsp[0]);
	key_type = RELATIVE_KEY;
  }
#line 13911 "parser.c" /* yacc.c:1646  */
    break;

  case 360:
#line 5197 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RESERVE", SYN_CLAUSE_11, &check_duplicate);
  }
#line 13919 "parser.c" /* yacc.c:1646  */
    break;

  case 363:
#line 5211 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SHARING", SYN_CLAUSE_12, &check_duplicate);
	current_file->sharing = (yyvsp[0]);
  }
#line 13928 "parser.c" /* yacc.c:1646  */
    break;

  case 364:
#line 5218 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 13934 "parser.c" /* yacc.c:1646  */
    break;

  case 365:
#line 5219 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 13940 "parser.c" /* yacc.c:1646  */
    break;

  case 366:
#line 5220 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 13946 "parser.c" /* yacc.c:1646  */
    break;

  case 369:
#line 5231 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present(COBC_HD_ENVIRONMENT_DIVISION,
				 COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_I_O_CONTROL;
}
#line 13956 "parser.c" /* yacc.c:1646  */
    break;

  case 372:
#line 5241 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 13964 "parser.c" /* yacc.c:1646  */
    break;

  case 378:
#line 5261 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	switch (CB_INTEGER ((yyvsp[-3]))->val) {
	case 0:
		/* SAME AREA */
		break;
	case 1:
		/* SAME RECORD */
		for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l))) {
				CB_FILE (cb_ref (CB_VALUE (l)))->same_clause = same_area;
			}
		}
		same_area++;
		break;
	case 2:
		/* SAME SORT-MERGE */
		break;
	}
  }
#line 13993 "parser.c" /* yacc.c:1646  */
    break;

  case 379:
#line 5288 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 13999 "parser.c" /* yacc.c:1646  */
    break;

  case 380:
#line 5289 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 14005 "parser.c" /* yacc.c:1646  */
    break;

  case 381:
#line 5290 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 14011 "parser.c" /* yacc.c:1646  */
    break;

  case 382:
#line 5291 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 14017 "parser.c" /* yacc.c:1646  */
    break;

  case 383:
#line 5298 "parser.y" /* yacc.c:1646  */
    {
	current_program->apply_commit = (yyvsp[0]);
	CB_PENDING("APPLY COMMIT");
  }
#line 14026 "parser.c" /* yacc.c:1646  */
    break;

  case 384:
#line 5308 "parser.y" /* yacc.c:1646  */
    {
	/* Fake for TAPE */
	cobc_cs_check = CB_CS_ASSIGN;
  }
#line 14035 "parser.c" /* yacc.c:1646  */
    break;

  case 385:
#line 5313 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	cb_verify (cb_multiple_file_tape_clause, "MULTIPLE FILE TAPE");
	cobc_cs_check = 0;
  }
#line 14047 "parser.c" /* yacc.c:1646  */
    break;

  case 391:
#line 5342 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_WORKING;
  }
#line 14055 "parser.c" /* yacc.c:1646  */
    break;

  case 394:
#line 5355 "parser.y" /* yacc.c:1646  */
    {
	header_check |= COBC_HD_DATA_DIVISION;
  }
#line 14063 "parser.c" /* yacc.c:1646  */
    break;

  case 396:
#line 5364 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_FILE_SECTION;
  }
#line 14073 "parser.c" /* yacc.c:1646  */
    break;

  case 399:
#line 5378 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE (current_file)) {
		if (CB_VALID_TREE ((yyvsp[0]))) {
			/* Do not keep Record if this is really a report */
			if (!current_file->reports) {
				finalize_file (current_file, CB_FIELD ((yyvsp[0])));
			}
		} else if (!current_file->reports) {
			cb_error (_("RECORD description missing or invalid"));
		}
	}
  }
#line 14090 "parser.c" /* yacc.c:1646  */
    break;

  case 400:
#line 5396 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION,
			       COBC_HD_FILE_SECTION, 0, 0);
	check_duplicate = 0;
	if (CB_INVALID_TREE ((yyvsp[0]))) {
		current_file = NULL;
		YYERROR;
	}
	current_file = CB_FILE (cb_ref ((yyvsp[0])));
	if (CB_VALID_TREE (current_file)) {
		if ((yyvsp[-1]) == cb_int1) {
			current_file->organization = COB_ORG_SORT;
		}
		/* note: this is a HACK and should be moved */
		if (current_file->flag_finalized) {
			cb_error_x ((yyvsp[0]), _("duplicate file description for %s"),
				cb_name (CB_TREE (current_file)));
		}
	}
  }
#line 14116 "parser.c" /* yacc.c:1646  */
    break;

  case 402:
#line 5419 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 14124 "parser.c" /* yacc.c:1646  */
    break;

  case 403:
#line 5426 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 14132 "parser.c" /* yacc.c:1646  */
    break;

  case 404:
#line 5430 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 14140 "parser.c" /* yacc.c:1646  */
    break;

  case 407:
#line 5441 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL", SYN_CLAUSE_1, &check_duplicate);
#if	0	/* RXWRXW - Global/External */
	if (current_file->flag_global) {
		cb_error (_("file cannot have both EXTERNAL and GLOBAL clauses"));
	}
#endif
	current_file->flag_external = 1;
  }
#line 14154 "parser.c" /* yacc.c:1646  */
    break;

  case 408:
#line 5451 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_2, &check_duplicate);
#if	0	/* RXWRXW - Global/External */
	if (current_file->flag_external) {
		cb_error (_("file cannot have both EXTERNAL and GLOBAL clauses"));
	}
#endif
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else {
		current_file->flag_global = 1;
		current_program->flag_file_global = 1;
	}
  }
#line 14173 "parser.c" /* yacc.c:1646  */
    break;

  case 418:
#line 5481 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLOCK", SYN_CLAUSE_3, &check_duplicate);
	/* ignore */
  }
#line 14182 "parser.c" /* yacc.c:1646  */
    break;

  case 422:
#line 5494 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (warningopt, _("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		current_file->record_max = cb_get_int ((yyvsp[-1]));
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			cb_error (_("RECORD clause invalid"));
		}
		if (current_file->organization == COB_ORG_INDEXED) {
			if (current_file->record_max > MAX_FD_RECORD_IDX)  {
				current_file->record_max = MAX_FD_RECORD_IDX;
				cb_error (_("RECORD size (IDX) exceeds maximum allowed (%d)"),
					  MAX_FD_RECORD_IDX);
			}
		} else if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
		}
	}
  }
#line 14210 "parser.c" /* yacc.c:1646  */
    break;

  case 423:
#line 5518 "parser.y" /* yacc.c:1646  */
    {
	int	error_ind = 0;

	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (warningopt, _("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		current_file->record_min = cb_get_int ((yyvsp[-3]));
		current_file->record_max = cb_get_int ((yyvsp[-1]));
		if (current_file->record_min < 0)  {
			current_file->record_min = 0;
			error_ind = 1;
		}
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			error_ind = 1;
		}
		if (current_file->organization == COB_ORG_INDEXED) {
			if (current_file->record_max > MAX_FD_RECORD_IDX)  {
				current_file->record_max = MAX_FD_RECORD_IDX;
				cb_error (_("RECORD size (IDX) exceeds maximum allowed (%d)"),
					  MAX_FD_RECORD_IDX);
				error_ind = 1;
			}
		} else if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
			error_ind = 1;
		}
		if (current_file->record_max <= current_file->record_min)  {
			error_ind = 1;
		}
		if (error_ind) {
			cb_error (_("RECORD clause invalid"));
		}
	}
  }
#line 14253 "parser.c" /* yacc.c:1646  */
    break;

  case 424:
#line 5558 "parser.y" /* yacc.c:1646  */
    {
	int	error_ind = 0;

	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	current_file->record_min = (yyvsp[-3]) ? cb_get_int ((yyvsp[-3])) : 0;
	current_file->record_max = (yyvsp[-2]) ? cb_get_int ((yyvsp[-2])) : 0;
	current_file->flag_check_record_varying_limits =
		current_file->record_min == 0 || current_file->record_max == 0;
	if ((yyvsp[-3]) && current_file->record_min < 0)  {
		current_file->record_min = 0;
		error_ind = 1;
	}
	if ((yyvsp[-2]) && current_file->record_max < 1)  {
		current_file->record_max = 1;
		error_ind = 1;
	}
	if ((yyvsp[-2])) {
		if (current_file->organization == COB_ORG_INDEXED) {
			if (current_file->record_max > MAX_FD_RECORD_IDX)  {
				current_file->record_max = MAX_FD_RECORD_IDX;
				cb_error (_("RECORD size (IDX) exceeds maximum allowed (%d)"),
					  MAX_FD_RECORD_IDX);
				error_ind = 1;
			}
		} else if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
			error_ind = 1;
		}
	}
	if (((yyvsp[-3]) || (yyvsp[-2])) && current_file->record_max <= current_file->record_min)  {
		error_ind = 1;
	}
	if (error_ind) {
		cb_error (_("RECORD clause invalid"));
	}
  }
#line 14296 "parser.c" /* yacc.c:1646  */
    break;

  case 426:
#line 5600 "parser.y" /* yacc.c:1646  */
    {
	current_file->record_depending = (yyvsp[0]);
  }
#line 14304 "parser.c" /* yacc.c:1646  */
    break;

  case 427:
#line 5606 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14310 "parser.c" /* yacc.c:1646  */
    break;

  case 428:
#line 5607 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14316 "parser.c" /* yacc.c:1646  */
    break;

  case 429:
#line 5611 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14322 "parser.c" /* yacc.c:1646  */
    break;

  case 430:
#line 5612 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14328 "parser.c" /* yacc.c:1646  */
    break;

  case 431:
#line 5620 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LABEL", SYN_CLAUSE_5, &check_duplicate);
	cb_verify (cb_label_records_clause, "LABEL RECORDS");
  }
#line 14337 "parser.c" /* yacc.c:1646  */
    break;

  case 432:
#line 5631 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
  }
#line 14346 "parser.c" /* yacc.c:1646  */
    break;

  case 433:
#line 5636 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
	if (!current_file->assign) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	}
  }
#line 14358 "parser.c" /* yacc.c:1646  */
    break;

  case 438:
#line 5659 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("DATA", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_data_records_clause, "DATA RECORDS");
  }
#line 14367 "parser.c" /* yacc.c:1646  */
    break;

  case 439:
#line 5671 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINAGE", SYN_CLAUSE_8, &check_duplicate);
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("LINAGE clause with wrong file type"));
	} else {
		current_file->linage = (yyvsp[-2]);
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		if (current_linage == 0) {
			linage_file = current_file;
		}
		current_linage++;
	}
  }
#line 14386 "parser.c" /* yacc.c:1646  */
    break;

  case 445:
#line 5699 "parser.y" /* yacc.c:1646  */
    {
	current_file->latfoot = (yyvsp[0]);
  }
#line 14394 "parser.c" /* yacc.c:1646  */
    break;

  case 446:
#line 5706 "parser.y" /* yacc.c:1646  */
    {
	current_file->lattop = (yyvsp[0]);
  }
#line 14402 "parser.c" /* yacc.c:1646  */
    break;

  case 447:
#line 5713 "parser.y" /* yacc.c:1646  */
    {
	current_file->latbot = (yyvsp[0]);
  }
#line 14410 "parser.c" /* yacc.c:1646  */
    break;

  case 448:
#line 5722 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	check_repeated ("RECORDING", SYN_CLAUSE_9, &check_duplicate);
	/* ignore */
  }
#line 14420 "parser.c" /* yacc.c:1646  */
    break;

  case 453:
#line 5735 "parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORDING MODE U or S can only be used with RECORD SEQUENTIAL files"));
	}
  }
#line 14430 "parser.c" /* yacc.c:1646  */
    break;

  case 456:
#line 5751 "parser.y" /* yacc.c:1646  */
    {
	struct cb_alphabet_name	*al;

	check_repeated ("CODE SET", SYN_CLAUSE_10, &check_duplicate);

	if (CB_VALID_TREE ((yyvsp[-1]))) {
		al = CB_ALPHABET_NAME (cb_ref ((yyvsp[-1])));
		switch (al->alphabet_type) {
#ifdef	COB_EBCDIC_MACHINE
		case CB_ALPHABET_ASCII:
#else
		case CB_ALPHABET_EBCDIC:
#endif
		case CB_ALPHABET_CUSTOM:
			current_file->code_set = al;
			break;
		default:
			if (CB_VALID_TREE ((yyvsp[-1]))) {
				cb_warning_x (warningopt, (yyvsp[-1]), _("ignoring CODE-SET '%s'"),
						  cb_name ((yyvsp[-1])));
			}
			break;
		}
	}

	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("CODE-SET clause invalid for file type"));
	}

	if (warningopt) {
		CB_PENDING ("CODE-SET");
	}
  }
#line 14469 "parser.c" /* yacc.c:1646  */
    break;

  case 458:
#line 5789 "parser.y" /* yacc.c:1646  */
    {
	  if (warningopt) {
		  CB_PENDING ("FOR sub-records");
	  }

	  current_file->code_set_items = CB_LIST ((yyvsp[0]));
  }
#line 14481 "parser.c" /* yacc.c:1646  */
    break;

  case 459:
#line 5802 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REPORT", SYN_CLAUSE_11, &check_duplicate);
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("REPORT clause with wrong file type"));
	} else {
		current_file->reports = (yyvsp[0]);
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		current_file->flag_line_adv = 1;
	}
  }
#line 14497 "parser.c" /* yacc.c:1646  */
    break;

  case 462:
#line 5822 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[0]))) {
		current_report = build_report ((yyvsp[0]));
		current_report->file = current_file;
		current_program->report_list =
			cb_list_add (current_program->report_list,
				     CB_TREE (current_report));
		if (report_count == 0) {
			report_instance = current_report;
		}
		report_count++;
	}
  }
#line 14515 "parser.c" /* yacc.c:1646  */
    break;

  case 463:
#line 5836 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[0]))) {
		current_report = build_report ((yyvsp[0]));
		current_report->file = current_file;
		current_program->report_list =
			cb_list_add (current_program->report_list,
				     CB_TREE (current_report));
		if (report_count == 0) {
			report_instance = current_report;
		}
		report_count++;
	}
  }
#line 14533 "parser.c" /* yacc.c:1646  */
    break;

  case 465:
#line 5855 "parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_COMMUNICATION;
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_COMMUNICATION_SECTION;
	/* add a compiler configuration if either */
	if (cb_std_define != CB_STD_85
	 && cb_std_define != CB_STD_RM
	 && cb_std_define != CB_STD_GC
	 && !cb_relaxed_syntax_checks) {
		cb_verify (CB_UNCONFORMABLE, "COMMUNICATION SECTION");
	} else if (cb_verify (CB_OBSOLETE, "COMMUNICATION SECTION")) {
		CB_PENDING ("COMMUNICATION SECTION");
	}
  }
#line 14552 "parser.c" /* yacc.c:1646  */
    break;

  case 469:
#line 5879 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE (current_cd)) {
		if (CB_VALID_TREE ((yyvsp[0]))) {
			cb_finalize_cd (current_cd, CB_FIELD ((yyvsp[0])));
		} else if (!current_cd->record) {
			cb_error (_("CD record missing"));
		}
	}
  }
#line 14566 "parser.c" /* yacc.c:1646  */
    break;

  case 470:
#line 5894 "parser.y" /* yacc.c:1646  */
    {
	/* CD internally defines a new file */
	if (CB_VALID_TREE ((yyvsp[0]))) {
		current_cd = cb_build_cd ((yyvsp[0]));

		CB_ADD_TO_CHAIN (CB_TREE (current_cd),
				 current_program->cd_list);
	} else {
		current_cd = NULL;
		/* TO-DO: Is this necessary? */
		if (current_program->cd_list) {
			current_program->cd_list
				= CB_CHAIN (current_program->cd_list);
		}
	}
	check_duplicate = 0;
  }
#line 14588 "parser.c" /* yacc.c:1646  */
    break;

  case 518:
#line 6002 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_WORKING_STORAGE_SECTION;
	current_storage = CB_STORAGE_WORKING;
  }
#line 14598 "parser.c" /* yacc.c:1646  */
    break;

  case 519:
#line 6008 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		CB_FIELD_ADD (current_program->working_storage, CB_FIELD ((yyvsp[0])));
	}
  }
#line 14608 "parser.c" /* yacc.c:1646  */
    break;

  case 520:
#line 6017 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 14616 "parser.c" /* yacc.c:1646  */
    break;

  case 521:
#line 6021 "parser.y" /* yacc.c:1646  */
    {
	current_field = NULL;
	control_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 14627 "parser.c" /* yacc.c:1646  */
    break;

  case 522:
#line 6028 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field *p;

	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	(yyval) = CB_TREE (description_field);
  }
#line 14640 "parser.c" /* yacc.c:1646  */
    break;

  case 528:
#line 6048 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
	save_tree = NULL;
  }
#line 14651 "parser.c" /* yacc.c:1646  */
    break;

  case 529:
#line 6055 "parser.y" /* yacc.c:1646  */
    {
	if (!qualifier) {
		current_field->flag_filler = 1;
	}
	if (!description_field) {
		description_field = current_field;
	}
  }
#line 14664 "parser.c" /* yacc.c:1646  */
    break;

  case 530:
#line 6064 "parser.y" /* yacc.c:1646  */
    {
#if 0 /* works fine without, leads to invalid free otherwise [COB_TREE_DEBUG] */
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-2]));
#endif
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
#if 0 /* CHECKME - *Why* would we want to change the field here? */
	current_field = cb_get_real_field ();
#endif
  }
#line 14682 "parser.c" /* yacc.c:1646  */
    break;

  case 531:
#line 6081 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 14690 "parser.c" /* yacc.c:1646  */
    break;

  case 534:
#line 6093 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_filler ();
	qualifier = NULL;
	keys_list = NULL;
	non_const_word = 0;
  }
#line 14701 "parser.c" /* yacc.c:1646  */
    break;

  case 536:
#line 6104 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	qualifier = (yyvsp[0]);
	keys_list = NULL;
	non_const_word = 0;
  }
#line 14712 "parser.c" /* yacc.c:1646  */
    break;

  case 537:
#line 6114 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 14720 "parser.c" /* yacc.c:1646  */
    break;

  case 538:
#line 6118 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
		(yyval) = NULL;
	} else {
		(yyval) = cb_null;
	}
  }
#line 14733 "parser.c" /* yacc.c:1646  */
    break;

  case 539:
#line 6129 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14739 "parser.c" /* yacc.c:1646  */
    break;

  case 540:
#line 6130 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 14745 "parser.c" /* yacc.c:1646  */
    break;

  case 541:
#line 6132 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 14751 "parser.c" /* yacc.c:1646  */
    break;

  case 542:
#line 6138 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 14759 "parser.c" /* yacc.c:1646  */
    break;

  case 543:
#line 6142 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 14767 "parser.c" /* yacc.c:1646  */
    break;

  case 544:
#line 6148 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 14775 "parser.c" /* yacc.c:1646  */
    break;

  case 545:
#line 6152 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int2;
  }
#line 14783 "parser.c" /* yacc.c:1646  */
    break;

  case 546:
#line 6156 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int4;
  }
#line 14791 "parser.c" /* yacc.c:1646  */
    break;

  case 547:
#line 6160 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (8);
  }
#line 14799 "parser.c" /* yacc.c:1646  */
    break;

  case 548:
#line 6164 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(long));
  }
#line 14807 "parser.c" /* yacc.c:1646  */
    break;

  case 549:
#line 6168 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(void *));
  }
#line 14815 "parser.c" /* yacc.c:1646  */
    break;

  case 550:
#line 6172 "parser.y" /* yacc.c:1646  */
    {
	if (cb_binary_comp_1) {
		(yyval) = cb_int2;
	} else {
		(yyval) = cb_int ((int)sizeof(float));
	}
  }
#line 14827 "parser.c" /* yacc.c:1646  */
    break;

  case 551:
#line 6180 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(float));
  }
#line 14835 "parser.c" /* yacc.c:1646  */
    break;

  case 552:
#line 6184 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(double));
  }
#line 14843 "parser.c" /* yacc.c:1646  */
    break;

  case 553:
#line 6188 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (4);
  }
#line 14851 "parser.c" /* yacc.c:1646  */
    break;

  case 554:
#line 6192 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (8);
  }
#line 14859 "parser.c" /* yacc.c:1646  */
    break;

  case 555:
#line 6196 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (16);
  }
#line 14867 "parser.c" /* yacc.c:1646  */
    break;

  case 556:
#line 6200 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
#line 14879 "parser.c" /* yacc.c:1646  */
    break;

  case 566:
#line 6232 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-4]), (yyvsp[-3]))) {
		YYERROR;
	}

	if (cb_ref ((yyvsp[-1])) != cb_error_node) {
		error_if_invalid_level_for_renames ((yyvsp[-1]));
		current_field->redefines = CB_FIELD (cb_ref ((yyvsp[-1])));
	}

	if ((yyvsp[0])) {
		error_if_invalid_level_for_renames ((yyvsp[0]));
		current_field->rename_thru = CB_FIELD (cb_ref ((yyvsp[0])));
	} else {
		/* If there is no THRU clause, RENAMES acts like REDEFINES. */
		current_field->pic = current_field->redefines->pic;
	}

	cb_validate_renames_item (current_field);
  }
#line 14904 "parser.c" /* yacc.c:1646  */
    break;

  case 567:
#line 6256 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 14912 "parser.c" /* yacc.c:1646  */
    break;

  case 568:
#line 6260 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]) == cb_error_node ? NULL : (yyvsp[0]);
  }
#line 14920 "parser.c" /* yacc.c:1646  */
    break;

  case 569:
#line 6267 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
  }
#line 14930 "parser.c" /* yacc.c:1646  */
    break;

  case 570:
#line 6273 "parser.y" /* yacc.c:1646  */
    {
	cb_validate_88_item (current_field);
  }
#line 14938 "parser.c" /* yacc.c:1646  */
    break;

  case 571:
#line 6280 "parser.y" /* yacc.c:1646  */
    {
	cb_tree x;
	int	level;

	cobc_cs_check = 0;
	level = cb_get_level ((yyvsp[-4]));
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-4]));
	if (level != 1) {
		cb_error (_("CONSTANT item not at 01 level"));
	} else if ((yyvsp[0])) {
		if (cb_verify(cb_constant_01, "01 CONSTANT")) {
			x = cb_build_constant ((yyvsp[-3]), (yyvsp[0]));
			CB_FIELD (x)->flag_item_78 = 1;
			CB_FIELD (x)->flag_constant = 1;
			CB_FIELD (x)->level = 1;
			CB_FIELD (x)->values = (yyvsp[0]);
			cb_needs_01 = 1;
			if ((yyvsp[-1])) {
				CB_FIELD (x)->flag_is_global = 1;
			}
			/* Ignore return value */
			(void)cb_validate_78_item (CB_FIELD (x), 0);
		}
	}
  }
#line 14969 "parser.c" /* yacc.c:1646  */
    break;

  case 572:
#line 6307 "parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
  }
#line 14979 "parser.c" /* yacc.c:1646  */
    break;

  case 573:
#line 6314 "parser.y" /* yacc.c:1646  */
    {
	/* Reset to last non-78 item */
	current_field = cb_validate_78_item (current_field, 0);
  }
#line 14988 "parser.c" /* yacc.c:1646  */
    break;

  case 574:
#line 6322 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 14996 "parser.c" /* yacc.c:1646  */
    break;

  case 575:
#line 6326 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT(cb_build_const_from ((yyvsp[0])));
  }
#line 15004 "parser.c" /* yacc.c:1646  */
    break;

  case 576:
#line 6333 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE (current_field)) {
		current_field->values = (yyvsp[0]);
	}
  }
#line 15014 "parser.c" /* yacc.c:1646  */
    break;

  case 577:
#line 6339 "parser.y" /* yacc.c:1646  */
    {
	current_field->values = CB_LIST_INIT (cb_build_const_start (current_field, (yyvsp[0])));
  }
#line 15022 "parser.c" /* yacc.c:1646  */
    break;

  case 578:
#line 6343 "parser.y" /* yacc.c:1646  */
    {
	current_field->values = CB_LIST_INIT (cb_build_const_next (current_field));
  }
#line 15030 "parser.c" /* yacc.c:1646  */
    break;

  case 579:
#line 6349 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 15036 "parser.c" /* yacc.c:1646  */
    break;

  case 580:
#line 6350 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 15042 "parser.c" /* yacc.c:1646  */
    break;

  case 581:
#line 6354 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 15048 "parser.c" /* yacc.c:1646  */
    break;

  case 582:
#line 6355 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("(", 1); }
#line 15054 "parser.c" /* yacc.c:1646  */
    break;

  case 583:
#line 6356 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal (")", 1); }
#line 15060 "parser.c" /* yacc.c:1646  */
    break;

  case 584:
#line 6357 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("+", 1); }
#line 15066 "parser.c" /* yacc.c:1646  */
    break;

  case 585:
#line 6358 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("-", 1); }
#line 15072 "parser.c" /* yacc.c:1646  */
    break;

  case 586:
#line 6359 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("*", 1); }
#line 15078 "parser.c" /* yacc.c:1646  */
    break;

  case 587:
#line 6360 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("/", 1); }
#line 15084 "parser.c" /* yacc.c:1646  */
    break;

  case 588:
#line 6361 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("&", 1); }
#line 15090 "parser.c" /* yacc.c:1646  */
    break;

  case 589:
#line 6362 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("|", 1); }
#line 15096 "parser.c" /* yacc.c:1646  */
    break;

  case 590:
#line 6363 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("^", 1); }
#line 15102 "parser.c" /* yacc.c:1646  */
    break;

  case 593:
#line 6373 "parser.y" /* yacc.c:1646  */
    {
	save_tree = cb_int0;
  }
#line 15110 "parser.c" /* yacc.c:1646  */
    break;

  case 611:
#line 6403 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REDEFINES", SYN_CLAUSE_1, &check_pic_duplicate);
	if (save_tree != NULL) {
		cb_verify_x ((yyvsp[0]), cb_free_redefines_position,
			     _("REDEFINES clause not following entry-name"));
	}

	current_field->redefines = cb_resolve_redefines (current_field, (yyvsp[0]));
	if (current_field->redefines == NULL) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
		YYERROR;
	}
  }
#line 15129 "parser.c" /* yacc.c:1646  */
    break;

  case 612:
#line 6424 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL", SYN_CLAUSE_2, &check_pic_duplicate);
	if (current_storage != CB_STORAGE_WORKING) {
		cb_error (_("%s not allowed here"), "EXTERNAL");
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "EXTERNAL");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "EXTERNAL");
#if	0	/* RXWRXW - Global/External */
	} else if (current_field->flag_is_global) {
		cb_error (_("%s and %s are mutually exclusive"), "GLOBAL", "EXTERNAL");
#endif
	} else if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "EXTERNAL");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "REDEFINES");
	} else if (current_field->flag_occurs) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
	} else {
		current_field->flag_external = 1;
		current_program->flag_has_external = 1;
	}
  }
#line 15157 "parser.c" /* yacc.c:1646  */
    break;

  case 613:
#line 6451 "parser.y" /* yacc.c:1646  */
    {
	current_field->ename = cb_to_cname (current_field->name);
  }
#line 15165 "parser.c" /* yacc.c:1646  */
    break;

  case 614:
#line 6455 "parser.y" /* yacc.c:1646  */
    {
	current_field->ename = cb_to_cname ((const char *)CB_LITERAL ((yyvsp[0]))->data);
  }
#line 15173 "parser.c" /* yacc.c:1646  */
    break;

  case 617:
#line 6468 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_3, &check_pic_duplicate);
	if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "GLOBAL");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "GLOBAL");
#if	0	/* RXWRXW - Global/External */
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "GLOBAL", "EXTERNAL");
#endif
	} else if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else if (current_storage == CB_STORAGE_LOCAL) {
		cb_error (_("%s not allowed here"), "GLOBAL");
	} else {
		current_field->flag_is_global = 1;
	}
  }
#line 15196 "parser.c" /* yacc.c:1646  */
    break;

  case 618:
#line 6493 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VOLATILE", SYN_CLAUSE_24, &check_pic_duplicate);
	/* note: there is no reason to check current_storage as we only parse
	         volatile_clause and its parent tokens where applicable,
	         same is true for level 66,78,88 */
	/* missing part: always generate and initialize storage */
	CB_UNFINISHED ("VOLATILE");
	current_field->flag_volatile = 1;
	/* TODO: set VOLATILE flag for all parent fields */
  }
#line 15211 "parser.c" /* yacc.c:1646  */
    break;

  case 619:
#line 6511 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PICTURE", SYN_CLAUSE_4, &check_pic_duplicate);
	current_field->pic = CB_PICTURE ((yyvsp[-1]));

	if ((yyvsp[0]) && (yyvsp[0]) != cb_error_node) {
		if (  (current_field->pic->category != CB_CATEGORY_NUMERIC
		    && current_field->pic->category != CB_CATEGORY_NUMERIC_EDITED)
		 || strpbrk (current_field->pic->orig, " CRDB-*") /* the standard seems to forbid also ',' */) {
			cb_error_x ((yyvsp[-1]), _("a locale-format PICTURE string must only consist of '9', '.', '+', 'Z' and the currency-sign"));
		} else {
			/* TODO: check that not in or part of CONSTANT RECORD */
			CB_PENDING_X ((yyvsp[-1]), "locale-format PICTURE");
		}
	}
  }
#line 15231 "parser.c" /* yacc.c:1646  */
    break;

  case 620:
#line 6530 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 15237 "parser.c" /* yacc.c:1646  */
    break;

  case 621:
#line 6532 "parser.y" /* yacc.c:1646  */
    {
	/* $2 -> optional locale-name to be used */
	(yyval) = (yyvsp[0]);
  }
#line 15246 "parser.c" /* yacc.c:1646  */
    break;

  case 623:
#line 6541 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 15254 "parser.c" /* yacc.c:1646  */
    break;

  case 624:
#line 6549 "parser.y" /* yacc.c:1646  */
    {
	if (CB_LOCALE_NAME_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a locale-name"),	cb_name ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 15267 "parser.c" /* yacc.c:1646  */
    break;

  case 627:
#line 6566 "parser.y" /* yacc.c:1646  */
    {
	if (is_reserved_word (CB_NAME ((yyvsp[0])))) {
		cb_error_x ((yyvsp[0]), _("'%s' is not a valid USAGE"), CB_NAME ((yyvsp[0])));
	} else if (is_default_reserved_word (CB_NAME ((yyvsp[0])))) {
		cb_error_x ((yyvsp[0]), _("'%s' is not defined, but is a reserved word in another dialect"),
				CB_NAME ((yyvsp[0])));
	} else {
		cb_error_x ((yyvsp[0]), _("unknown USAGE: %s"), CB_NAME ((yyvsp[0])));
	}
	check_and_set_usage (CB_USAGE_ERROR);
	YYERROR;
  }
#line 15284 "parser.c" /* yacc.c:1646  */
    break;

  case 628:
#line 6579 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_ERROR);
  }
#line 15292 "parser.c" /* yacc.c:1646  */
    break;

  case 629:
#line 6586 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 15300 "parser.c" /* yacc.c:1646  */
    break;

  case 630:
#line 6590 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BIT);
	CB_PENDING ("USAGE BIT");
  }
#line 15309 "parser.c" /* yacc.c:1646  */
    break;

  case 631:
#line 6595 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 15317 "parser.c" /* yacc.c:1646  */
    break;

  case 632:
#line 6599 "parser.y" /* yacc.c:1646  */
    {
	/* see FR #310 */
	CB_PENDING ("USAGE COMP-0");
  }
#line 15326 "parser.c" /* yacc.c:1646  */
    break;

  case 633:
#line 6604 "parser.y" /* yacc.c:1646  */
    {
	current_field->flag_comp_1 = 1;
	if (cb_binary_comp_1) {
		check_and_set_usage (CB_USAGE_SIGNED_SHORT);
	} else {
		check_and_set_usage (CB_USAGE_FLOAT);
	}
  }
#line 15339 "parser.c" /* yacc.c:1646  */
    break;

  case 634:
#line 6613 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_DOUBLE);
  }
#line 15347 "parser.c" /* yacc.c:1646  */
    break;

  case 635:
#line 6617 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_PACKED);
  }
#line 15355 "parser.c" /* yacc.c:1646  */
    break;

  case 636:
#line 6621 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 15363 "parser.c" /* yacc.c:1646  */
    break;

  case 637:
#line 6625 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_COMP_5);
  }
#line 15371 "parser.c" /* yacc.c:1646  */
    break;

  case 638:
#line 6629 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_COMP_6);
  }
#line 15379 "parser.c" /* yacc.c:1646  */
    break;

  case 639:
#line 6633 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_COMP_X);
  }
#line 15387 "parser.c" /* yacc.c:1646  */
    break;

  case 640:
#line 6637 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_COMP_N);
  }
#line 15395 "parser.c" /* yacc.c:1646  */
    break;

  case 641:
#line 6641 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FLOAT);
  }
#line 15403 "parser.c" /* yacc.c:1646  */
    break;

  case 642:
#line 6645 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_DISPLAY);
  }
#line 15411 "parser.c" /* yacc.c:1646  */
    break;

  case 643:
#line 6649 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_INDEX);
  }
#line 15419 "parser.c" /* yacc.c:1646  */
    break;

  case 644:
#line 6653 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_PACKED);
  }
#line 15427 "parser.c" /* yacc.c:1646  */
    break;

  case 645:
#line 6657 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_POINTER);
	current_field->flag_is_pointer = 1;
  }
#line 15436 "parser.c" /* yacc.c:1646  */
    break;

  case 646:
#line 6662 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_PROGRAM_POINTER);
	current_field->flag_is_pointer = 1;
  }
#line 15445 "parser.c" /* yacc.c:1646  */
    break;

  case 647:
#line 6667 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL);
  }
#line 15453 "parser.c" /* yacc.c:1646  */
    break;

  case 648:
#line 6671 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_WINDOW);
  }
#line 15461 "parser.c" /* yacc.c:1646  */
    break;

  case 649:
#line 6675 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_SUBWINDOW);
  }
#line 15469 "parser.c" /* yacc.c:1646  */
    break;

  case 650:
#line 6679 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_FONT);
	CB_PENDING ("HANDLE OF FONT");
  }
#line 15478 "parser.c" /* yacc.c:1646  */
    break;

  case 651:
#line 6684 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_THREAD);
  }
#line 15486 "parser.c" /* yacc.c:1646  */
    break;

  case 652:
#line 6688 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_MENU);
	CB_PENDING ("HANDLE OF MENU");
  }
#line 15495 "parser.c" /* yacc.c:1646  */
    break;

  case 653:
#line 6693 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_VARIANT);
  }
#line 15503 "parser.c" /* yacc.c:1646  */
    break;

  case 654:
#line 6697 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_LM);
	CB_PENDING ("HANDLE OF LAYOUT-MANAGER");
  }
#line 15512 "parser.c" /* yacc.c:1646  */
    break;

  case 655:
#line 6702 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL);
	CB_PENDING ("HANDLE OF control-type");
  }
#line 15521 "parser.c" /* yacc.c:1646  */
    break;

  case 656:
#line 6707 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL);
	cb_error_x ((yyvsp[0]), _("unknown HANDLE type: %s"), CB_NAME ((yyvsp[0])));
  }
#line 15530 "parser.c" /* yacc.c:1646  */
    break;

  case 657:
#line 6712 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_SHORT);
  }
#line 15538 "parser.c" /* yacc.c:1646  */
    break;

  case 658:
#line 6716 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_INT);
  }
#line 15546 "parser.c" /* yacc.c:1646  */
    break;

  case 659:
#line 6720 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_SIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
#endif
  }
#line 15558 "parser.c" /* yacc.c:1646  */
    break;

  case 660:
#line 6728 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
#line 15566 "parser.c" /* yacc.c:1646  */
    break;

  case 661:
#line 6732 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
  }
#line 15574 "parser.c" /* yacc.c:1646  */
    break;

  case 662:
#line 6736 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
  }
#line 15586 "parser.c" /* yacc.c:1646  */
    break;

  case 663:
#line 6744 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_CHAR);
  }
#line 15594 "parser.c" /* yacc.c:1646  */
    break;

  case 664:
#line 6748 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_CHAR);
  }
#line 15602 "parser.c" /* yacc.c:1646  */
    break;

  case 665:
#line 6752 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_SHORT);
  }
#line 15610 "parser.c" /* yacc.c:1646  */
    break;

  case 666:
#line 6756 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
#line 15618 "parser.c" /* yacc.c:1646  */
    break;

  case 667:
#line 6760 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_INT);
  }
#line 15626 "parser.c" /* yacc.c:1646  */
    break;

  case 668:
#line 6764 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
  }
#line 15634 "parser.c" /* yacc.c:1646  */
    break;

  case 669:
#line 6768 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
  }
#line 15642 "parser.c" /* yacc.c:1646  */
    break;

  case 670:
#line 6772 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
  }
#line 15650 "parser.c" /* yacc.c:1646  */
    break;

  case 671:
#line 6776 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_SIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
#endif
  }
#line 15662 "parser.c" /* yacc.c:1646  */
    break;

  case 672:
#line 6784 "parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
  }
#line 15674 "parser.c" /* yacc.c:1646  */
    break;

  case 673:
#line 6792 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_BIN32);
  }
#line 15682 "parser.c" /* yacc.c:1646  */
    break;

  case 674:
#line 6796 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_BIN64);
  }
#line 15690 "parser.c" /* yacc.c:1646  */
    break;

  case 675:
#line 6800 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_BIN128);
  }
#line 15698 "parser.c" /* yacc.c:1646  */
    break;

  case 676:
#line 6804 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_DEC64);
  }
#line 15706 "parser.c" /* yacc.c:1646  */
    break;

  case 677:
#line 6808 "parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_DEC128);
  }
#line 15714 "parser.c" /* yacc.c:1646  */
    break;

  case 678:
#line 6812 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate);
	CB_UNFINISHED ("USAGE NATIONAL");
  }
#line 15723 "parser.c" /* yacc.c:1646  */
    break;

  case 690:
#line 6842 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_clause = 1;
	current_field->flag_sign_separate = ((yyvsp[0]) ? 1 : 0);
	current_field->flag_sign_leading  = 1;
  }
#line 15734 "parser.c" /* yacc.c:1646  */
    break;

  case 691:
#line 6849 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_clause = 1;
	current_field->flag_sign_separate = ((yyvsp[0]) ? 1 : 0);
	current_field->flag_sign_leading  = 0;
  }
#line 15745 "parser.c" /* yacc.c:1646  */
    break;

  case 692:
#line 6863 "parser.y" /* yacc.c:1646  */
    {
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[-4]), (yyvsp[-3]));
  }
#line 15755 "parser.c" /* yacc.c:1646  */
    break;

  case 694:
#line 6872 "parser.y" /* yacc.c:1646  */
    {
	current_field->step_count = cb_get_int ((yyvsp[0]));
  }
#line 15763 "parser.c" /* yacc.c:1646  */
    break;

  case 695:
#line 6882 "parser.y" /* yacc.c:1646  */
    {
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[-4]), (yyvsp[-3]));
  }
#line 15773 "parser.c" /* yacc.c:1646  */
    break;

  case 696:
#line 6889 "parser.y" /* yacc.c:1646  */
    {
	current_field->flag_unbounded = 1;
	if (current_field->parent) {
		current_field->parent->flag_unbounded = 1;
	}
	current_field->depending = (yyvsp[-1]);
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[-6]), cb_int0);
  }
#line 15788 "parser.c" /* yacc.c:1646  */
    break;

  case 697:
#line 6901 "parser.y" /* yacc.c:1646  */
    {
	setup_occurs ();
	current_field->occurs_min = (yyvsp[-3]) ? cb_get_int ((yyvsp[-3])) : 0;
	if ((yyvsp[-2])) {
		current_field->occurs_max = cb_get_int ((yyvsp[-2]));
		if (current_field->occurs_max <= current_field->occurs_min) {
			cb_error (_("OCCURS TO must be greater than OCCURS FROM"));
		}
	} else {
		current_field->occurs_max = 0;
	}
	CB_PENDING ("OCCURS DYNAMIC");
  }
#line 15806 "parser.c" /* yacc.c:1646  */
    break;

  case 698:
#line 6917 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 15812 "parser.c" /* yacc.c:1646  */
    break;

  case 699:
#line 6918 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 15818 "parser.c" /* yacc.c:1646  */
    break;

  case 700:
#line 6922 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 15824 "parser.c" /* yacc.c:1646  */
    break;

  case 701:
#line 6923 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 15830 "parser.c" /* yacc.c:1646  */
    break;

  case 702:
#line 6927 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 15836 "parser.c" /* yacc.c:1646  */
    break;

  case 703:
#line 6928 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 15842 "parser.c" /* yacc.c:1646  */
    break;

  case 705:
#line 6933 "parser.y" /* yacc.c:1646  */
    {
	current_field->depending = (yyvsp[0]);
  }
#line 15850 "parser.c" /* yacc.c:1646  */
    break;

  case 707:
#line 6939 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_index ((yyvsp[0]), cb_zero, 0, current_field);
	CB_FIELD_PTR ((yyval))->index_type = CB_STATIC_INT_INDEX;
  }
#line 15859 "parser.c" /* yacc.c:1646  */
    break;

  case 709:
#line 6947 "parser.y" /* yacc.c:1646  */
    {
	/* current_field->initialized = 1; */
  }
#line 15867 "parser.c" /* yacc.c:1646  */
    break;

  case 712:
#line 6956 "parser.y" /* yacc.c:1646  */
    {
	if (!cb_relaxed_syntax_checks) {
		cb_error (_("INDEXED should follow ASCENDING/DESCENDING"));
	} else {
		cb_warning (warningopt, _("INDEXED should follow ASCENDING/DESCENDING"));
	}
  }
#line 15879 "parser.c" /* yacc.c:1646  */
    break;

  case 716:
#line 6970 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_tree		l;
		struct cb_key	*keys;
		int		i;
		int		nkeys;

		l = (yyvsp[0]);
		nkeys = cb_list_length ((yyvsp[0]));
		keys = cobc_parse_malloc (sizeof (struct cb_key) * nkeys);

		for (i = 0; i < nkeys; i++) {
			keys[i].dir = CB_PURPOSE_INT (l);
			keys[i].key = CB_VALUE (l);
			l = CB_CHAIN (l);
		}
		current_field->keys = keys;
		current_field->nkeys = nkeys;
	}
  }
#line 15904 "parser.c" /* yacc.c:1646  */
    break;

  case 719:
#line 6999 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l, item;
	struct cb_field *field;

	for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = (yyvsp[-3]);
		item = CB_VALUE (l);
		if (item == cb_error_node) {
			continue;
		}
		/* internally reference-modify each of the given keys */
		if (qualifier
#if 0 /* Simon: those are never reference-modified ... */
		  && !CB_REFERENCE(item)->chain
#endif /* the following is perfectly fine and would raise a syntax error
		  if we add the self-reference */
		  && strcasecmp (CB_NAME(item), CB_NAME(qualifier))) {
			/* reference by the OCCURS item */
			CB_REFERENCE(item)->chain = qualifier;
		}
		/* reference all the way up as later fields may have same name */
		for (field = CB_FIELD(cb_ref(qualifier))->parent; field; field = field->parent) {
			if (field->flag_filler) continue;
			CB_REFERENCE(item)->chain = cb_build_reference(field->name);
		}
	}
	keys_list = cb_list_append (keys_list, (yyvsp[0]));
	(yyval) = keys_list;
  }
#line 15938 "parser.c" /* yacc.c:1646  */
    break;

  case 720:
#line 7031 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_ASCENDING); }
#line 15944 "parser.c" /* yacc.c:1646  */
    break;

  case 721:
#line 7032 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_DESCENDING); }
#line 15950 "parser.c" /* yacc.c:1646  */
    break;

  case 724:
#line 7041 "parser.y" /* yacc.c:1646  */
    {
	current_field->index_list = (yyvsp[0]);
  }
#line 15958 "parser.c" /* yacc.c:1646  */
    break;

  case 725:
#line 7047 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 15964 "parser.c" /* yacc.c:1646  */
    break;

  case 726:
#line 7049 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 15970 "parser.c" /* yacc.c:1646  */
    break;

  case 727:
#line 7054 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_index ((yyvsp[0]), cb_int1, 1U, current_field);
	CB_FIELD_PTR ((yyval))->index_type = CB_STATIC_INT_INDEX;
  }
#line 15979 "parser.c" /* yacc.c:1646  */
    break;

  case 728:
#line 7065 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("JUSTIFIED", SYN_CLAUSE_8, &check_pic_duplicate);
	current_field->flag_justified = 1;
  }
#line 15988 "parser.c" /* yacc.c:1646  */
    break;

  case 729:
#line 7076 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SYNCHRONIZED", SYN_CLAUSE_9, &check_pic_duplicate);
	current_field->flag_synchronized = 1;
  }
#line 15997 "parser.c" /* yacc.c:1646  */
    break;

  case 732:
#line 7086 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SYNCHRONIZED RIGHT");
  }
#line 16005 "parser.c" /* yacc.c:1646  */
    break;

  case 733:
#line 7096 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLANK", SYN_CLAUSE_10, &check_pic_duplicate);
	current_field->flag_blank_zero = 1;
  }
#line 16014 "parser.c" /* yacc.c:1646  */
    break;

  case 734:
#line 7107 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BASED", SYN_CLAUSE_11, &check_pic_duplicate);
	if (current_storage == CB_STORAGE_FILE) {
		cb_error (_("%s not allowed here"), "BASED");
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "BASED");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "BASED");
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "EXTERNAL");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "REDEFINES");
	} else if (current_field->flag_any_length) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY LENGTH");
	} else if (current_field->flag_occurs) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else {
		current_field->flag_item_based = 1;
	}
  }
#line 16039 "parser.c" /* yacc.c:1646  */
    break;

  case 735:
#line 7133 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE", SYN_CLAUSE_12, &check_pic_duplicate);
	current_field->values = (yyvsp[0]);
  }
#line 16048 "parser.c" /* yacc.c:1646  */
    break;

  case 737:
#line 7141 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 16054 "parser.c" /* yacc.c:1646  */
    break;

  case 738:
#line 7142 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 16060 "parser.c" /* yacc.c:1646  */
    break;

  case 739:
#line 7146 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0])); }
#line 16066 "parser.c" /* yacc.c:1646  */
    break;

  case 742:
#line 7153 "parser.y" /* yacc.c:1646  */
    {
	if (current_field->level != 88) {
		cb_error (_("FALSE clause only allowed for 88 level"));
	}
	current_field->false_88 = CB_LIST_INIT ((yyvsp[0]));
  }
#line 16077 "parser.c" /* yacc.c:1646  */
    break;

  case 743:
#line 7165 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY LENGTH");
	} else {
		current_field->flag_any_length = 1;
	}
  }
#line 16090 "parser.c" /* yacc.c:1646  */
    break;

  case 744:
#line 7174 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY NUMERIC");
	} else {
		current_field->flag_any_length = 1;
		current_field->flag_any_numeric = 1;
	}
  }
#line 16104 "parser.c" /* yacc.c:1646  */
    break;

  case 745:
#line 7189 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL-FORM", SYN_CLAUSE_2, &check_pic_duplicate);
	CB_PENDING ("EXTERNAL-FORM");
	if (current_storage != CB_STORAGE_WORKING) {
		cb_error (_("%s not allowed here"), "EXTERNAL-FORM");
	} else if (current_field->level != 1) {	/* docs say: at group level */
		cb_error (_("%s only allowed at 01 level"), "EXTERNAL-FORM");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "EXTERNAL-FORM");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s combination not allowed"), "EXTERNAL-FORM", "REDEFINES");
	} else {
		current_field->flag_is_external_form = 1;
	}
  }
#line 16124 "parser.c" /* yacc.c:1646  */
    break;

  case 746:
#line 7212 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("IDENTIFIED BY", SYN_CLAUSE_3, &check_pic_duplicate);
	if (!current_field->flag_is_external_form) {
		CB_PENDING ("EXTERNAL-FORM (IDENTIFIED BY)");
		if (current_storage != CB_STORAGE_WORKING) {
			cb_error (_("%s not allowed here"), "IDENTIFIED BY");
		} else if (!qualifier) {
			cb_error (_("%s requires a data name"), "IDENTIFIED BY");
		} else if (current_field->redefines) {
			cb_error (_("%s and %s combination not allowed"), "IDENTIFIED BY", "REDEFINES");
		}
	}
	current_field->external_form_identifier = (yyvsp[0]);
  }
#line 16143 "parser.c" /* yacc.c:1646  */
    break;

  case 748:
#line 7232 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LOCAL_STORAGE_SECTION;
	current_storage = CB_STORAGE_LOCAL;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "LOCAL-STORAGE");
	}
  }
#line 16156 "parser.c" /* yacc.c:1646  */
    break;

  case 749:
#line 7241 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_program->local_storage = CB_FIELD ((yyvsp[0]));
	}
  }
#line 16166 "parser.c" /* yacc.c:1646  */
    break;

  case 751:
#line 7253 "parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LINKAGE_SECTION;
	current_storage = CB_STORAGE_LINKAGE;
  }
#line 16176 "parser.c" /* yacc.c:1646  */
    break;

  case 752:
#line 7259 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_program->linkage_storage = CB_FIELD ((yyvsp[0]));
	}
  }
#line 16186 "parser.c" /* yacc.c:1646  */
    break;

  case 754:
#line 7270 "parser.y" /* yacc.c:1646  */
    {
	header_check |= COBC_HD_REPORT_SECTION;
	current_storage = CB_STORAGE_REPORT;
	description_field = NULL;
	current_program->flag_report = 1;
	cb_clear_real_field ();
  }
#line 16198 "parser.c" /* yacc.c:1646  */
    break;

  case 758:
#line 7288 "parser.y" /* yacc.c:1646  */
    {
	if (CB_INVALID_TREE ((yyvsp[0]))) {
		YYERROR;
	} else {
		current_field = NULL;
		control_field = NULL;
		description_field = NULL;
		current_report = CB_REPORT_PTR ((yyvsp[0]));
	}
	check_duplicate = 0;
  }
#line 16214 "parser.c" /* yacc.c:1646  */
    break;

  case 759:
#line 7301 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field *p;

	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	current_program->report_storage = description_field;
	current_program->flag_report = 1;
	if (current_report->records == NULL) {
		current_report->records = description_field;
	}
	finalize_report (current_report, description_field);
	(yyval) = CB_TREE (description_field);
  }
#line 16233 "parser.c" /* yacc.c:1646  */
    break;

  case 762:
#line 7320 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 16241 "parser.c" /* yacc.c:1646  */
    break;

  case 763:
#line 7327 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_1, &check_duplicate);
	current_report->global = 1;
	cb_error (_("GLOBAL is not allowed with RD"));
  }
#line 16251 "parser.c" /* yacc.c:1646  */
    break;

  case 764:
#line 7333 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CODE", SYN_CLAUSE_2, &check_duplicate);
	current_report->code_clause = (yyvsp[0]);
  }
#line 16260 "parser.c" /* yacc.c:1646  */
    break;

  case 767:
#line 7345 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONTROL", SYN_CLAUSE_3, &check_duplicate);
  }
#line 16268 "parser.c" /* yacc.c:1646  */
    break;

  case 771:
#line 7358 "parser.y" /* yacc.c:1646  */
    {
	current_report->control_final = 1;
  }
#line 16276 "parser.c" /* yacc.c:1646  */
    break;

  case 774:
#line 7370 "parser.y" /* yacc.c:1646  */
    {
	/* Add field to current control list */
	CB_ADD_TO_CHAIN ((yyvsp[0]), current_report->controls);
  }
#line 16285 "parser.c" /* yacc.c:1646  */
    break;

  case 775:
#line 7381 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PAGE", SYN_CLAUSE_4, &check_duplicate);
	if (!current_report->heading) {
		current_report->heading = 1;
	}
	if (!current_report->first_detail) {
		current_report->first_detail = current_report->heading;
	}
	if (!current_report->last_control) {
		if (current_report->last_detail) {
			current_report->last_control = current_report->last_detail;
		} else if (current_report->footing) {
			current_report->last_control = current_report->footing;
		} else {
			current_report->last_control = current_report->lines;
		}
		if (current_report->t_last_detail) {
			current_report->t_last_control = current_report->t_last_detail;
		} else if (current_report->t_footing) {
			current_report->t_last_control = current_report->t_footing;
		} else if(current_report->t_lines) {
			current_report->t_last_control = current_report->t_lines;
		}
	}
	if (!current_report->last_detail && !current_report->footing) {
		current_report->last_detail = current_report->lines;
		current_report->footing = current_report->lines;
	} else if (!current_report->last_detail) {
		current_report->last_detail = current_report->footing;
	} else if (!current_report->footing) {
		current_report->footing = current_report->last_detail;
	}
	/* PAGE LIMIT values checked in finalize_report in typeck.c */
  }
#line 16324 "parser.c" /* yacc.c:1646  */
    break;

  case 776:
#line 7419 "parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P ((yyvsp[-1]))) {
		current_report->lines = cb_get_int ((yyvsp[-1]));
		if (current_report->lines > 999) {
			cb_error ("PAGE LIMIT lines > 999");
		}
	} else {
		current_report->t_lines = (yyvsp[-1]);
	}
  }
#line 16339 "parser.c" /* yacc.c:1646  */
    break;

  case 778:
#line 7431 "parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P ((yyvsp[-2]))) {
		current_report->lines = cb_get_int ((yyvsp[-2]));
		if (current_report->lines > 999) {
			cb_error ("PAGE LIMIT lines > 999");
		}
	} else {
		current_report->t_lines = (yyvsp[-2]);
	}
  }
#line 16354 "parser.c" /* yacc.c:1646  */
    break;

  case 779:
#line 7445 "parser.y" /* yacc.c:1646  */
    {
	/* may be repeated later by page detail */
	check_repeated ("LINE LIMIT", SYN_CLAUSE_5, &check_duplicate);
	if (CB_LITERAL_P ((yyvsp[-1]))) {
		current_report->columns = cb_get_int ((yyvsp[-1]));
	} else {
		current_report->t_columns = (yyvsp[-1]);
	}
  }
#line 16368 "parser.c" /* yacc.c:1646  */
    break;

  case 789:
#line 7473 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINE LIMIT", SYN_CLAUSE_5, &check_duplicate);
	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->columns = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_columns = (yyvsp[0]);
	}
  }
#line 16381 "parser.c" /* yacc.c:1646  */
    break;

  case 790:
#line 7485 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("HEADING", SYN_CLAUSE_6, &check_duplicate);
	error_if_no_page_lines_limit ("HEADING");

	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->heading = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_heading = (yyvsp[0]);
	}
  }
#line 16396 "parser.c" /* yacc.c:1646  */
    break;

  case 791:
#line 7499 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FIRST DETAIL", SYN_CLAUSE_7, &check_duplicate);
	error_if_no_page_lines_limit ("FIRST DETAIL");

	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->first_detail = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_first_detail = (yyvsp[0]);
	}
  }
#line 16411 "parser.c" /* yacc.c:1646  */
    break;

  case 792:
#line 7513 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LAST CONTROL HEADING", SYN_CLAUSE_8, &check_duplicate);
	error_if_no_page_lines_limit ("LAST CONTROL HEADING");

	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->last_control = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_last_control = (yyvsp[0]);
	}
  }
#line 16426 "parser.c" /* yacc.c:1646  */
    break;

  case 793:
#line 7527 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LAST DETAIL", SYN_CLAUSE_9, &check_duplicate);
	error_if_no_page_lines_limit ("LAST DETAIL");

	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->last_detail = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_last_detail = (yyvsp[0]);
	}
  }
#line 16441 "parser.c" /* yacc.c:1646  */
    break;

  case 794:
#line 7541 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOOTING", SYN_CLAUSE_10, &check_duplicate);
	error_if_no_page_lines_limit ("FOOTING");

	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->footing = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_footing = (yyvsp[0]);
	}
  }
#line 16456 "parser.c" /* yacc.c:1646  */
    break;

  case 797:
#line 7559 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = cb_build_field_tree ((yyvsp[-1]), (yyvsp[0]), current_field, current_storage,
				 current_file, 0);
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-1]));
	check_pic_duplicate = 0;
	if (CB_INVALID_TREE (x)) {
		YYERROR;
	}

	current_field = CB_FIELD (x);
	if (!description_field) {
		description_field = current_field;
	}
  }
#line 16478 "parser.c" /* yacc.c:1646  */
    break;

  case 799:
#line 7578 "parser.y" /* yacc.c:1646  */
    {
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-2]));
	cb_unput_dot ();
	yyerrok;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
#line 16492 "parser.c" /* yacc.c:1646  */
    break;

  case 818:
#line 7614 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("TYPE", SYN_CLAUSE_16, &check_pic_duplicate);
  }
#line 16500 "parser.c" /* yacc.c:1646  */
    break;

  case 819:
#line 7621 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_HEADING;
  }
#line 16508 "parser.c" /* yacc.c:1646  */
    break;

  case 820:
#line 7625 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_PAGE_HEADING;
  }
#line 16516 "parser.c" /* yacc.c:1646  */
    break;

  case 823:
#line 7631 "parser.y" /* yacc.c:1646  */
    {
	if (current_report != NULL) {
		current_report->has_detail = 1;
	}
	current_field->report_flag |= COB_REPORT_DETAIL;
  }
#line 16527 "parser.c" /* yacc.c:1646  */
    break;

  case 824:
#line 7638 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_PAGE_FOOTING;
  }
#line 16535 "parser.c" /* yacc.c:1646  */
    break;

  case 825:
#line 7642 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_FOOTING;
  }
#line 16543 "parser.c" /* yacc.c:1646  */
    break;

  case 826:
#line 7649 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_HEADING;
  }
#line 16551 "parser.c" /* yacc.c:1646  */
    break;

  case 827:
#line 7653 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_HEADING;
	current_field->report_control = (yyvsp[-1]);
	if ((yyvsp[0])) {
		current_field->report_flag |= COB_REPORT_PAGE;
	}
  }
#line 16563 "parser.c" /* yacc.c:1646  */
    break;

  case 828:
#line 7661 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_HEADING_FINAL;
  }
#line 16571 "parser.c" /* yacc.c:1646  */
    break;

  case 829:
#line 7670 "parser.y" /* yacc.c:1646  */
    {(yyval) = NULL;}
#line 16577 "parser.c" /* yacc.c:1646  */
    break;

  case 830:
#line 7671 "parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int0;}
#line 16583 "parser.c" /* yacc.c:1646  */
    break;

  case 831:
#line 7676 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING;
  }
#line 16591 "parser.c" /* yacc.c:1646  */
    break;

  case 832:
#line 7680 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING;
	current_field->report_control = (yyvsp[-1]);
  }
#line 16600 "parser.c" /* yacc.c:1646  */
    break;

  case 833:
#line 7685 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING_FINAL;
  }
#line 16608 "parser.c" /* yacc.c:1646  */
    break;

  case 834:
#line 7689 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING;
	current_field->report_flag |= COB_REPORT_ALL;
  }
#line 16617 "parser.c" /* yacc.c:1646  */
    break;

  case 835:
#line 7697 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("NEXT GROUP", SYN_CLAUSE_17, &check_pic_duplicate);
  }
#line 16625 "parser.c" /* yacc.c:1646  */
    break;

  case 836:
#line 7704 "parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign > 0) {
		current_field->report_flag |= COB_REPORT_NEXT_GROUP_PLUS;
	} else {
		current_field->report_flag |= COB_REPORT_NEXT_GROUP_LINE;
	}
	current_field->next_group_line = cb_get_int ((yyvsp[0]));
  }
#line 16638 "parser.c" /* yacc.c:1646  */
    break;

  case 837:
#line 7713 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_NEXT_GROUP_PLUS;
	current_field->next_group_line = cb_get_int((yyvsp[0]));
  }
#line 16647 "parser.c" /* yacc.c:1646  */
    break;

  case 838:
#line 7718 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_NEXT_GROUP_PLUS;
	current_field->next_group_line = cb_get_int((yyvsp[0]));
  }
#line 16656 "parser.c" /* yacc.c:1646  */
    break;

  case 839:
#line 7723 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_NEXT_GROUP_PAGE;
  }
#line 16664 "parser.c" /* yacc.c:1646  */
    break;

  case 843:
#line 7736 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SUM", SYN_CLAUSE_19, &check_pic_duplicate);
	current_field->report_sum_list = (yyvsp[-1]);
	build_sum_counter (current_report, current_field);
  }
#line 16674 "parser.c" /* yacc.c:1646  */
    break;

  case 846:
#line 7746 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_sum_upon = (yyvsp[0]);
  }
#line 16682 "parser.c" /* yacc.c:1646  */
    break;

  case 847:
#line 7753 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_reset = (yyvsp[0]);
  }
#line 16690 "parser.c" /* yacc.c:1646  */
    break;

  case 848:
#line 7757 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_RESET_FINAL;
  }
#line 16698 "parser.c" /* yacc.c:1646  */
    break;

  case 849:
#line 7764 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_when = (yyvsp[0]);
  }
#line 16707 "parser.c" /* yacc.c:1646  */
    break;

  case 850:
#line 7769 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag &= ~COB_REPORT_BEFORE;
  }
#line 16717 "parser.c" /* yacc.c:1646  */
    break;

  case 851:
#line 7775 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag &= ~COB_REPORT_BEFORE;
	current_field->report_flag |= COB_REPORT_PAGE;
  }
#line 16728 "parser.c" /* yacc.c:1646  */
    break;

  case 852:
#line 7782 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag |= COB_REPORT_BEFORE;
  }
#line 16738 "parser.c" /* yacc.c:1646  */
    break;

  case 853:
#line 7788 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag |= COB_REPORT_BEFORE;
	current_field->report_flag |= COB_REPORT_PAGE;
  }
#line 16749 "parser.c" /* yacc.c:1646  */
    break;

  case 854:
#line 7798 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_PRESENT;
  }
#line 16757 "parser.c" /* yacc.c:1646  */
    break;

  case 855:
#line 7802 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag |= COB_REPORT_NEGATE;
  }
#line 16766 "parser.c" /* yacc.c:1646  */
    break;

  case 858:
#line 7815 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_PAGE;
  }
#line 16774 "parser.c" /* yacc.c:1646  */
    break;

  case 859:
#line 7819 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_control = (yyvsp[0]);
  }
#line 16782 "parser.c" /* yacc.c:1646  */
    break;

  case 861:
#line 7827 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("RW VARYING clause");
  }
#line 16790 "parser.c" /* yacc.c:1646  */
    break;

  case 862:
#line 7834 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINE", SYN_CLAUSE_21, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_LINE;
  }
#line 16799 "parser.c" /* yacc.c:1646  */
    break;

  case 866:
#line 7851 "parser.y" /* yacc.c:1646  */
    {
	if (current_field->report_line == 0) {
		CB_PENDING ("LINE 0");
	}
  }
#line 16809 "parser.c" /* yacc.c:1646  */
    break;

  case 867:
#line 7857 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_LINE_NEXT_PAGE;
  }
#line 16817 "parser.c" /* yacc.c:1646  */
    break;

  case 868:
#line 7861 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_LINE_PLUS;
	current_field->report_line = cb_get_int ((yyvsp[0]));
	if ((yyvsp[0]) != cb_int0 &&
	    (yyvsp[0]) != cb_int1) {
		if ((CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign < 0)
		|| current_field->report_line < 0) {
			cb_error (_("positive integer value expected"));
		}
	}
	if (current_field->report_line == 0) {
		CB_PENDING ("LINE PLUS 0");
	}
  }
#line 16836 "parser.c" /* yacc.c:1646  */
    break;

  case 869:
#line 7876 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_LINE_PLUS;
	current_field->report_line = cb_get_int ((yyvsp[0]));
	if((yyvsp[0]) != cb_int0
	&& (yyvsp[0]) != cb_int1) {
		if ((CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign < 0)
		|| current_field->report_line < 0) {
			cb_error (_("positive integer value expected"));
		}
	}
	if (current_field->report_line == 0) {
		CB_PENDING ("LINE PLUS 0");
	}
  }
#line 16855 "parser.c" /* yacc.c:1646  */
    break;

  case 870:
#line 7894 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_line = cb_get_int ((yyvsp[0]));
	if ((yyvsp[0]) != cb_int0) {
		if (CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign > 0) {
			current_field->report_flag |= COB_REPORT_LINE_PLUS;
		} else if ((CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign < 0)
			|| current_field->report_line < 0) {
			cb_error (_("positive integer value expected"));
			current_field->report_line = 1;
			current_field->report_flag |= COB_REPORT_LINE_PLUS;
		}
	}
  }
#line 16873 "parser.c" /* yacc.c:1646  */
    break;

  case 871:
#line 7912 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLUMN", SYN_CLAUSE_18, &check_pic_duplicate);
	if((current_field->report_flag & (COB_REPORT_COLUMN_LEFT|COB_REPORT_COLUMN_RIGHT|COB_REPORT_COLUMN_CENTER))
	&& (current_field->report_flag & COB_REPORT_COLUMN_PLUS)) {
		if (cb_relaxed_syntax_checks) {
			cb_warning (COBC_WARN_FILLER, _("PLUS is not recommended with LEFT, RIGHT or CENTER"));
		} else {
			cb_error (_("PLUS is not allowed with LEFT, RIGHT or CENTER"));
		}
	}
  }
#line 16889 "parser.c" /* yacc.c:1646  */
    break;

  case 875:
#line 7936 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_COLUMN_LEFT;
  }
#line 16897 "parser.c" /* yacc.c:1646  */
    break;

  case 876:
#line 7940 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_COLUMN_RIGHT;
  }
#line 16905 "parser.c" /* yacc.c:1646  */
    break;

  case 877:
#line 7944 "parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_COLUMN_CENTER;
  }
#line 16913 "parser.c" /* yacc.c:1646  */
    break;

  case 878:
#line 7951 "parser.y" /* yacc.c:1646  */
    {
	int colnum;
	colnum = cb_get_int ((yyvsp[0]));
	if (colnum > 0) {
		if(current_field->parent
		&& current_field->parent->children == current_field) {
			cb_warning (COBC_WARN_FILLER, _("PLUS is ignored on first field of line"));
			if (current_field->step_count == 0)
				current_field->step_count = colnum;
		} else {
			current_field->report_flag |= COB_REPORT_COLUMN_PLUS;
		}
	} else {
		colnum = 0;
	}
	if(current_field->report_column == 0)
		current_field->report_column = colnum;
	current_field->report_num_col++;
  }
#line 16937 "parser.c" /* yacc.c:1646  */
    break;

  case 882:
#line 7980 "parser.y" /* yacc.c:1646  */
    {
	int colnum;
	colnum = cb_get_int ((yyvsp[0]));
	if (CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign > 0) {
		if(current_field->parent
		&& current_field->parent->children == current_field) {
			cb_warning (COBC_WARN_FILLER, _("PLUS is ignored on first field of line"));
		} else {
			current_field->report_flag |= COB_REPORT_COLUMN_PLUS;
		}
	}
	if((yyvsp[0]) != cb_int1
	&& (yyvsp[0]) != cb_int0) {
		if (colnum <= 0
		|| (CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign < 0)) {
			cb_error (_("invalid COLUMN integer; must be > 0"));
			colnum = 0;
			(yyval) = cb_int0;
		} else if(colnum <= current_field->report_column) {
			cb_warning (COBC_WARN_FILLER, _("COLUMN numbers should increase"));
		}
		current_field->report_column_list =
				cb_list_append (current_field->report_column_list, CB_LIST_INIT ((yyvsp[0])));
	}
	if(current_field->report_column == 0)
		current_field->report_column = colnum;
	current_field->report_num_col++;
  }
#line 16970 "parser.c" /* yacc.c:1646  */
    break;

  case 883:
#line 8012 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SOURCE", SYN_CLAUSE_22, &check_pic_duplicate);
	current_field->report_source = (yyvsp[-1]);
  }
#line 16979 "parser.c" /* yacc.c:1646  */
    break;

  case 884:
#line 8020 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GROUP", SYN_CLAUSE_23, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_GROUP_INDICATE;
  }
#line 16988 "parser.c" /* yacc.c:1646  */
    break;

  case 886:
#line 8030 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = CB_CS_SCREEN;
	current_storage = CB_STORAGE_SCREEN;
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 17000 "parser.c" /* yacc.c:1646  */
    break;

  case 887:
#line 8038 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field *p;

	if (description_field) {
		for (p = description_field; p; p = p->sister) {
			cb_validate_field (p);
		}
		current_program->screen_storage = description_field;
		current_program->flag_screen = 1;
	}
	cobc_cs_check = 0;
  }
#line 17017 "parser.c" /* yacc.c:1646  */
    break;

  case 893:
#line 8065 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = cb_build_field_tree ((yyvsp[-1]), (yyvsp[0]), current_field, current_storage,
				 current_file, 0);
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-1]));
	check_pic_duplicate = 0;
	if (CB_INVALID_TREE (x)) {
		YYERROR;
	}

	current_field = CB_FIELD (x);
	if (current_field->parent) {
		current_field->screen_foreg = current_field->parent->screen_foreg;
		current_field->screen_backg = current_field->parent->screen_backg;
		current_field->screen_prompt = current_field->parent->screen_prompt;
	}
  }
#line 17041 "parser.c" /* yacc.c:1646  */
    break;

  case 894:
#line 8085 "parser.y" /* yacc.c:1646  */
    {
	cob_flags_t	flags;

	if (current_field->parent) {
		flags = current_field->parent->screen_flag;
		flags &= ~COB_SCREEN_BLANK_LINE;
		flags &= ~COB_SCREEN_BLANK_SCREEN;
		flags &= ~COB_SCREEN_ERASE_EOL;
		flags &= ~COB_SCREEN_ERASE_EOS;
		flags &= ~COB_SCREEN_LINE_PLUS;
		flags &= ~COB_SCREEN_LINE_MINUS;
		flags &= ~COB_SCREEN_COLUMN_PLUS;
		flags &= ~COB_SCREEN_COLUMN_MINUS;

		flags = zero_conflicting_flags (current_field->screen_flag,
						flags);

		current_field->screen_flag |= flags;
	}

	if (current_field->screen_flag & COB_SCREEN_INITIAL) {
		if (!(current_field->screen_flag & COB_SCREEN_INPUT)) {
			cb_error (_("INITIAL specified on non-input field"));
		}
	}
	if (!qualifier) {
		current_field->flag_filler = 1;
	}

	if (!description_field) {
		description_field = current_field;
	}
	if (current_field->flag_occurs
	 && !has_relative_pos (current_field)) {
		cb_error (_("relative LINE/COLUMN clause required with OCCURS"));
	}
  }
#line 17083 "parser.c" /* yacc.c:1646  */
    break;

  case 895:
#line 8124 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = cb_build_field_tree ((yyvsp[-1]), (yyvsp[0]), current_field, current_storage,
				 current_file, 0);
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-1]));
	check_pic_duplicate = 0;
	if (CB_INVALID_TREE (x)) {
		YYERROR;
	}

	current_field = CB_FIELD (x);
	if (current_field->parent) {
		current_field->screen_foreg = current_field->parent->screen_foreg;
		current_field->screen_backg = current_field->parent->screen_backg;
		current_field->screen_prompt = current_field->parent->screen_prompt;
	}
  }
#line 17107 "parser.c" /* yacc.c:1646  */
    break;

  case 896:
#line 8144 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL CONTROL");
  }
#line 17115 "parser.c" /* yacc.c:1646  */
    break;

  case 897:
#line 8149 "parser.y" /* yacc.c:1646  */
    {
	cob_flags_t	flags;

	if (current_field->parent) {
		flags = current_field->parent->screen_flag;
		flags &= ~COB_SCREEN_BLANK_LINE;
		flags &= ~COB_SCREEN_BLANK_SCREEN;
		flags &= ~COB_SCREEN_ERASE_EOL;
		flags &= ~COB_SCREEN_ERASE_EOS;
		flags &= ~COB_SCREEN_LINE_PLUS;
		flags &= ~COB_SCREEN_LINE_MINUS;
		flags &= ~COB_SCREEN_COLUMN_PLUS;
		flags &= ~COB_SCREEN_COLUMN_MINUS;

		flags = zero_conflicting_flags (current_field->screen_flag,
						flags);

		current_field->screen_flag |= flags;
	}

	if (current_field->screen_flag & COB_SCREEN_INITIAL) {
		if (!(current_field->screen_flag & COB_SCREEN_INPUT)) {
			cb_error (_("INITIAL specified on non-input field"));
		}
	}
	if (!qualifier) {
		current_field->flag_filler = 1;
	}

	if (!description_field) {
		description_field = current_field;
	}
	if (current_field->flag_occurs
	 && !has_relative_pos (current_field)) {
		cb_error (_("relative LINE/COLUMN clause required with OCCURS"));
	}
	cobc_cs_check = CB_CS_SCREEN;
  }
#line 17158 "parser.c" /* yacc.c:1646  */
    break;

  case 898:
#line 8189 "parser.y" /* yacc.c:1646  */
    {
	/*
	  Tree associated with level number has already been freed; we don't
	  need to do anything here.
	*/
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
#if	1	/* RXWRXW Screen field */
	if (current_field) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
	}
#endif
	current_field = cb_get_real_field ();
  }
#line 17180 "parser.c" /* yacc.c:1646  */
    break;

  case 901:
#line 8215 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
				       "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
#line 17189 "parser.c" /* yacc.c:1646  */
    break;

  case 902:
#line 8220 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				       "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
#line 17198 "parser.c" /* yacc.c:1646  */
    break;

  case 903:
#line 8225 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("BELL", COB_SCREEN_BELL);
  }
#line 17206 "parser.c" /* yacc.c:1646  */
    break;

  case 904:
#line 8229 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("BLINK", COB_SCREEN_BLINK);
  }
#line 17214 "parser.c" /* yacc.c:1646  */
    break;

  case 905:
#line 8233 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
				       "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
#line 17223 "parser.c" /* yacc.c:1646  */
    break;

  case 906:
#line 8238 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				       "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 17232 "parser.c" /* yacc.c:1646  */
    break;

  case 907:
#line 8243 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				       "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 17241 "parser.c" /* yacc.c:1646  */
    break;

  case 908:
#line 8248 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				       "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 17250 "parser.c" /* yacc.c:1646  */
    break;

  case 909:
#line 8253 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("STANDARD intensity");
#if 0 /* in general we could simply remove high/low, but for syntax checks
	we still need a flag */
	set_screen_attr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				       "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
#endif
  }
#line 17263 "parser.c" /* yacc.c:1646  */
    break;

  case 910:
#line 8262 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 17271 "parser.c" /* yacc.c:1646  */
    break;

  case 911:
#line 8266 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 17279 "parser.c" /* yacc.c:1646  */
    break;

  case 912:
#line 8270 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 17287 "parser.c" /* yacc.c:1646  */
    break;

  case 913:
#line 8274 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("REVERSE-VIDEO", COB_SCREEN_REVERSE);
  }
#line 17295 "parser.c" /* yacc.c:1646  */
    break;

  case 914:
#line 8278 "parser.y" /* yacc.c:1646  */
    {
	/* set_screen_attr ("SIZE", COB_SCREEN_SIZE); */
	CB_PENDING ("SIZE clause");
	current_field->size = cb_get_int ((yyvsp[0]));
  }
#line 17305 "parser.c" /* yacc.c:1646  */
    break;

  case 915:
#line 8284 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("screen positions from data-item"));
  }
#line 17313 "parser.c" /* yacc.c:1646  */
    break;

  case 916:
#line 8288 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("screen positions from data-item"));
	CB_PENDING ("SIZE clause");
  }
#line 17322 "parser.c" /* yacc.c:1646  */
    break;

  case 917:
#line 8293 "parser.y" /* yacc.c:1646  */
    {
	/* set_screen_attr ("SIZE", COB_SCREEN_SIZE); */
	CB_PENDING ("SIZE clause");
	current_field->size = cb_get_int ((yyvsp[0]));
  }
#line 17332 "parser.c" /* yacc.c:1646  */
    break;

  case 918:
#line 8299 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("UNDERLINE", COB_SCREEN_UNDERLINE);
  }
#line 17340 "parser.c" /* yacc.c:1646  */
    break;

  case 919:
#line 8303 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("OVERLINE", COB_SCREEN_OVERLINE);
	CB_PENDING ("OVERLINE");
  }
#line 17349 "parser.c" /* yacc.c:1646  */
    break;

  case 920:
#line 8308 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("GRID", COB_SCREEN_GRID);
	CB_PENDING ("GRID");
  }
#line 17358 "parser.c" /* yacc.c:1646  */
    break;

  case 921:
#line 8313 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("LEFTLINE", COB_SCREEN_LEFTLINE);
	CB_PENDING ("LEFTLINE");
  }
#line 17367 "parser.c" /* yacc.c:1646  */
    break;

  case 922:
#line 8318 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("AUTO", COB_SCREEN_AUTO,
				       "TAB", COB_SCREEN_TAB);
  }
#line 17376 "parser.c" /* yacc.c:1646  */
    break;

  case 923:
#line 8323 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("TAB", COB_SCREEN_TAB,
				       "AUTO", COB_SCREEN_AUTO);
  }
#line 17385 "parser.c" /* yacc.c:1646  */
    break;

  case 924:
#line 8328 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("SECURE", COB_SCREEN_SECURE,
				       "NO-ECHO", COB_SCREEN_NO_ECHO);
  }
#line 17394 "parser.c" /* yacc.c:1646  */
    break;

  case 925:
#line 8333 "parser.y" /* yacc.c:1646  */
    {
	if (cb_no_echo_means_secure) {
		set_screen_attr ("SECURE", COB_SCREEN_SECURE);
	} else {
		set_screen_attr_with_conflict ("NO-ECHO", COB_SCREEN_NO_ECHO,
					       "SECURE", COB_SCREEN_SECURE);
	}
  }
#line 17407 "parser.c" /* yacc.c:1646  */
    break;

  case 926:
#line 8342 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("REQUIRED", COB_SCREEN_REQUIRED);
  }
#line 17415 "parser.c" /* yacc.c:1646  */
    break;

  case 927:
#line 8346 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("FULL", COB_SCREEN_FULL);
  }
#line 17423 "parser.c" /* yacc.c:1646  */
    break;

  case 928:
#line 8350 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
	current_field->screen_prompt = (yyvsp[0]);
  }
#line 17432 "parser.c" /* yacc.c:1646  */
    break;

  case 929:
#line 8355 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
  }
#line 17440 "parser.c" /* yacc.c:1646  */
    break;

  case 930:
#line 8359 "parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("INITIAL", COB_SCREEN_INITIAL);
  }
#line 17448 "parser.c" /* yacc.c:1646  */
    break;

  case 931:
#line 8363 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINE", SYN_CLAUSE_16, &check_pic_duplicate);
  }
#line 17456 "parser.c" /* yacc.c:1646  */
    break;

  case 932:
#line 8367 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("LINES clause");	/* note: should only occur with controls */
  }
#line 17464 "parser.c" /* yacc.c:1646  */
    break;

  case 933:
#line 8371 "parser.y" /* yacc.c:1646  */
    {
	//check_repeated ("CLINE", SYN_CLAUSE_5000, &check_pic_duplicate);
  }
#line 17472 "parser.c" /* yacc.c:1646  */
    break;

  case 934:
#line 8375 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLUMN", SYN_CLAUSE_17, &check_pic_duplicate);
  }
#line 17480 "parser.c" /* yacc.c:1646  */
    break;

  case 935:
#line 8379 "parser.y" /* yacc.c:1646  */
    {
	//check_repeated ("CCOL", SYN_CLAUSE_5001, &check_pic_duplicate);
  }
#line 17488 "parser.c" /* yacc.c:1646  */
    break;

  case 936:
#line 8383 "parser.y" /* yacc.c:1646  */
    {
#if 0 /* TODO: implement, and add reverse to BACKGROUND/FOREGROUND-COLOR */
	check_repeated ("COLOR", SYN_CLAUSE_19, &check_pic_duplicate);
	set_screen_attr_with_conflict ("COLOR", COB_SCREEN_COLOR,
				       "BACKGROUND-COLOR", COB_SCREEN_BACKGROUND_COLOR);
	set_screen_attr_with_conflict ("COLOR", COB_SCREEN_COLOR,
				       "FOREGROUND-COLOR", FOREGROUND_COLOR);
#endif
	CB_PENDING ("COLOR clause");
  }
#line 17503 "parser.c" /* yacc.c:1646  */
    break;

  case 937:
#line 8394 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_18, &check_pic_duplicate);
	current_field->screen_foreg = (yyvsp[0]);
  }
#line 17512 "parser.c" /* yacc.c:1646  */
    break;

  case 938:
#line 8399 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_19, &check_pic_duplicate);
	current_field->screen_backg = (yyvsp[0]);
  }
#line 17521 "parser.c" /* yacc.c:1646  */
    break;

  case 947:
#line 8412 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = check_not_88_level ((yyvsp[0]));

	check_repeated ("USING", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->screen_from = (yyval);
	current_field->screen_to = (yyval);
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
#line 17534 "parser.c" /* yacc.c:1646  */
    break;

  case 948:
#line 8421 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FROM", SYN_CLAUSE_21, &check_pic_duplicate);
	current_field->screen_from = (yyvsp[0]);
  }
#line 17543 "parser.c" /* yacc.c:1646  */
    break;

  case 949:
#line 8426 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = check_not_88_level ((yyvsp[0]));

	check_repeated ("TO", SYN_CLAUSE_22, &check_pic_duplicate);
	current_field->screen_to = (yyval);
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
#line 17555 "parser.c" /* yacc.c:1646  */
    break;

  case 951:
#line 8438 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check |= CB_CS_GRAPHICAL_CONTROL;
  }
#line 17563 "parser.c" /* yacc.c:1646  */
    break;

  case 1256:
#line 9041 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 17569 "parser.c" /* yacc.c:1646  */
    break;

  case 1257:
#line 9042 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 17575 "parser.c" /* yacc.c:1646  */
    break;

  case 1258:
#line 9046 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 17581 "parser.c" /* yacc.c:1646  */
    break;

  case 1259:
#line 9047 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 17587 "parser.c" /* yacc.c:1646  */
    break;

  case 1260:
#line 9052 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_field->screen_line = (yyvsp[0]);
	}
  }
#line 17597 "parser.c" /* yacc.c:1646  */
    break;

  case 1262:
#line 9062 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
  }
#line 17605 "parser.c" /* yacc.c:1646  */
    break;

  case 1263:
#line 9066 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
  }
#line 17613 "parser.c" /* yacc.c:1646  */
    break;

  case 1264:
#line 9073 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_field->screen_column = (yyvsp[0]);
	}
  }
#line 17623 "parser.c" /* yacc.c:1646  */
    break;

  case 1265:
#line 9082 "parser.y" /* yacc.c:1646  */
    {
	/* Nothing */
  }
#line 17631 "parser.c" /* yacc.c:1646  */
    break;

  case 1266:
#line 9086 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
  }
#line 17639 "parser.c" /* yacc.c:1646  */
    break;

  case 1267:
#line 9090 "parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;
  }
#line 17647 "parser.c" /* yacc.c:1646  */
    break;

  case 1268:
#line 9097 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("OCCURS screen items"));
	check_repeated ("OCCURS", SYN_CLAUSE_23, &check_pic_duplicate);
	current_field->occurs_max = cb_get_int ((yyvsp[-1]));
	current_field->occurs_min = current_field->occurs_max;
	current_field->indexes++;
	current_field->flag_occurs = 1;
  }
#line 17660 "parser.c" /* yacc.c:1646  */
    break;

  case 1269:
#line 9109 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("GLOBAL screen items"));
  }
#line 17668 "parser.c" /* yacc.c:1646  */
    break;

  case 1270:
#line 9118 "parser.y" /* yacc.c:1646  */
    {
	current_section = NULL;
	current_paragraph = NULL;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
  }
#line 17682 "parser.c" /* yacc.c:1646  */
    break;

  case 1271:
#line 9128 "parser.y" /* yacc.c:1646  */
    {
	current_section = NULL;
	current_paragraph = NULL;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	cobc_in_procedure = 1U;
	cb_set_system_names ();
	backup_current_pos ();
  }
#line 17696 "parser.c" /* yacc.c:1646  */
    break;

  case 1272:
#line 9138 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-3])) {
		if (current_program->entry_convention) {
			cb_warning (COBC_WARN_FILLER, _("overriding convention specified in ENTRY-CONVENTION"));
		}
		current_program->entry_convention = (yyvsp[-3]);
	} else if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
	header_check |= COBC_HD_PROCEDURE_DIVISION;
  }
#line 17712 "parser.c" /* yacc.c:1646  */
    break;

  case 1273:
#line 9150 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_main
	 && !current_program->flag_chained && (yyvsp[-4])) {
		cb_error (_("executable program requested but PROCEDURE/ENTRY has USING clause"));
	}
	/* Main entry point */
	emit_entry (current_program->program_id, 0, (yyvsp[-4]), NULL);
	current_program->num_proc_params = cb_list_length ((yyvsp[-4]));
	if (current_program->source_name) {
		emit_entry (current_program->source_name, 1, (yyvsp[-4]), NULL);
	}
  }
#line 17729 "parser.c" /* yacc.c:1646  */
    break;

  case 1274:
#line 9163 "parser.y" /* yacc.c:1646  */
    {
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
	}
  }
#line 17748 "parser.c" /* yacc.c:1646  */
    break;

  case 1275:
#line 9178 "parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	/* No PROCEDURE DIVISION header here */
	/* Only a statement is allowed as first element */
	/* Thereafter, sections/paragraphs may be used */
	check_pic_duplicate = 0;
	check_duplicate = 0;
	if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
	cobc_in_procedure = 1U;
	label = cb_build_reference ("MAIN SECTION");
	current_section = CB_LABEL (cb_build_label (label, NULL));
	current_section->flag_section = 1;
	current_section->flag_dummy_section = 1;
	current_section->flag_skip_label = !!skip_statements;
	current_section->flag_declaratives = !!in_declaratives;
	current_section->xref.skip = 1;
	emit_statement (CB_TREE (current_section));
	label = cb_build_reference ("MAIN PARAGRAPH");
	current_paragraph = CB_LABEL (cb_build_label (label, NULL));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_dummy_paragraph = 1;
	current_paragraph->xref.skip = 1;
	emit_statement (CB_TREE (current_paragraph));
	cb_set_system_names ();
  }
#line 17782 "parser.c" /* yacc.c:1646  */
    break;

  case 1277:
#line 9212 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 17790 "parser.c" /* yacc.c:1646  */
    break;

  case 1278:
#line 9216 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 17799 "parser.c" /* yacc.c:1646  */
    break;

  case 1279:
#line 9221 "parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > MAX_CALL_FIELD_PARAMS) {
		cb_error (_("number of parameters exceeds maximum %d"),
			  MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 17811 "parser.c" /* yacc.c:1646  */
    break;

  case 1280:
#line 9229 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("CHAINING invalid in user FUNCTION"));
	} else {
		current_program->flag_chained = 1;
	}
  }
#line 17824 "parser.c" /* yacc.c:1646  */
    break;

  case 1281:
#line 9238 "parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > MAX_CALL_FIELD_PARAMS) {
		cb_error (_("number of parameters exceeds maximum %d"),
			  MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 17836 "parser.c" /* yacc.c:1646  */
    break;

  case 1282:
#line 9248 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17842 "parser.c" /* yacc.c:1646  */
    break;

  case 1283:
#line 9250 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 17848 "parser.c" /* yacc.c:1646  */
    break;

  case 1284:
#line 9255 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	struct cb_field	*f;

	x = cb_build_identifier ((yyvsp[0]), 0);
	if ((yyvsp[-1]) == cb_int1 && CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
		f = CB_FIELD (cb_ref (x));
		f->flag_is_pdiv_opt = 1;
	}

	if (call_mode == CB_CALL_BY_VALUE
	    && CB_REFERENCE_P ((yyvsp[0]))
	    && CB_FIELD (cb_ref ((yyvsp[0])))->flag_any_length) {
		cb_error_x ((yyvsp[0]), _("ANY LENGTH items may only be BY REFERENCE formal parameters"));
	}

	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), x);
	CB_SIZES ((yyval)) = size_mode;
  }
#line 17872 "parser.c" /* yacc.c:1646  */
    break;

  case 1286:
#line 9279 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 17880 "parser.c" /* yacc.c:1646  */
    break;

  case 1287:
#line 9283 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error (_("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		CB_UNFINISHED (_("parameters passed BY VALUE"));
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 17893 "parser.c" /* yacc.c:1646  */
    break;

  case 1289:
#line 9296 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO;
	}
  }
#line 17905 "parser.c" /* yacc.c:1646  */
    break;

  case 1290:
#line 9304 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_4;
	}
  }
#line 17917 "parser.c" /* yacc.c:1646  */
    break;

  case 1291:
#line 9312 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO | CB_SIZE_UNSIGNED;
	}
  }
#line 17929 "parser.c" /* yacc.c:1646  */
    break;

  case 1292:
#line 9320 "parser.y" /* yacc.c:1646  */
    {
	if (size_mode) {
		size_mode |= CB_SIZE_UNSIGNED;
	}
  }
#line 17939 "parser.c" /* yacc.c:1646  */
    break;

  case 1294:
#line 9330 "parser.y" /* yacc.c:1646  */
    {
	unsigned char *s = CB_LITERAL ((yyvsp[0]))->data;
	size_mode = 0;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL ((yyvsp[0]))->size != 1) {
		cb_error_x ((yyvsp[0]), _("invalid value for SIZE"));
	} else {
		size_mode = 0;
		switch (*s) {
		case '1':
			size_mode = CB_SIZE_1;
			break;
		case '2':
			size_mode = CB_SIZE_2;
			break;
		case '4':
			size_mode = CB_SIZE_4;
			break;
		case '8':
			size_mode = CB_SIZE_8;
			break;
		default:
			cb_error_x ((yyvsp[0]), _("invalid value for SIZE"));
			break;
		}
	}
  }
#line 17973 "parser.c" /* yacc.c:1646  */
    break;

  case 1295:
#line 9363 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 17981 "parser.c" /* yacc.c:1646  */
    break;

  case 1296:
#line 9367 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error (_("OPTIONAL only allowed for BY REFERENCE items"));
		(yyval) = cb_int0;
	} else {
		(yyval) = cb_int1;
	}
  }
#line 17994 "parser.c" /* yacc.c:1646  */
    break;

  case 1297:
#line 9379 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("RETURNING clause is required for a FUNCTION"));
	}
  }
#line 18004 "parser.c" /* yacc.c:1646  */
    break;

  case 1298:
#line 9385 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_main) {
		cb_error (_("RETURNING clause cannot be OMITTED for main program"));
	}
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("RETURNING clause cannot be OMITTED for a FUNCTION"));
	}
	current_program->flag_void = 1;
  }
#line 18018 "parser.c" /* yacc.c:1646  */
    break;

  case 1299:
#line 9395 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		/* standard rule: returning item is allocated in the
		   activating runtime element */
		if (f->storage != CB_STORAGE_LINKAGE) {
			cb_error (_("RETURNING item is not defined in LINKAGE SECTION"));
		} else if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01"));
		} else if (f->flag_occurs) {
			cb_error (_("RETURNING item should not have OCCURS"));
		} else {
			if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
				if (f->flag_any_length) {
					cb_error (_("function RETURNING item may not be ANY LENGTH"));
				}

				f->flag_is_returning = 1;
			}
			current_program->returning = (yyvsp[0]);
		}
	}
  }
#line 18048 "parser.c" /* yacc.c:1646  */
    break;

  case 1301:
#line 9424 "parser.y" /* yacc.c:1646  */
    {
	in_declaratives = 1;
	emit_statement (cb_build_comment ("DECLARATIVES"));
  }
#line 18057 "parser.c" /* yacc.c:1646  */
    break;

  case 1302:
#line 9430 "parser.y" /* yacc.c:1646  */
    {
	if (needs_field_debug) {
		start_debug = 1;
	}
	in_declaratives = 0;
	in_debugging = 0;
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
		current_paragraph = NULL;
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		current_section->flag_fatal_check = 1;
		emit_statement (cb_build_perform_exit (current_section));
		current_section = NULL;
	}
	skip_statements = 0;
	emit_statement (cb_build_comment ("END DECLARATIVES"));
	check_unreached = 0;
  }
#line 18087 "parser.c" /* yacc.c:1646  */
    break;

  case 1307:
#line 9468 "parser.y" /* yacc.c:1646  */
    {
	if (next_label_list) {
		cb_tree	plabel;
		char	name[32];

		snprintf (name, sizeof(name), "L$%d", next_label_id);
		plabel = cb_build_label (cb_build_reference (name), NULL);
		CB_LABEL (plabel)->flag_next_sentence = 1;
		emit_statement (plabel);
		current_program->label_list =
			cb_list_append (current_program->label_list, next_label_list);
		next_label_list = NULL;
		next_label_id++;
	}
	/* check_unreached = 0; */
	cb_end_statement();
  }
#line 18109 "parser.c" /* yacc.c:1646  */
    break;

  case 1309:
#line 9487 "parser.y" /* yacc.c:1646  */
    {
	/* check_unreached = 0; */
	cb_end_statement();
  }
#line 18118 "parser.c" /* yacc.c:1646  */
    break;

  case 1310:
#line 9498 "parser.y" /* yacc.c:1646  */
    {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[-1]), 0) == cb_error_node) {
		YYERROR;
	}

	/* Exit the last paragraph/section */
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
	}
	if (current_program->flag_debugging && !in_debugging) {
		if (current_paragraph || current_section) {
			emit_statement (cb_build_comment (
					"DEBUGGING - Fall through"));
			emit_statement (cb_build_debug (cb_debug_contents,
					"FALL THROUGH", NULL));
		}
	}

	/* Begin a new section */
	current_section = CB_LABEL (cb_build_label ((yyvsp[-1]), NULL));
	current_section->flag_section = 1;
	/* Careful here, one negation */
	current_section->flag_real_label = !in_debugging;
	current_section->flag_declaratives = !!in_declaratives;
	current_section->flag_skip_label = !!skip_statements;
	current_paragraph = NULL;
  }
#line 18161 "parser.c" /* yacc.c:1646  */
    break;

  case 1311:
#line 9538 "parser.y" /* yacc.c:1646  */
    {
	emit_statement (CB_TREE (current_section));
  }
#line 18169 "parser.c" /* yacc.c:1646  */
    break;

  case 1314:
#line 9549 "parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[-1]), 1) == cb_error_node) {
		YYERROR;
	}

	/* Exit the last paragraph */
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
		if (current_program->flag_debugging && !in_debugging) {
			emit_statement (cb_build_comment (
					"DEBUGGING - Fall through"));
			emit_statement (cb_build_debug (cb_debug_contents,
					"FALL THROUGH", NULL));
		}
	}

	/* Begin a new paragraph */
	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->flag_section = 1;
		current_section->flag_dummy_section = 1;
		current_section->flag_declaratives = !!in_declaratives;
		current_section->flag_skip_label = !!skip_statements;
		current_section->xref.skip = 1;
		emit_statement (CB_TREE (current_section));
	}
	current_paragraph = CB_LABEL (cb_build_label ((yyvsp[-1]), current_section));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_real_label = !in_debugging;
	current_paragraph->segment = current_section->segment;
	emit_statement (CB_TREE (current_paragraph));
  }
#line 18215 "parser.c" /* yacc.c:1646  */
    break;

  case 1315:
#line 9594 "parser.y" /* yacc.c:1646  */
    {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[0]), 0) != cb_error_node) {
		if (is_reserved_word (CB_NAME ((yyvsp[0])))) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a statement"), CB_NAME ((yyvsp[0])));
		} else if (is_default_reserved_word (CB_NAME ((yyvsp[0])))) {
			cb_error_x ((yyvsp[0]), _("unknown statement '%s'; it may exist in another dialect"),
				    CB_NAME ((yyvsp[0])));
		} else {
			cb_error_x ((yyvsp[0]), _("unknown statement '%s'"), CB_NAME ((yyvsp[0])));
		}
	}
	YYERROR;
  }
#line 18235 "parser.c" /* yacc.c:1646  */
    break;

  case 1316:
#line 9613 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18243 "parser.c" /* yacc.c:1646  */
    break;

  case 1317:
#line 9617 "parser.y" /* yacc.c:1646  */
    {
	int segnum = cb_get_int ((yyvsp[0]));

	(yyval) = NULL;
	if (cb_verify (cb_section_segments, "SECTION segment")) {
		if (segnum > 99) {
			cb_error (_("SECTION segment-number must be less than or equal to 99"));
		} else {
			if (in_declaratives && segnum > 49) {
				cb_error (_("SECTION segment-number in DECLARATIVES must be less than 50"));
			}
			if (!in_declaratives) {
				current_program->flag_segments = 1;
				current_section->segment = segnum;
			} else {
				/* Simon: old version did not allow segments in declaratives at all
					ToDo: check codegen for possible missing parts */
				CB_PENDING (_("SECTION segment within DECLARATIVES"));
			}
		}
	}
  }
#line 18270 "parser.c" /* yacc.c:1646  */
    break;

  case 1318:
#line 9646 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = current_program->exec_list;
	current_program->exec_list = NULL;
	check_unreached = 0;
  }
#line 18280 "parser.c" /* yacc.c:1646  */
    break;

  case 1319:
#line 9651 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_TREE (current_statement);
	current_statement = NULL;
  }
#line 18289 "parser.c" /* yacc.c:1646  */
    break;

  case 1320:
#line 9656 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_reverse (current_program->exec_list);
	current_program->exec_list = (yyvsp[-2]);
	current_statement = CB_STATEMENT ((yyvsp[-1]));
  }
#line 18299 "parser.c" /* yacc.c:1646  */
    break;

  case 1321:
#line 9664 "parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->flag_section = 1;
		current_section->flag_dummy_section = 1;
		current_section->flag_skip_label = !!skip_statements;
		current_section->flag_declaratives = !!in_declaratives;
		current_section->xref.skip = 1;
		emit_statement (CB_TREE (current_section));
	}
	if (!current_paragraph) {
		label = cb_build_reference ("MAIN PARAGRAPH");
		current_paragraph = CB_LABEL (cb_build_label (label, NULL));
		CB_TREE (current_paragraph)->source_file
			= CB_TREE (current_section)->source_file;
		CB_TREE (current_paragraph)->source_line
			= CB_TREE (current_section)->source_line;
		current_paragraph->flag_declaratives = !!in_declaratives;
		current_paragraph->flag_skip_label = !!skip_statements;
		current_paragraph->flag_dummy_paragraph = 1;
		current_paragraph->xref.skip = 1;
		emit_statement (CB_TREE (current_paragraph));
	}
	if (check_headers_present (COBC_HD_PROCEDURE_DIVISION, 0, 0, 0) == 1) {
		if (current_program->prog_type == COB_MODULE_TYPE_PROGRAM) {
			backup_current_pos ();
			emit_entry (current_program->program_id, 0, NULL, NULL);
		}
	}
  }
#line 18337 "parser.c" /* yacc.c:1646  */
    break;

  case 1322:
#line 9698 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 18345 "parser.c" /* yacc.c:1646  */
    break;

  case 1323:
#line 9702 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 18353 "parser.c" /* yacc.c:1646  */
    break;

  case 1387:
#line 9775 "parser.y" /* yacc.c:1646  */
    {
	if (cb_verify (cb_next_sentence_phrase, "NEXT SENTENCE")) {
		cb_tree label;
		char	name[32];

		begin_statement ("NEXT SENTENCE", 0);
		sprintf (name, "L$%d", next_label_id);
		label = cb_build_reference (name);
		next_label_list = cb_list_add (next_label_list, label);
		emit_statement (cb_build_goto (label, NULL));
	}
	check_unreached = 0;
  }
#line 18371 "parser.c" /* yacc.c:1646  */
    break;

  case 1388:
#line 9789 "parser.y" /* yacc.c:1646  */
    {
	yyerrok;
	cobc_cs_check = 0;
  }
#line 18380 "parser.c" /* yacc.c:1646  */
    break;

  case 1389:
#line 9800 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ACCEPT", TERM_ACCEPT);
	cobc_cs_check = CB_CS_ACCEPT;
  }
#line 18389 "parser.c" /* yacc.c:1646  */
    break;

  case 1391:
#line 9810 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
  }
#line 18399 "parser.c" /* yacc.c:1646  */
    break;

  case 1392:
#line 9816 "parser.y" /* yacc.c:1646  */
    {
	/* Check for invalid use of screen clauses */
	if (current_statement->attr_ptr
	 || (!is_screen_field ((yyvsp[-3])) && line_column)) {
		cb_verify_x ((yyvsp[-3]), cb_accept_display_extensions,
			     _("non-standard ACCEPT"));
	}

	if (cb_accept_update && !has_dispattr (COB_SCREEN_NO_UPDATE)) {
		set_dispattr (COB_SCREEN_UPDATE);
	}
	if (cb_accept_auto && !has_dispattr (COB_SCREEN_TAB)) {
		set_dispattr (COB_SCREEN_AUTO);
	}
	if ((yyvsp[-3]) == cb_null && current_statement->attr_ptr) {
		if (current_statement->attr_ptr->prompt) {
			emit_conflicting_clause_message ("ACCEPT OMITTED",
				_("PROMPT clause"));
		}
		if (current_statement->attr_ptr->size_is) {
			emit_conflicting_clause_message ("ACCEPT OMITTED",
				_("SIZE IS clause"));
		}
	}
	cobc_cs_check = 0;
	cb_emit_accept ((yyvsp[-3]), line_column, current_statement->attr_ptr);
  }
#line 18431 "parser.c" /* yacc.c:1646  */
    break;

  case 1393:
#line 9844 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
  }
#line 18441 "parser.c" /* yacc.c:1646  */
    break;

  case 1394:
#line 9850 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	CB_PENDING ("ACCEPT FROM SCREEN");
  }
#line 18450 "parser.c" /* yacc.c:1646  */
    break;

  case 1395:
#line 9855 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_line_or_col ((yyvsp[-2]), 0);
  }
#line 18458 "parser.c" /* yacc.c:1646  */
    break;

  case 1396:
#line 9859 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_line_or_col ((yyvsp[-2]), 1);
  }
#line 18466 "parser.c" /* yacc.c:1646  */
    break;

  case 1397:
#line 9863 "parser.y" /* yacc.c:1646  */
    {
	/* information about terminal and its capabilities
	cb_emit_accept_terminal_info ($1); */
	CB_PENDING ("ACCEPT FROM TERMINAL INFO");
  }
#line 18476 "parser.c" /* yacc.c:1646  */
    break;

  case 1398:
#line 9869 "parser.y" /* yacc.c:1646  */
    {
	/* information about OS and runtime features
	cb_emit_accept_system_info ($1); */
	CB_PENDING ("ACCEPT FROM SYSTEM INFO");
  }
#line 18486 "parser.c" /* yacc.c:1646  */
    break;

  case 1399:
#line 9875 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_date_yyyymmdd ((yyvsp[-3]));
  }
#line 18495 "parser.c" /* yacc.c:1646  */
    break;

  case 1400:
#line 9880 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_date ((yyvsp[-2]));
  }
#line 18504 "parser.c" /* yacc.c:1646  */
    break;

  case 1401:
#line 9885 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_day_yyyyddd ((yyvsp[-3]));
  }
#line 18513 "parser.c" /* yacc.c:1646  */
    break;

  case 1402:
#line 9890 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_day ((yyvsp[-2]));
  }
#line 18522 "parser.c" /* yacc.c:1646  */
    break;

  case 1403:
#line 9895 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_day_of_week ((yyvsp[-2]));
  }
#line 18530 "parser.c" /* yacc.c:1646  */
    break;

  case 1404:
#line 9901 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_escape_key ((yyvsp[-3]));
  }
#line 18538 "parser.c" /* yacc.c:1646  */
    break;

  case 1405:
#line 9907 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_exception_status ((yyvsp[-3]));
  }
#line 18546 "parser.c" /* yacc.c:1646  */
    break;

  case 1406:
#line 9911 "parser.y" /* yacc.c:1646  */
    {
	/* check is data from keyboard available? "1", else "0"
	cb_emit_accept_input_status ($1); */
	CB_PENDING ("ACCEPT FROM INPUT STATUS");
  }
#line 18556 "parser.c" /* yacc.c:1646  */
    break;

  case 1407:
#line 9917 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_time ((yyvsp[-2]));
  }
#line 18564 "parser.c" /* yacc.c:1646  */
    break;

  case 1408:
#line 9921 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_user_name ((yyvsp[-3]));
  }
#line 18573 "parser.c" /* yacc.c:1646  */
    break;

  case 1409:
#line 9926 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_command_line ((yyvsp[-2]));
  }
#line 18581 "parser.c" /* yacc.c:1646  */
    break;

  case 1410:
#line 9930 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_environment ((yyvsp[-3]));
  }
#line 18589 "parser.c" /* yacc.c:1646  */
    break;

  case 1411:
#line 9934 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_get_environment ((yyvsp[-1]), (yyvsp[-4]));
  }
#line 18597 "parser.c" /* yacc.c:1646  */
    break;

  case 1412:
#line 9938 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_arg_number ((yyvsp[-2]));
  }
#line 18605 "parser.c" /* yacc.c:1646  */
    break;

  case 1413:
#line 9942 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_arg_value ((yyvsp[-3]));
  }
#line 18613 "parser.c" /* yacc.c:1646  */
    break;

  case 1414:
#line 9946 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_mnemonic ((yyvsp[-2]), (yyvsp[0]));
  }
#line 18621 "parser.c" /* yacc.c:1646  */
    break;

  case 1415:
#line 9950 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_name ((yyvsp[-2]), (yyvsp[0]));
  }
#line 18629 "parser.c" /* yacc.c:1646  */
    break;

  case 1416:
#line 9954 "parser.y" /* yacc.c:1646  */
    {
	cb_verify_x ((yyvsp[-1]), cb_accept_display_extensions,
		     _("non-standard ACCEPT"));

	if (cb_accept_update && !has_dispattr (COB_SCREEN_NO_UPDATE)) {
		set_dispattr (COB_SCREEN_UPDATE);
	}
	if (cb_accept_auto && !has_dispattr (COB_SCREEN_TAB)) {
		set_dispattr (COB_SCREEN_AUTO);
	}
	cobc_cs_check = 0;
	cb_emit_accept ((yyvsp[-1]), line_column, current_statement->attr_ptr);
  }
#line 18647 "parser.c" /* yacc.c:1646  */
    break;

  case 1417:
#line 9968 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("ACCEPT MESSAGE COUNT");
  }
#line 18655 "parser.c" /* yacc.c:1646  */
    break;

  case 1419:
#line 9976 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 18663 "parser.c" /* yacc.c:1646  */
    break;

  case 1420:
#line 9982 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
  }
#line 18673 "parser.c" /* yacc.c:1646  */
    break;

  case 1421:
#line 9988 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18681 "parser.c" /* yacc.c:1646  */
    break;

  case 1422:
#line 9995 "parser.y" /* yacc.c:1646  */
    {
	line_column = CB_BUILD_PAIR ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 18689 "parser.c" /* yacc.c:1646  */
    break;

  case 1423:
#line 9999 "parser.y" /* yacc.c:1646  */
    {
	line_column = CB_BUILD_PAIR ((yyvsp[-2]), cb_int0);
  }
#line 18697 "parser.c" /* yacc.c:1646  */
    break;

  case 1424:
#line 10003 "parser.y" /* yacc.c:1646  */
    {
	line_column = CB_BUILD_PAIR (cb_int0, (yyvsp[-1]));
  }
#line 18705 "parser.c" /* yacc.c:1646  */
    break;

  case 1425:
#line 10010 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18713 "parser.c" /* yacc.c:1646  */
    break;

  case 1426:
#line 10014 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_binary_op ((yyvsp[-2]), '+', (yyvsp[0]));
  }
#line 18721 "parser.c" /* yacc.c:1646  */
    break;

  case 1427:
#line 10018 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_binary_op ((yyvsp[-2]), '-', (yyvsp[0]));
  }
#line 18729 "parser.c" /* yacc.c:1646  */
    break;

  case 1435:
#line 10042 "parser.y" /* yacc.c:1646  */
    {
	  check_repeated ("FROM CRT", SYN_CLAUSE_2, &check_duplicate);
  }
#line 18737 "parser.c" /* yacc.c:1646  */
    break;

  case 1436:
#line 10046 "parser.y" /* yacc.c:1646  */
    {
	  check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
#line 18745 "parser.c" /* yacc.c:1646  */
    break;

  case 1438:
#line 10051 "parser.y" /* yacc.c:1646  */
    {
	check_repeated (_("TIME-OUT or BEFORE TIME clauses"), SYN_CLAUSE_4,
			&check_duplicate);
	set_attribs (NULL, NULL, NULL, (yyvsp[0]), NULL, NULL, 0);
  }
#line 18755 "parser.c" /* yacc.c:1646  */
    break;

  case 1445:
#line 10076 "parser.y" /* yacc.c:1646  */
    {
	set_attr_with_conflict ("LINE", SYN_CLAUSE_1,
				_("AT screen-location"), SYN_CLAUSE_3, 1,
				&check_line_col_duplicate);

	if ((CB_LITERAL_P ((yyvsp[0])) && cb_get_int ((yyvsp[0])) == 0) || (yyvsp[0]) == cb_zero) {
		cb_verify (cb_accept_display_extensions, "LINE 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR ((yyvsp[0]), cb_int0);
	} else {
		CB_PAIR_X (line_column) = (yyvsp[0]);
	}
  }
#line 18775 "parser.c" /* yacc.c:1646  */
    break;

  case 1446:
#line 10092 "parser.y" /* yacc.c:1646  */
    {
	set_attr_with_conflict ("COLUMN", SYN_CLAUSE_2,
				_("AT screen-location"), SYN_CLAUSE_3, 1,
				&check_line_col_duplicate);

	if ((CB_LITERAL_P ((yyvsp[0])) && cb_get_int ((yyvsp[0])) == 0) || (yyvsp[0]) == cb_zero) {
		cb_verify (cb_accept_display_extensions, "COLUMN 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR (cb_int0, (yyvsp[0]));
	} else {
		CB_PAIR_Y (line_column) = (yyvsp[0]);
	}
  }
#line 18795 "parser.c" /* yacc.c:1646  */
    break;

  case 1447:
#line 10108 "parser.y" /* yacc.c:1646  */
    {
	set_attr_with_conflict (_("AT screen-location"), SYN_CLAUSE_3,
				_("LINE or COLUMN"), SYN_CLAUSE_1 | SYN_CLAUSE_2,
				1, &check_line_col_duplicate);

	cb_verify (cb_accept_display_extensions, "AT clause");

	line_column = (yyvsp[0]);
  }
#line 18809 "parser.c" /* yacc.c:1646  */
    break;

  case 1448:
#line 10120 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 18815 "parser.c" /* yacc.c:1646  */
    break;

  case 1449:
#line 10124 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 18821 "parser.c" /* yacc.c:1646  */
    break;

  case 1450:
#line 10129 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 18829 "parser.c" /* yacc.c:1646  */
    break;

  case 1451:
#line 10136 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("AUTO", SYN_CLAUSE_5, &check_duplicate);
	set_dispattr_with_conflict ("AUTO", COB_SCREEN_AUTO,
				    "TAB", COB_SCREEN_TAB);
  }
#line 18839 "parser.c" /* yacc.c:1646  */
    break;

  case 1452:
#line 10142 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("TAB", SYN_CLAUSE_6, &check_duplicate);
	set_dispattr_with_conflict ("TAB", COB_SCREEN_TAB,
				    "AUTO", COB_SCREEN_AUTO);
  }
#line 18849 "parser.c" /* yacc.c:1646  */
    break;

  case 1453:
#line 10148 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BELL", SYN_CLAUSE_7, &check_duplicate);
	set_dispattr (COB_SCREEN_BELL);
  }
#line 18858 "parser.c" /* yacc.c:1646  */
    break;

  case 1454:
#line 10153 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BELL", SYN_CLAUSE_7, &check_duplicate);
	/* FIXME: do we need a COB_NO_SCREEN_BELL here?
	set_dispattr (COB_SCREEN_BELL); */
  }
#line 18868 "parser.c" /* yacc.c:1646  */
    break;

  case 1455:
#line 10159 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLINK", SYN_CLAUSE_8, &check_duplicate);
	set_dispattr (COB_SCREEN_BLINK);
  }
#line 18877 "parser.c" /* yacc.c:1646  */
    break;

  case 1456:
#line 10164 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONVERSION", SYN_CLAUSE_9, &check_duplicate);
	CB_PENDING ("ACCEPT CONVERSION");
  }
#line 18886 "parser.c" /* yacc.c:1646  */
    break;

  case 1457:
#line 10169 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FULL", SYN_CLAUSE_10, &check_duplicate);
	set_dispattr (COB_SCREEN_FULL);
  }
#line 18895 "parser.c" /* yacc.c:1646  */
    break;

  case 1458:
#line 10174 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LEFTLINE", SYN_CLAUSE_12, &check_duplicate);
	set_dispattr (COB_SCREEN_LEFTLINE);
  }
#line 18904 "parser.c" /* yacc.c:1646  */
    break;

  case 1459:
#line 10179 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOWER", SYN_CLAUSE_13, &check_duplicate);
	set_dispattr_with_conflict ("LOWER", COB_SCREEN_LOWER,
				    "UPPER", COB_SCREEN_UPPER);
  }
#line 18914 "parser.c" /* yacc.c:1646  */
    break;

  case 1460:
#line 10185 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("HIGHLIGHT", SYN_CLAUSE_11, &check_duplicate);
	set_dispattr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				    "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 18924 "parser.c" /* yacc.c:1646  */
    break;

  case 1461:
#line 10191 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOWLIGHT", SYN_CLAUSE_14, &check_duplicate);
	set_dispattr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				    "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 18934 "parser.c" /* yacc.c:1646  */
    break;

  case 1462:
#line 10198 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SAME phrase");
	/* may not be specified along with the UNDERLINED, BLINK, REVERSED,
	HIGH, LOW, STANDARD, COLOR, FOREGROUND-COLOR, or BACKGROUND-COLOR phrases */
  }
#line 18944 "parser.c" /* yacc.c:1646  */
    break;

  case 1463:
#line 10204 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("STANDARD intensity");
  }
#line 18952 "parser.c" /* yacc.c:1646  */
    break;

  case 1464:
#line 10208 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 18960 "parser.c" /* yacc.c:1646  */
    break;

  case 1465:
#line 10212 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 18968 "parser.c" /* yacc.c:1646  */
    break;

  case 1466:
#line 10216 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 18976 "parser.c" /* yacc.c:1646  */
    break;

  case 1467:
#line 10220 "parser.y" /* yacc.c:1646  */
    {
	if (cb_no_echo_means_secure) {
		check_repeated ("SECURE", SYN_CLAUSE_20, &check_duplicate);
		set_dispattr (COB_SCREEN_SECURE);
	} else {
		check_repeated ("NO-ECHO", SYN_CLAUSE_15, &check_duplicate);
		set_dispattr_with_conflict ("NO-ECHO", COB_SCREEN_NO_ECHO,
					    "SECURE", COB_SCREEN_SECURE);
	}
  }
#line 18991 "parser.c" /* yacc.c:1646  */
    break;

  case 1468:
#line 10231 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("OVERLINE", SYN_CLAUSE_16, &check_duplicate);
	set_dispattr (COB_SCREEN_OVERLINE);
  }
#line 19000 "parser.c" /* yacc.c:1646  */
    break;

  case 1469:
#line 10236 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PROMPT", SYN_CLAUSE_17, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, (yyvsp[0]), NULL, COB_SCREEN_PROMPT);
  }
#line 19009 "parser.c" /* yacc.c:1646  */
    break;

  case 1470:
#line 10241 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PROMPT", SYN_CLAUSE_17, &check_duplicate);
	set_dispattr (COB_SCREEN_PROMPT);
  }
#line 19018 "parser.c" /* yacc.c:1646  */
    break;

  case 1471:
#line 10246 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REQUIRED", SYN_CLAUSE_18, &check_duplicate);
	set_dispattr (COB_SCREEN_REQUIRED);
  }
#line 19027 "parser.c" /* yacc.c:1646  */
    break;

  case 1472:
#line 10251 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REVERSE-VIDEO", SYN_CLAUSE_19, &check_duplicate);
	set_dispattr (COB_SCREEN_REVERSE);
  }
#line 19036 "parser.c" /* yacc.c:1646  */
    break;

  case 1473:
#line 10256 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SECURE", SYN_CLAUSE_20, &check_duplicate);
	set_dispattr_with_conflict ("SECURE", COB_SCREEN_SECURE,
				    "NO-ECHO", COB_SCREEN_NO_ECHO);
  }
#line 19046 "parser.c" /* yacc.c:1646  */
    break;

  case 1474:
#line 10262 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIZE", SYN_CLAUSE_21, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[0]), 0);
  }
#line 19055 "parser.c" /* yacc.c:1646  */
    break;

  case 1475:
#line 10267 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UNDERLINE", SYN_CLAUSE_22, &check_duplicate);
	set_dispattr (COB_SCREEN_UNDERLINE);
  }
#line 19064 "parser.c" /* yacc.c:1646  */
    break;

  case 1476:
#line 10272 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("NO UPDATE", SYN_CLAUSE_23, &check_duplicate);
	set_dispattr_with_conflict ("NO UPDATE", COB_SCREEN_NO_UPDATE,
				    "UPDATE", COB_SCREEN_UPDATE);
  }
#line 19074 "parser.c" /* yacc.c:1646  */
    break;

  case 1477:
#line 10278 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UPDATE", SYN_CLAUSE_24, &check_duplicate);
	set_dispattr_with_conflict ("UPDATE", COB_SCREEN_UPDATE,
				    "NO UPDATE", COB_SCREEN_NO_UPDATE);
  }
#line 19084 "parser.c" /* yacc.c:1646  */
    break;

  case 1478:
#line 10284 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UPPER", SYN_CLAUSE_25, &check_duplicate);
	set_dispattr_with_conflict ("UPPER", COB_SCREEN_UPPER,
				    "LOWER", COB_SCREEN_LOWER);
  }
#line 19094 "parser.c" /* yacc.c:1646  */
    break;

  case 1479:
#line 10290 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_26, &check_duplicate);
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_27, &check_duplicate);
	CB_PENDING ("COLOR");
  }
#line 19104 "parser.c" /* yacc.c:1646  */
    break;

  case 1480:
#line 10296 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_26, &check_duplicate);
	set_attribs ((yyvsp[0]), NULL, NULL, NULL, NULL, NULL, 0);
  }
#line 19113 "parser.c" /* yacc.c:1646  */
    break;

  case 1481:
#line 10301 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_27, &check_duplicate);
	set_attribs (NULL, (yyvsp[0]), NULL, NULL, NULL, NULL, 0);
  }
#line 19122 "parser.c" /* yacc.c:1646  */
    break;

  case 1482:
#line 10306 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL UP", SYN_CLAUSE_28, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN);
  }
#line 19133 "parser.c" /* yacc.c:1646  */
    break;

  case 1483:
#line 10313 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL DOWN", SYN_CLAUSE_19, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP);
  }
#line 19144 "parser.c" /* yacc.c:1646  */
    break;

  case 1484:
#line 10320 "parser.y" /* yacc.c:1646  */
    {
	check_repeated (_("TIME-OUT or BEFORE TIME clauses"), SYN_CLAUSE_4,
			&check_duplicate);
	set_attribs (NULL, NULL, NULL, (yyvsp[0]), NULL, NULL, 0);
  }
#line 19154 "parser.c" /* yacc.c:1646  */
    break;

  case 1488:
#line 10333 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONTROL KEY", SYN_CLAUSE_29, &check_duplicate);
	CB_PENDING ("CONTROL KEY");
#if 0 /* should generate the following *after* the ACCEPT is finished */
	cb_emit_accept_escape_key ((yyvsp[0]));
#endif
  }
#line 19166 "parser.c" /* yacc.c:1646  */
    break;

  case 1497:
#line 10361 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), ACCEPT);
  }
#line 19174 "parser.c" /* yacc.c:1646  */
    break;

  case 1498:
#line 10365 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), ACCEPT);
# if 0 /* activate only for debugging purposes for attribs
	FIXME: Replace by DEBUG_LOG function */
	if (current_statement->attr_ptr) {
		print_bits (current_statement->attr_ptr->dispattrs);
	} else {
		fputs("No Attribs", stderr);
	}
#endif
  }
#line 19190 "parser.c" /* yacc.c:1646  */
    break;

  case 1499:
#line 10383 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ADD", TERM_ADD);
  }
#line 19198 "parser.c" /* yacc.c:1646  */
    break;

  case 1501:
#line 10392 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '+', cb_build_binary_list ((yyvsp[-3]), '+'));
  }
#line 19206 "parser.c" /* yacc.c:1646  */
    break;

  case 1502:
#line 10396 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-3])) {
		cb_list_add ((yyvsp[-4]), (yyvsp[-3]));
	}
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_list ((yyvsp[-4]), '+'));
  }
#line 19217 "parser.c" /* yacc.c:1646  */
    break;

  case 1503:
#line 10403 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_corresponding (cb_build_add, (yyvsp[-2]), (yyvsp[-4]), (yyvsp[-1]));
  }
#line 19225 "parser.c" /* yacc.c:1646  */
    break;

  case 1504:
#line 10407 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("ADD TABLE");
	cb_emit_tab_arithmetic (cb_build_add, (yyvsp[-4]), (yyvsp[-6]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 19234 "parser.c" /* yacc.c:1646  */
    break;

  case 1505:
#line 10414 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 19240 "parser.c" /* yacc.c:1646  */
    break;

  case 1506:
#line 10415 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 19246 "parser.c" /* yacc.c:1646  */
    break;

  case 1507:
#line 10420 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), ADD);
  }
#line 19254 "parser.c" /* yacc.c:1646  */
    break;

  case 1508:
#line 10424 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), ADD);
  }
#line 19262 "parser.c" /* yacc.c:1646  */
    break;

  case 1509:
#line 10434 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ALLOCATE", 0);
	cobc_cs_check = CB_CS_ALLOCATE;
	current_statement->flag_no_based = 1;
  }
#line 19272 "parser.c" /* yacc.c:1646  */
    break;

  case 1511:
#line 10444 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_allocate ((yyvsp[-3]), (yyvsp[0]), NULL, (yyvsp[-2]));
  }
#line 19280 "parser.c" /* yacc.c:1646  */
    break;

  case 1512:
#line 10448 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		cb_error_x (CB_TREE (current_statement),
			    _("ALLOCATE CHARACTERS requires RETURNING clause"));
	} else {
		cb_emit_allocate (NULL, (yyvsp[0]), (yyvsp[-4]), (yyvsp[-2]));
	}
  }
#line 19293 "parser.c" /* yacc.c:1646  */
    break;

  case 1514:
#line 10461 "parser.y" /* yacc.c:1646  */
    {
	int adressing = cb_get_int ((yyvsp[0]));

	if (adressing == 24
	 || adressing == 31) {
		cb_warning (COBC_WARN_FILLER, _("ignoring %s phrase"), "LOC");
	} else {
		cb_error (_("addressing mode should be either 24 or 31 bit"));
	}
  }
#line 19308 "parser.c" /* yacc.c:1646  */
    break;

  case 1515:
#line 10473 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 19314 "parser.c" /* yacc.c:1646  */
    break;

  case 1516:
#line 10474 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 19320 "parser.c" /* yacc.c:1646  */
    break;

  case 1517:
#line 10482 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ALTER", 0);
	cb_verify (cb_alter_statement, "ALTER");
  }
#line 19329 "parser.c" /* yacc.c:1646  */
    break;

  case 1521:
#line 10496 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_alter ((yyvsp[-3]), (yyvsp[0]));
  }
#line 19337 "parser.c" /* yacc.c:1646  */
    break;

  case 1524:
#line 10508 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CALL", TERM_CALL);
	cobc_cs_check = CB_CS_CALL;
	call_nothing = 0;
	cobc_allow_program_name = 1;
	backup_current_pos ();
  }
#line 19349 "parser.c" /* yacc.c:1646  */
    break;

  case 1525:
#line 10517 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 19357 "parser.c" /* yacc.c:1646  */
    break;

  case 1526:
#line 10524 "parser.y" /* yacc.c:1646  */
    {
	cobc_allow_program_name = 0;
  }
#line 19365 "parser.c" /* yacc.c:1646  */
    break;

  case 1527:
#line 10531 "parser.y" /* yacc.c:1646  */
    {
	int call_conv = 0;
	int call_conv_local = 0;

	if (current_program->prog_type == COB_MODULE_TYPE_PROGRAM
	    && !current_program->flag_recursive
	    && is_recursive_call ((yyvsp[-5]))) {
		cb_warning_x (COBC_WARN_FILLER, (yyvsp[-5]),
			_("recursive program call - assuming RECURSIVE attribute"));
		current_program->flag_recursive = 1;
	}
	call_conv = current_call_convention;
	if ((CB_PAIR_X ((yyvsp[0])) != NULL)
	 && (call_conv & CB_CONV_STATIC_LINK)) {
		cb_warning_x (COBC_WARN_FILLER, (yyvsp[-5]),
		    _("STATIC CALL convention ignored because of ON EXCEPTION"));
		call_conv &= ~CB_CONV_STATIC_LINK;
	}
	if ((yyvsp[-7])) {
		if (CB_INTEGER_P ((yyvsp[-7]))) {
			call_conv_local = CB_INTEGER ((yyvsp[-7]))->val;
			if ((CB_PAIR_X ((yyvsp[0])) != NULL)
			 && (call_conv_local & CB_CONV_STATIC_LINK)) {
				cb_error_x ((yyvsp[-7]), _("%s and %s are mutually exclusive"),
					"STATIC CALL", "ON EXCEPTION");
				call_conv_local &= ~CB_CONV_STATIC_LINK;
			}
			call_conv |= call_conv_local;
			if (CB_INTEGER ((yyvsp[-7]))->val & CB_CONV_COBOL) {
				call_conv &= ~CB_CONV_STDCALL;
			} else {
				call_conv &= ~CB_CONV_COBOL;
			}
		} else {
			call_conv = cb_get_int((yyvsp[-7]));
		}
	}
	/* For CALL ... RETURNING NOTHING, set the call convention bit */
	if (call_nothing) {
		call_conv |= CB_CONV_NO_RET_UPD;
	}
	cb_emit_call ((yyvsp[-5]), (yyvsp[-2]), (yyvsp[-1]), CB_PAIR_X ((yyvsp[0])), CB_PAIR_Y ((yyvsp[0])),
		      cb_int (call_conv), (yyvsp[-6]), (yyvsp[-3]), backup_source_line);
  }
#line 19414 "parser.c" /* yacc.c:1646  */
    break;

  case 1528:
#line 10579 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19422 "parser.c" /* yacc.c:1646  */
    break;

  case 1529:
#line 10583 "parser.y" /* yacc.c:1646  */
    {
	if (current_call_convention & CB_CONV_COBOL) {
		(yyval) = cb_int (CB_CONV_STATIC_LINK | CB_CONV_COBOL);
	} else {
		(yyval) = cb_int (CB_CONV_STATIC_LINK);
	}
  }
#line 19434 "parser.c" /* yacc.c:1646  */
    break;

  case 1530:
#line 10591 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_STDCALL);
  }
#line 19442 "parser.c" /* yacc.c:1646  */
    break;

  case 1531:
#line 10595 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (0);
  }
#line 19450 "parser.c" /* yacc.c:1646  */
    break;

  case 1532:
#line 10599 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_ref ((yyvsp[0]));
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME(x)->token != CB_FEATURE_CONVENTION) {
			cb_error_x ((yyvsp[0]), _("invalid mnemonic name"));
			(yyval) = NULL;
		} else {
			(yyval) = CB_SYSTEM_NAME(x)->value;
		}
	} else {
		(yyval) = NULL;
	}
  }
#line 19470 "parser.c" /* yacc.c:1646  */
    break;

  case 1533:
#line 10618 "parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P ((yyvsp[0]))) {
		cb_trim_program_id ((yyvsp[0]));
	}
  }
#line 19480 "parser.c" /* yacc.c:1646  */
    break;

  case 1534:
#line 10624 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
	/* hack to push the prototype name */
	if ((yyvsp[0]) && CB_REFERENCE_P ((yyvsp[0]))) {
		if ((yyvsp[-1])) {
			cb_warning_x (COBC_WARN_FILLER, (yyvsp[-1]), _("id/literal ignored, using prototype name"));
		}
		(yyval) = (yyvsp[0]);
	} else if ((yyvsp[-1]) && CB_LITERAL_P ((yyvsp[-1]))) {
		(yyval) = (yyvsp[-1]);
	} else {
		cb_error (_("NESTED phrase is only valid with literal"));
		(yyval) = cb_error_node;
	}
  }
#line 19500 "parser.c" /* yacc.c:1646  */
    break;

  case 1535:
#line 10643 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19508 "parser.c" /* yacc.c:1646  */
    break;

  case 1536:
#line 10648 "parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P ((yyvsp[-1]))) {
		cb_trim_program_id ((yyvsp[-1]));
	}
	(yyval) = (yyvsp[-1]);
  }
#line 19519 "parser.c" /* yacc.c:1646  */
    break;

  case 1537:
#line 10658 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("NESTED phrase for CALL statement");
  }
#line 19527 "parser.c" /* yacc.c:1646  */
    break;

  case 1539:
#line 10666 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19535 "parser.c" /* yacc.c:1646  */
    break;

  case 1540:
#line 10670 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 19544 "parser.c" /* yacc.c:1646  */
    break;

  case 1541:
#line 10675 "parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > MAX_CALL_FIELD_PARAMS) {
		cb_error_x (CB_TREE (current_statement),
			    _("number of parameters exceeds maximum %d"),
			    MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 19557 "parser.c" /* yacc.c:1646  */
    break;

  case 1542:
#line 10686 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 19563 "parser.c" /* yacc.c:1646  */
    break;

  case 1543:
#line 10688 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 19569 "parser.c" /* yacc.c:1646  */
    break;

  case 1544:
#line 10693 "parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error_x (CB_TREE (current_statement),
			    _("OMITTED only allowed when parameters are passed BY REFERENCE"));
	}
	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), cb_null);
  }
#line 19581 "parser.c" /* yacc.c:1646  */
    break;

  case 1545:
#line 10701 "parser.y" /* yacc.c:1646  */
    {
	int	save_mode;	/* internal single parameter only mode */

	save_mode = call_mode;
	if (call_mode != CB_CALL_BY_REFERENCE) {
		if (CB_FILE_P ((yyvsp[0])) || (CB_REFERENCE_P ((yyvsp[0])) &&
		    CB_FILE_P (CB_REFERENCE ((yyvsp[0]))->value))) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid file name reference"));
		} else if (call_mode == CB_CALL_BY_VALUE) {
			/* FIXME: compiler configuration needed, IBM allows one-byte
			          alphanumeric items [--> a `char`], too, while
			          COBOL 2002/2014 allow only numeric literals
			   --> revise after rw-merge */
			if (cb_category_is_alpha ((yyvsp[0]))) {
				cb_warning_x (COBC_WARN_FILLER, (yyvsp[0]),
					      _("BY CONTENT assumed for alphanumeric item '%s'"),
						  cb_name ((yyvsp[0])));
				call_mode = CB_CALL_BY_CONTENT;
			} else if (cb_category_is_national ((yyvsp[0]))) {
				cb_warning_x (COBC_WARN_FILLER, (yyvsp[0]),
					      _("BY CONTENT assumed for national item '%s'"),
						  cb_name ((yyvsp[0])));
				call_mode = CB_CALL_BY_CONTENT;
			}
		}
	}
	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), (yyvsp[0]));
	CB_SIZES ((yyval)) = size_mode;
	call_mode = save_mode;
  }
#line 19617 "parser.c" /* yacc.c:1646  */
    break;

  case 1547:
#line 10737 "parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 19625 "parser.c" /* yacc.c:1646  */
    break;

  case 1548:
#line 10741 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY CONTENT");
	} else {
		call_mode = CB_CALL_BY_CONTENT;
	}
  }
#line 19638 "parser.c" /* yacc.c:1646  */
    break;

  case 1549:
#line 10750 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 19651 "parser.c" /* yacc.c:1646  */
    break;

  case 1550:
#line 10762 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19659 "parser.c" /* yacc.c:1646  */
    break;

  case 1551:
#line 10766 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 19667 "parser.c" /* yacc.c:1646  */
    break;

  case 1552:
#line 10770 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 19675 "parser.c" /* yacc.c:1646  */
    break;

  case 1553:
#line 10774 "parser.y" /* yacc.c:1646  */
    {
	call_nothing = CB_CONV_NO_RET_UPD;
	(yyval) = cb_null;
  }
#line 19684 "parser.c" /* yacc.c:1646  */
    break;

  case 1554:
#line 10779 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01 or 77"));
			(yyval) = NULL;
		} else if (f->storage != CB_STORAGE_LINKAGE &&
			   !f->flag_item_based) {
			cb_error (_("RETURNING item must be a LINKAGE SECTION item or have BASED clause"));
			(yyval) = NULL;
		} else {
			(yyval) = cb_build_address ((yyvsp[0]));
		}
	} else {
		(yyval) = NULL;
	}
  }
#line 19708 "parser.c" /* yacc.c:1646  */
    break;

  case 1559:
#line 10812 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR (NULL, NULL);
  }
#line 19716 "parser.c" /* yacc.c:1646  */
    break;

  case 1560:
#line 10816 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-1]), (yyvsp[0]));
  }
#line 19724 "parser.c" /* yacc.c:1646  */
    break;

  case 1561:
#line 10820 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-1]));
  }
#line 19736 "parser.c" /* yacc.c:1646  */
    break;

  case 1562:
#line 10831 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19744 "parser.c" /* yacc.c:1646  */
    break;

  case 1563:
#line 10835 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 19752 "parser.c" /* yacc.c:1646  */
    break;

  case 1564:
#line 10842 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 19760 "parser.c" /* yacc.c:1646  */
    break;

  case 1565:
#line 10846 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_call_overflow, "ON OVERFLOW");
	(yyval) = (yyvsp[0]);
  }
#line 19769 "parser.c" /* yacc.c:1646  */
    break;

  case 1566:
#line 10854 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19777 "parser.c" /* yacc.c:1646  */
    break;

  case 1567:
#line 10858 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 19785 "parser.c" /* yacc.c:1646  */
    break;

  case 1568:
#line 10865 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 19793 "parser.c" /* yacc.c:1646  */
    break;

  case 1569:
#line 10872 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), CALL);
  }
#line 19801 "parser.c" /* yacc.c:1646  */
    break;

  case 1570:
#line 10876 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), CALL);
  }
#line 19809 "parser.c" /* yacc.c:1646  */
    break;

  case 1571:
#line 10886 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CANCEL", 0);
	cobc_allow_program_name = 1;
  }
#line 19818 "parser.c" /* yacc.c:1646  */
    break;

  case 1572:
#line 10891 "parser.y" /* yacc.c:1646  */
    {
	cobc_allow_program_name = 0;
  }
#line 19826 "parser.c" /* yacc.c:1646  */
    break;

  case 1573:
#line 10898 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_cancel ((yyvsp[0]));
  }
#line 19834 "parser.c" /* yacc.c:1646  */
    break;

  case 1574:
#line 10902 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_cancel ((yyvsp[0]));
  }
#line 19842 "parser.c" /* yacc.c:1646  */
    break;

  case 1576:
#line 10910 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
  }
#line 19850 "parser.c" /* yacc.c:1646  */
    break;

  case 1577:
#line 10919 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CLOSE", 0);
  }
#line 19858 "parser.c" /* yacc.c:1646  */
    break;

  case 1581:
#line 10932 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_close ((yyvsp[-1]), (yyvsp[0]));
  }
#line 19867 "parser.c" /* yacc.c:1646  */
    break;

  case 1582:
#line 10937 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_close ((yyvsp[-1]), (yyvsp[0]));
  }
#line 19876 "parser.c" /* yacc.c:1646  */
    break;

  case 1583:
#line 10944 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_NORMAL); }
#line 19882 "parser.c" /* yacc.c:1646  */
    break;

  case 1584:
#line 10945 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_UNIT); }
#line 19888 "parser.c" /* yacc.c:1646  */
    break;

  case 1585:
#line 10946 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_UNIT_REMOVAL); }
#line 19894 "parser.c" /* yacc.c:1646  */
    break;

  case 1586:
#line 10947 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_NO_REWIND); }
#line 19900 "parser.c" /* yacc.c:1646  */
    break;

  case 1587:
#line 10948 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_LOCK); }
#line 19906 "parser.c" /* yacc.c:1646  */
    break;

  case 1588:
#line 10953 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "CLOSE WINDOW";
  }
#line 19915 "parser.c" /* yacc.c:1646  */
    break;

  case 1589:
#line 10958 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_close_window ((yyvsp[-1]), (yyvsp[0]));
  }
#line 19923 "parser.c" /* yacc.c:1646  */
    break;

  case 1590:
#line 10964 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 19929 "parser.c" /* yacc.c:1646  */
    break;

  case 1591:
#line 10965 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 19935 "parser.c" /* yacc.c:1646  */
    break;

  case 1592:
#line 10973 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("COMPUTE", TERM_COMPUTE);
  }
#line 19943 "parser.c" /* yacc.c:1646  */
    break;

  case 1594:
#line 10982 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-3]), 0, (yyvsp[-1]));
  }
#line 19951 "parser.c" /* yacc.c:1646  */
    break;

  case 1595:
#line 10989 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), COMPUTE);
  }
#line 19959 "parser.c" /* yacc.c:1646  */
    break;

  case 1596:
#line 10993 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), COMPUTE);
  }
#line 19967 "parser.c" /* yacc.c:1646  */
    break;

  case 1597:
#line 11003 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("COMMIT", 0);
	cb_emit_commit ();
  }
#line 19976 "parser.c" /* yacc.c:1646  */
    break;

  case 1598:
#line 11014 "parser.y" /* yacc.c:1646  */
    {
	size_t	save_unreached;

	/* Do not check unreached for CONTINUE */
	save_unreached = check_unreached;
	check_unreached = 0;
	begin_statement ("CONTINUE", 0);
	cb_emit_continue ();
	check_unreached = (unsigned int) save_unreached;
  }
#line 19991 "parser.c" /* yacc.c:1646  */
    break;

  case 1599:
#line 11031 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DESTROY", 0);
	CB_PENDING ("GRAPHICAL CONTROL");
  }
#line 20000 "parser.c" /* yacc.c:1646  */
    break;

  case 1601:
#line 11040 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_destroy (NULL);
  }
#line 20008 "parser.c" /* yacc.c:1646  */
    break;

  case 1602:
#line 11047 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_destroy ((yyvsp[0]));
  }
#line 20016 "parser.c" /* yacc.c:1646  */
    break;

  case 1603:
#line 11057 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DELETE", TERM_DELETE);
  }
#line 20024 "parser.c" /* yacc.c:1646  */
    break;

  case 1605:
#line 11066 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_delete ((yyvsp[-3]));
  }
#line 20032 "parser.c" /* yacc.c:1646  */
    break;

  case 1607:
#line 11074 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_delete_file ((yyvsp[0]));
  }
#line 20041 "parser.c" /* yacc.c:1646  */
    break;

  case 1608:
#line 11079 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_delete_file ((yyvsp[0]));
  }
#line 20050 "parser.c" /* yacc.c:1646  */
    break;

  case 1609:
#line 11087 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DELETE);
  }
#line 20058 "parser.c" /* yacc.c:1646  */
    break;

  case 1610:
#line 11091 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DELETE);
  }
#line 20066 "parser.c" /* yacc.c:1646  */
    break;

  case 1611:
#line 11101 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DISABLE", 0);
  }
#line 20074 "parser.c" /* yacc.c:1646  */
    break;

  case 1615:
#line 11115 "parser.y" /* yacc.c:1646  */
    {
	  /* Add cb_verify for <= COBOL-85 */
  }
#line 20082 "parser.c" /* yacc.c:1646  */
    break;

  case 1621:
#line 11133 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DISPLAY", TERM_DISPLAY);
	cobc_cs_check = CB_CS_DISPLAY;
	display_type = UNKNOWN_DISPLAY;
	is_first_display_item = 1;
  }
#line 20093 "parser.c" /* yacc.c:1646  */
    break;

  case 1623:
#line 11145 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_env_name ((yyvsp[-2]));
  }
#line 20101 "parser.c" /* yacc.c:1646  */
    break;

  case 1624:
#line 11149 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_env_value ((yyvsp[-2]));
  }
#line 20109 "parser.c" /* yacc.c:1646  */
    break;

  case 1625:
#line 11153 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arg_number ((yyvsp[-2]));
  }
#line 20117 "parser.c" /* yacc.c:1646  */
    break;

  case 1626:
#line 11157 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_command_line ((yyvsp[-2]));
  }
#line 20125 "parser.c" /* yacc.c:1646  */
    break;

  case 1634:
#line 11171 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) != NULL) {
		error_if_different_display_type ((yyvsp[0]), NULL, NULL, NULL);
		cb_emit_display ((yyvsp[0]), NULL, cb_int1, NULL, NULL, 0,
				 display_type);
	}
  }
#line 20137 "parser.c" /* yacc.c:1646  */
    break;

  case 1635:
#line 11179 "parser.y" /* yacc.c:1646  */
    {
	set_display_type ((yyvsp[0]), NULL, NULL, NULL);
	cb_emit_display ((yyvsp[0]), NULL, cb_int1, NULL, NULL, 1,
			 display_type);
  }
#line 20147 "parser.c" /* yacc.c:1646  */
    break;

  case 1638:
#line 11193 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	advancing_value = cb_int1;
	upon_value = NULL;
	line_column = NULL;
  }
#line 20159 "parser.c" /* yacc.c:1646  */
    break;

  case 1639:
#line 11201 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2]) == cb_null) {
		/* Emit DISPLAY OMITTED. */
		CB_UNFINISHED_X (CB_TREE(current_statement), "DISPLAY OMITTED");
		error_if_no_advancing_in_screen_display (advancing_value);
	}

	/* Emit device or screen DISPLAY. */

	/*
	  Check that disp_list does not contain an invalid mix of fields.
	*/
	if (display_type == UNKNOWN_DISPLAY) {
		set_display_type ((yyvsp[-2]), upon_value, line_column,
				  current_statement->attr_ptr);
	} else {
		error_if_different_display_type ((yyvsp[-2]), upon_value,
						 line_column,
						 current_statement->attr_ptr);
	}

	if (display_type == SCREEN_DISPLAY
	    || display_type == FIELD_ON_SCREEN_DISPLAY) {
		error_if_no_advancing_in_screen_display (advancing_value);
	}

	cb_emit_display ((yyvsp[-2]), upon_value, advancing_value, line_column,
			 current_statement->attr_ptr,
			 is_first_display_item, display_type);

	is_first_display_item = 0;
  }
#line 20196 "parser.c" /* yacc.c:1646  */
    break;

  case 1640:
#line 11237 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 20204 "parser.c" /* yacc.c:1646  */
    break;

  case 1641:
#line 11241 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 20212 "parser.c" /* yacc.c:1646  */
    break;

  case 1648:
#line 11263 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UPON", SYN_CLAUSE_1, &check_duplicate);
  }
#line 20220 "parser.c" /* yacc.c:1646  */
    break;

  case 1649:
#line 11267 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("NO ADVANCING", SYN_CLAUSE_2, &check_duplicate);
	advancing_value = cb_int0;
  }
#line 20229 "parser.c" /* yacc.c:1646  */
    break;

  case 1650:
#line 11272 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
#line 20237 "parser.c" /* yacc.c:1646  */
    break;

  case 1653:
#line 11281 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_build_display_mnemonic ((yyvsp[0]));
  }
#line 20245 "parser.c" /* yacc.c:1646  */
    break;

  case 1654:
#line 11285 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_build_display_name ((yyvsp[0]));
  }
#line 20253 "parser.c" /* yacc.c:1646  */
    break;

  case 1655:
#line 11289 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_int2;
  }
#line 20261 "parser.c" /* yacc.c:1646  */
    break;

  case 1656:
#line 11293 "parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_null;
  }
#line 20269 "parser.c" /* yacc.c:1646  */
    break;

  case 1659:
#line 11305 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = SYN_CLAUSE_10;
	check_line_col_duplicate = 0;
	line_column = NULL;
	set_dispattr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				    "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 20281 "parser.c" /* yacc.c:1646  */
    break;

  case 1660:
#line 11313 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_display (CB_LIST_INIT (cb_space), cb_null, cb_int1, line_column, NULL, 1, FIELD_ON_SCREEN_DISPLAY);
  }
#line 20289 "parser.c" /* yacc.c:1646  */
    break;

  case 1661:
#line 11322 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_display ((yyvsp[-1]), cb_null, cb_int1, line_column, NULL, 1, FIELD_ON_SCREEN_DISPLAY);
  }
#line 20297 "parser.c" /* yacc.c:1646  */
    break;

  case 1662:
#line 11328 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
  }
#line 20307 "parser.c" /* yacc.c:1646  */
    break;

  case 1663:
#line 11334 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 20315 "parser.c" /* yacc.c:1646  */
    break;

  case 1664:
#line 11341 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 20323 "parser.c" /* yacc.c:1646  */
    break;

  case 1665:
#line 11345 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 20331 "parser.c" /* yacc.c:1646  */
    break;

  case 1668:
#line 11355 "parser.y" /* yacc.c:1646  */
    {
	set_dispattr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				    "ERASE EOL", COB_SCREEN_ERASE_EOL);
	(yyval) = cb_space;
  }
#line 20341 "parser.c" /* yacc.c:1646  */
    break;

  case 1669:
#line 11365 "parser.y" /* yacc.c:1646  */
    {
	CB_UNFINISHED_X (CB_TREE(current_statement), "DISPLAY MESSAGE");
	upon_value = NULL;
  }
#line 20350 "parser.c" /* yacc.c:1646  */
    break;

  case 1670:
#line 11370 "parser.y" /* yacc.c:1646  */
    {
	/* for now: minimal support for display and prompt only */
	if (upon_value) {
		cb_emit_display (CB_LIST_INIT (upon_value), NULL, NULL, NULL,
				 NULL, 1, FIELD_ON_SCREEN_DISPLAY);
	}
	cb_emit_display ((yyvsp[-2]), NULL, NULL, NULL,
			 NULL, 1, FIELD_ON_SCREEN_DISPLAY);
	cb_emit_accept (cb_null, NULL, NULL);
  }
#line 20365 "parser.c" /* yacc.c:1646  */
    break;

  case 1675:
#line 11394 "parser.y" /* yacc.c:1646  */
    {
	upon_value = (yyvsp[0]);
  }
#line 20373 "parser.c" /* yacc.c:1646  */
    break;

  case 1680:
#line 11405 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY WINDOW";
  }
#line 20382 "parser.c" /* yacc.c:1646  */
    break;

  case 1681:
#line 11410 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
  }
#line 20393 "parser.c" /* yacc.c:1646  */
    break;

  case 1682:
#line 11417 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_display_window (NULL, upon_value, (yyvsp[-2]), line_column,
			 current_statement->attr_ptr);
  }
#line 20402 "parser.c" /* yacc.c:1646  */
    break;

  case 1685:
#line 11430 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY FLOATING WINDOW";
  }
#line 20411 "parser.c" /* yacc.c:1646  */
    break;

  case 1686:
#line 11435 "parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
  }
#line 20422 "parser.c" /* yacc.c:1646  */
    break;

  case 1687:
#line 11442 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-5])) {
		/* TODO: set "CELL WIDTH" and "CELL HEIGHT" to "LABEL FONT" */
		/* if not set already */
	}
	cb_emit_display_window (cb_int0, upon_value, (yyvsp[-2]), line_column,
			 current_statement->attr_ptr);
  }
#line 20435 "parser.c" /* yacc.c:1646  */
    break;

  case 1688:
#line 11454 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY INITIAL WINDOW";
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
	/* TODO: initialize attributes for SHADOW, BOTTOM */
  }
#line 20449 "parser.c" /* yacc.c:1646  */
    break;

  case 1689:
#line 11464 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-3])) {
		/* TODO: set "CELL WIDTH" and "CELL HEIGHT" to "LABEL FONT" */
		/* if not set already */
	}
	cb_emit_display_window ((yyvsp[-4]), upon_value, NULL, line_column,
			 current_statement->attr_ptr);
  }
#line 20462 "parser.c" /* yacc.c:1646  */
    break;

  case 1690:
#line 11475 "parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int1;}
#line 20468 "parser.c" /* yacc.c:1646  */
    break;

  case 1691:
#line 11476 "parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int2;}
#line 20474 "parser.c" /* yacc.c:1646  */
    break;

  case 1692:
#line 11477 "parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int3;}
#line 20480 "parser.c" /* yacc.c:1646  */
    break;

  case 1693:
#line 11481 "parser.y" /* yacc.c:1646  */
    {(yyval) = NULL;}
#line 20486 "parser.c" /* yacc.c:1646  */
    break;

  case 1694:
#line 11482 "parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int1;}
#line 20492 "parser.c" /* yacc.c:1646  */
    break;

  case 1695:
#line 11487 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 20500 "parser.c" /* yacc.c:1646  */
    break;

  case 1696:
#line 11491 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 20508 "parser.c" /* yacc.c:1646  */
    break;

  case 1697:
#line 11498 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		if (f->usage != CB_USAGE_HNDL_WINDOW
		 && f->usage != CB_USAGE_HNDL_SUBWINDOW) {
			cb_error_x ((yyvsp[0]), _("HANDLE must be a %s HANDLE"), "WINDOW");
		}
	}
	(yyval) = (yyvsp[0]);
  }
#line 20525 "parser.c" /* yacc.c:1646  */
    break;

  case 1698:
#line 11511 "parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		if (f->usage != CB_USAGE_HNDL) {
			cb_error_x ((yyvsp[0]), _("HANDLE must be a generic HANDLE"));
		}
	}
	(yyval) = (yyvsp[0]);
  }
#line 20541 "parser.c" /* yacc.c:1646  */
    break;

  case 1699:
#line 11523 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 20549 "parser.c" /* yacc.c:1646  */
    break;

  case 1703:
#line 11538 "parser.y" /* yacc.c:1646  */
    {
	/* TODO: store */
  }
#line 20557 "parser.c" /* yacc.c:1646  */
    break;

  case 1710:
#line 11550 "parser.y" /* yacc.c:1646  */
    { /* TODO: set attribute */ }
#line 20563 "parser.c" /* yacc.c:1646  */
    break;

  case 1711:
#line 11553 "parser.y" /* yacc.c:1646  */
    { /* TODO: set attribute */ }
#line 20569 "parser.c" /* yacc.c:1646  */
    break;

  case 1712:
#line 11557 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 20575 "parser.c" /* yacc.c:1646  */
    break;

  case 1713:
#line 11558 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 20581 "parser.c" /* yacc.c:1646  */
    break;

  case 1714:
#line 11559 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 20587 "parser.c" /* yacc.c:1646  */
    break;

  case 1715:
#line 11563 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 20593 "parser.c" /* yacc.c:1646  */
    break;

  case 1716:
#line 11564 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 20599 "parser.c" /* yacc.c:1646  */
    break;

  case 1717:
#line 11565 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 20605 "parser.c" /* yacc.c:1646  */
    break;

  case 1718:
#line 11566 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 20611 "parser.c" /* yacc.c:1646  */
    break;

  case 1723:
#line 11582 "parser.y" /* yacc.c:1646  */
    {
	if (upon_value) {
		emit_duplicate_clause_message("POP-UP AREA");
	}
	upon_value = (yyvsp[0]);
  }
#line 20622 "parser.c" /* yacc.c:1646  */
    break;

  case 1724:
#line 11592 "parser.y" /* yacc.c:1646  */
    {
	if (!strcmp (current_statement->name, "DISPLAY WINDOW")) {
		cb_error_x ((yyvsp[0]), _("HANDLE clause invalid for %s"),
			current_statement->name);
		upon_value = cb_error_node;
	} else{
		if (upon_value) {
			emit_duplicate_clause_message("POP-UP AREA / HANDLE IN");
		}
		upon_value = (yyvsp[0]);
	}
  }
#line 20639 "parser.c" /* yacc.c:1646  */
    break;

  case 1725:
#line 11608 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BELL", SYN_CLAUSE_4, &check_duplicate);
	set_dispattr (COB_SCREEN_BELL);
  }
#line 20648 "parser.c" /* yacc.c:1646  */
    break;

  case 1726:
#line 11613 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLANK LINE", SYN_CLAUSE_5, &check_duplicate);
	set_dispattr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
				    "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
#line 20658 "parser.c" /* yacc.c:1646  */
    break;

  case 1727:
#line 11619 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLANK SCREEN", SYN_CLAUSE_6, &check_duplicate);
	set_dispattr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				    "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
#line 20668 "parser.c" /* yacc.c:1646  */
    break;

  case 1728:
#line 11625 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLINK", SYN_CLAUSE_7, &check_duplicate);
	set_dispattr (COB_SCREEN_BLINK);
  }
#line 20677 "parser.c" /* yacc.c:1646  */
    break;

  case 1729:
#line 11630 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONVERSION", SYN_CLAUSE_8, &check_duplicate);
	cb_warning (COBC_WARN_FILLER, _("ignoring %s phrase"), "CONVERSION");
  }
#line 20686 "parser.c" /* yacc.c:1646  */
    break;

  case 1730:
#line 11635 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ERASE EOL", SYN_CLAUSE_9, &check_duplicate);
	set_dispattr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
				    "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
#line 20696 "parser.c" /* yacc.c:1646  */
    break;

  case 1731:
#line 11641 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ERASE EOS", SYN_CLAUSE_10, &check_duplicate);
	set_dispattr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				    "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 20706 "parser.c" /* yacc.c:1646  */
    break;

  case 1732:
#line 11647 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("HIGHLIGHT", SYN_CLAUSE_11, &check_duplicate);
	set_dispattr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				    "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 20716 "parser.c" /* yacc.c:1646  */
    break;

  case 1733:
#line 11653 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOWLIGHT", SYN_CLAUSE_12, &check_duplicate);
	set_dispattr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				    "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 20726 "parser.c" /* yacc.c:1646  */
    break;

  case 1734:
#line 11660 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SAME phrase");
	/* may not be specified along with the UNDERLINED, BLINK, REVERSED,
	HIGH, LOW, STANDARD, COLOR, FOREGROUND-COLOR, or BACKGROUND-COLOR phrases */
  }
#line 20736 "parser.c" /* yacc.c:1646  */
    break;

  case 1735:
#line 11666 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("STANDARD intensity");
  }
#line 20744 "parser.c" /* yacc.c:1646  */
    break;

  case 1736:
#line 11670 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 20752 "parser.c" /* yacc.c:1646  */
    break;

  case 1737:
#line 11674 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 20760 "parser.c" /* yacc.c:1646  */
    break;

  case 1738:
#line 11678 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 20768 "parser.c" /* yacc.c:1646  */
    break;

  case 1739:
#line 11682 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("OVERLINE", SYN_CLAUSE_13, &check_duplicate);
	set_dispattr (COB_SCREEN_OVERLINE);
  }
#line 20777 "parser.c" /* yacc.c:1646  */
    break;

  case 1740:
#line 11687 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REVERSE-VIDEO", SYN_CLAUSE_14, &check_duplicate);
	set_dispattr (COB_SCREEN_REVERSE);
  }
#line 20786 "parser.c" /* yacc.c:1646  */
    break;

  case 1741:
#line 11692 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIZE", SYN_CLAUSE_15, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[0]), 0);
  }
#line 20795 "parser.c" /* yacc.c:1646  */
    break;

  case 1742:
#line 11697 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UNDERLINE", SYN_CLAUSE_16, &check_duplicate);
	set_dispattr (COB_SCREEN_UNDERLINE);
  }
#line 20804 "parser.c" /* yacc.c:1646  */
    break;

  case 1743:
#line 11702 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_17, &check_duplicate);
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_18, &check_duplicate);
	CB_PENDING ("COLOR");
  }
#line 20814 "parser.c" /* yacc.c:1646  */
    break;

  case 1744:
#line 11708 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_17, &check_duplicate);
	set_attribs ((yyvsp[0]), NULL, NULL, NULL, NULL, NULL, 0);
  }
#line 20823 "parser.c" /* yacc.c:1646  */
    break;

  case 1745:
#line 11713 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_18, &check_duplicate);
	set_attribs (NULL, (yyvsp[0]), NULL, NULL, NULL, NULL, 0);
  }
#line 20832 "parser.c" /* yacc.c:1646  */
    break;

  case 1746:
#line 11718 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL UP", SYN_CLAUSE_19, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN);
  }
#line 20843 "parser.c" /* yacc.c:1646  */
    break;

  case 1747:
#line 11725 "parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL DOWN", SYN_CLAUSE_20, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP);
  }
#line 20854 "parser.c" /* yacc.c:1646  */
    break;

  case 1748:
#line 11735 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DISPLAY);
  }
#line 20862 "parser.c" /* yacc.c:1646  */
    break;

  case 1749:
#line 11739 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DISPLAY);
  }
#line 20870 "parser.c" /* yacc.c:1646  */
    break;

  case 1750:
#line 11749 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DIVIDE", TERM_DIVIDE);
  }
#line 20878 "parser.c" /* yacc.c:1646  */
    break;

  case 1752:
#line 11758 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '/', (yyvsp[-3]));
  }
#line 20886 "parser.c" /* yacc.c:1646  */
    break;

  case 1753:
#line 11762 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-3]), '/', (yyvsp[-5])));
  }
#line 20894 "parser.c" /* yacc.c:1646  */
    break;

  case 1754:
#line 11766 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-5]), '/', (yyvsp[-3])));
  }
#line 20902 "parser.c" /* yacc.c:1646  */
    break;

  case 1755:
#line 11770 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_divide ((yyvsp[-5]), (yyvsp[-7]), (yyvsp[-3]), (yyvsp[-1]));
  }
#line 20910 "parser.c" /* yacc.c:1646  */
    break;

  case 1756:
#line 11774 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_divide ((yyvsp[-7]), (yyvsp[-5]), (yyvsp[-3]), (yyvsp[-1]));
  }
#line 20918 "parser.c" /* yacc.c:1646  */
    break;

  case 1757:
#line 11781 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DIVIDE);
  }
#line 20926 "parser.c" /* yacc.c:1646  */
    break;

  case 1758:
#line 11785 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DIVIDE);
  }
#line 20934 "parser.c" /* yacc.c:1646  */
    break;

  case 1759:
#line 11795 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ENABLE", 0);
  }
#line 20942 "parser.c" /* yacc.c:1646  */
    break;

  case 1761:
#line 11806 "parser.y" /* yacc.c:1646  */
    {
	check_unreached = 0;
	begin_statement ("ENTRY", 0);
	backup_current_pos ();
  }
#line 20952 "parser.c" /* yacc.c:1646  */
    break;

  case 1763:
#line 11816 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "ENTRY");
	} else if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "ENTRY");
	} else if (cb_verify (cb_entry_statement, "ENTRY")) {
		if (!cobc_check_valid_name ((char *)(CB_LITERAL ((yyvsp[-1]))->data), ENTRY_NAME)) {
			emit_entry ((char *)(CB_LITERAL ((yyvsp[-1]))->data), 1, (yyvsp[0]), (yyvsp[-2]));
		}
	}
  }
#line 20968 "parser.c" /* yacc.c:1646  */
    break;

  case 1764:
#line 11834 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("EVALUATE", TERM_EVALUATE);
	eval_level++;
	if (eval_level >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_level = 0;
		eval_inc = 0;
		eval_inc2 = 0;
		YYERROR;
	} else {
		for (eval_inc = 0; eval_inc < EVAL_DEPTH; ++eval_inc) {
			eval_check[eval_level][eval_inc] = NULL;
		}
		eval_inc = 0;
		eval_inc2 = 0;
	}
	cb_end_cond (cb_any);
	cb_save_cond ();
	cb_true_side ();
  }
#line 20994 "parser.c" /* yacc.c:1646  */
    break;

  case 1766:
#line 11861 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_evaluate ((yyvsp[-1]), (yyvsp[0]));
	eval_level--;
  }
#line 21003 "parser.c" /* yacc.c:1646  */
    break;

  case 1767:
#line 11868 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 21009 "parser.c" /* yacc.c:1646  */
    break;

  case 1768:
#line 11870 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 21015 "parser.c" /* yacc.c:1646  */
    break;

  case 1769:
#line 11875 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	eval_check[eval_level][eval_inc++] = (yyvsp[0]);
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 21030 "parser.c" /* yacc.c:1646  */
    break;

  case 1770:
#line 11886 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_true;
	eval_check[eval_level][eval_inc++] = NULL;
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 21045 "parser.c" /* yacc.c:1646  */
    break;

  case 1771:
#line 11897 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_false;
	eval_check[eval_level][eval_inc++] = cb_false;
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 21060 "parser.c" /* yacc.c:1646  */
    break;

  case 1772:
#line 11911 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
	} else {
		(yyval) = (yyvsp[-1]);
	}
  }
#line 21072 "parser.c" /* yacc.c:1646  */
    break;

  case 1773:
#line 11920 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21080 "parser.c" /* yacc.c:1646  */
    break;

  case 1774:
#line 11926 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 21086 "parser.c" /* yacc.c:1646  */
    break;

  case 1775:
#line 11928 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 21092 "parser.c" /* yacc.c:1646  */
    break;

  case 1776:
#line 11934 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_CHAIN ((yyvsp[0]), (yyvsp[-1]));
	eval_inc2 = 0;
  }
#line 21101 "parser.c" /* yacc.c:1646  */
    break;

  case 1777:
#line 11939 "parser.y" /* yacc.c:1646  */
    {
	eval_inc2 = 0;
	cb_verify (cb_missing_statement,
		_("WHEN without imperative statement"));
	/* Note: we don't clear the EVALUATE terminator here
	         as we'd have to skip this later
	         [side effect: possible warning about missing terminator] */
	(yyval) = CB_BUILD_CHAIN (CB_LIST_INIT (cb_build_continue ()), (yyvsp[-1]));
  }
#line 21115 "parser.c" /* yacc.c:1646  */
    break;

  case 1778:
#line 11949 "parser.y" /* yacc.c:1646  */
    {
	eval_inc2 = 0;
	cb_verify (cb_missing_statement,
		_("WHEN without imperative statement"));
	/* Put the dot token back into the stack for reparse */
	cb_unput_dot ();
	(yyval) = CB_BUILD_CHAIN (CB_LIST_INIT (cb_build_continue ()), (yyvsp[-1]));
  }
#line 21128 "parser.c" /* yacc.c:1646  */
    break;

  case 1779:
#line 11962 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_CHAIN ((yyvsp[0]), NULL);
	eval_inc2 = 0;
  }
#line 21137 "parser.c" /* yacc.c:1646  */
    break;

  case 1780:
#line 11967 "parser.y" /* yacc.c:1646  */
    {
	eval_inc2 = 0;
	cb_verify (cb_missing_statement,
		_("WHEN OTHER without imperative statement"));
	/* Note: we don't clear the EVALUATE terminator here
	         as we'd have to skip this later
	         [side effect: possible warning about missing terminator] */
	(yyval) = NULL;
  }
#line 21151 "parser.c" /* yacc.c:1646  */
    break;

  case 1781:
#line 11977 "parser.y" /* yacc.c:1646  */
    {
	eval_inc2 = 0;
	cb_verify (cb_missing_statement,
		_("WHEN OTHER without imperative statement"));
	/* Put the dot token back into the stack for reparse */
	cb_unput_dot ();
	(yyval) = NULL;
  }
#line 21164 "parser.c" /* yacc.c:1646  */
    break;

  case 1782:
#line 11989 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
	eval_inc2 = 0;
  }
#line 21173 "parser.c" /* yacc.c:1646  */
    break;

  case 1783:
#line 11995 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0]));
	eval_inc2 = 0;
  }
#line 21182 "parser.c" /* yacc.c:1646  */
    break;

  case 1784:
#line 12002 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 21188 "parser.c" /* yacc.c:1646  */
    break;

  case 1785:
#line 12004 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 21194 "parser.c" /* yacc.c:1646  */
    break;

  case 1786:
#line 12009 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	not0;
	cb_tree	e1;
	cb_tree	e2;
	cb_tree	x;
	cb_tree	parm1;

	not0 = cb_int0;
	e2 = (yyvsp[0]);
	x = NULL;
	parm1 = (yyvsp[-1]);
	if (eval_check[eval_level][eval_inc2]
	 && eval_check[eval_level][eval_inc2] != cb_false) {
		/* Check if the first token is NOT */
		/* It may belong to the EVALUATE, however see */
		/* below when it may be part of a partial expression */
		if (CB_PURPOSE_INT (parm1) == '!') {
			/* Pop stack if subject not TRUE / FALSE */
			not0 = cb_int1;
			x = parm1;
			parm1 = CB_CHAIN (parm1);
		}
		/* Partial expression handling */
		switch (CB_PURPOSE_INT (parm1)) {
		/* Relational conditions */
		case '<':
		case '>':
		case '[':
		case ']':
		case '~':
		case '=':
		/* Class conditions */
		case '9':
		case 'A':
		case 'L':
		case 'U':
		case 'P':
		case 'N':
		case 'O':
		case 'C':
			if (e2) {
				cb_error_x (e2, _("invalid THROUGH usage"));
				e2 = NULL;
			}
			not0 = CB_PURPOSE (parm1);
			if (x) {
				/* Rebind the NOT to the partial expression */
				parm1 = cb_build_list (cb_int ('!'), NULL, parm1);
			}
			/* Insert subject at head of list */
			parm1 = cb_build_list (cb_int ('x'),
					    eval_check[eval_level][eval_inc2], parm1);
			break;
		}
	}

	/* Build expr now */
	e1 = cb_build_expr (parm1);

	eval_inc2++;
	(yyval) = CB_BUILD_PAIR (not0, CB_BUILD_PAIR (e1, e2));

	if (eval_check[eval_level][eval_inc2-1] == cb_false) {
		/* It was  EVALUATE FALSE; So flip condition */
		if (e1 == cb_true)
			e1 = cb_false;
		else if (e1 == cb_false)
			e1 = cb_true;
	}
	cb_terminate_cond ();
	cb_end_cond (e1);
	cb_save_cond ();
	cb_true_side ();
  }
#line 21273 "parser.c" /* yacc.c:1646  */
    break;

  case 1787:
#line 12083 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_any; eval_inc2++; }
#line 21279 "parser.c" /* yacc.c:1646  */
    break;

  case 1788:
#line 12084 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; eval_inc2++; }
#line 21285 "parser.c" /* yacc.c:1646  */
    break;

  case 1789:
#line 12085 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_false; eval_inc2++; }
#line 21291 "parser.c" /* yacc.c:1646  */
    break;

  case 1790:
#line 12086 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_error_node; eval_inc2++; }
#line 21297 "parser.c" /* yacc.c:1646  */
    break;

  case 1791:
#line 12090 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21303 "parser.c" /* yacc.c:1646  */
    break;

  case 1792:
#line 12091 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 21309 "parser.c" /* yacc.c:1646  */
    break;

  case 1793:
#line 12096 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), EVALUATE);
  }
#line 21317 "parser.c" /* yacc.c:1646  */
    break;

  case 1794:
#line 12100 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), EVALUATE);
  }
#line 21325 "parser.c" /* yacc.c:1646  */
    break;

  case 1795:
#line 12110 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("EXIT", 0);
	cobc_cs_check = CB_CS_EXIT;
  }
#line 21334 "parser.c" /* yacc.c:1646  */
    break;

  case 1796:
#line 12115 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 21342 "parser.c" /* yacc.c:1646  */
    break;

  case 1797:
#line 12122 "parser.y" /* yacc.c:1646  */
    {
  /* TODO: add warning/error if there's another statement in the paragraph */
  }
#line 21350 "parser.c" /* yacc.c:1646  */
    break;

  case 1798:
#line 12126 "parser.y" /* yacc.c:1646  */
    {
	if (in_declaratives && use_global_ind) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PROGRAM is not allowed within a USE GLOBAL procedure"));
	}
	if (current_program->prog_type != COB_MODULE_TYPE_PROGRAM) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PROGRAM not allowed within a FUNCTION"));
	}
	if (current_program->flag_main) {
		check_unreached = 0;
	} else {
		check_unreached = 1;
	}
	if ((yyvsp[0])) {
		if (!current_program->cb_return_code) {
			cb_error_x ((yyvsp[0]), _("RETURNING/GIVING not allowed for non-returning runtime elements"));
		} else {
			cb_emit_move ((yyvsp[0]), CB_LIST_INIT (current_program->cb_return_code));
		}
	}
	current_statement->name = (const char *)"EXIT PROGRAM";
	cb_emit_exit (0);
  }
#line 21379 "parser.c" /* yacc.c:1646  */
    break;

  case 1799:
#line 12151 "parser.y" /* yacc.c:1646  */
    {
	if (in_declaratives && use_global_ind) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT FUNCTION is not allowed within a USE GLOBAL procedure"));
	}
	if (current_program->prog_type != COB_MODULE_TYPE_FUNCTION) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT FUNCTION only allowed within a FUNCTION"));
	}
	check_unreached = 1;
	current_statement->name = (const char *)"EXIT FUNCTION";
	cb_emit_exit (0);
  }
#line 21397 "parser.c" /* yacc.c:1646  */
    break;

  case 1800:
#line 12165 "parser.y" /* yacc.c:1646  */
    {
	struct cb_perform	*p;
	cb_tree			plabel;
	char			name[64];

	if (!perform_stack) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PERFORM is only valid with inline PERFORM"));
	} else if (CB_VALUE (perform_stack) != cb_error_node) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (!p->cycle_label) {
			sprintf (name, "EXIT PERFORM CYCLE %d", cb_id);
			p->cycle_label = cb_build_reference (name);
			plabel = cb_build_label (p->cycle_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PERFORM CYCLE";
		cb_emit_goto (CB_LIST_INIT (p->cycle_label), NULL);
		check_unreached = 1;
	}
  }
#line 21424 "parser.c" /* yacc.c:1646  */
    break;

  case 1801:
#line 12188 "parser.y" /* yacc.c:1646  */
    {
	struct cb_perform	*p;
	cb_tree			plabel;
	char			name[64];

	if (!perform_stack) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PERFORM is only valid with inline PERFORM"));
	} else if (CB_VALUE (perform_stack) != cb_error_node) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (!p->exit_label) {
			sprintf (name, "EXIT PERFORM %d", cb_id);
			p->exit_label = cb_build_reference (name);
			plabel = cb_build_label (p->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PERFORM";
		cb_emit_goto (CB_LIST_INIT (p->exit_label), NULL);
		check_unreached = 1;
	}
  }
#line 21451 "parser.c" /* yacc.c:1646  */
    break;

  case 1802:
#line 12211 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	plabel;
	char	name[64];

	if (!current_section) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT SECTION is only valid with an active SECTION"));
	} else {
		if (!current_section->exit_label) {
			sprintf (name, "EXIT SECTION %d", cb_id);
			current_section->exit_label = cb_build_reference (name);
			plabel = cb_build_label (current_section->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT SECTION";
		cb_emit_goto (CB_LIST_INIT (current_section->exit_label), NULL);
		check_unreached = 1;
	}
  }
#line 21476 "parser.c" /* yacc.c:1646  */
    break;

  case 1803:
#line 12232 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	plabel;
	char	name[64];

	if (!current_paragraph) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PARAGRAPH is only valid with an active PARAGRAPH"));
	} else {
		if (!current_paragraph->exit_label) {
			sprintf (name, "EXIT PARAGRAPH %d", cb_id);
			current_paragraph->exit_label = cb_build_reference (name);
			plabel = cb_build_label (current_paragraph->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PARAGRAPH";
		cb_emit_goto (CB_LIST_INIT (current_paragraph->exit_label), NULL);
		check_unreached = 1;
	}
  }
#line 21501 "parser.c" /* yacc.c:1646  */
    break;

  case 1804:
#line 12255 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21507 "parser.c" /* yacc.c:1646  */
    break;

  case 1805:
#line 12258 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 21513 "parser.c" /* yacc.c:1646  */
    break;

  case 1806:
#line 12266 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("FREE", 0);
	current_statement->flag_no_based = 1;
  }
#line 21522 "parser.c" /* yacc.c:1646  */
    break;

  case 1808:
#line 12275 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_free ((yyvsp[0]));
  }
#line 21530 "parser.c" /* yacc.c:1646  */
    break;

  case 1809:
#line 12285 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("GENERATE", 0);
  }
#line 21538 "parser.c" /* yacc.c:1646  */
    break;

  case 1811:
#line 12294 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
		cb_emit_generate ((yyvsp[0]));
	}
  }
#line 21549 "parser.c" /* yacc.c:1646  */
    break;

  case 1812:
#line 12306 "parser.y" /* yacc.c:1646  */
    {
	if (!current_paragraph->flag_statement) {
		current_paragraph->flag_first_is_goto = 1;
	}
	begin_statement ("GO TO", 0);
	save_debug = start_debug;
	start_debug = 0;
  }
#line 21562 "parser.c" /* yacc.c:1646  */
    break;

  case 1814:
#line 12319 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_goto ((yyvsp[-1]), (yyvsp[0]));
	start_debug = save_debug;
  }
#line 21571 "parser.c" /* yacc.c:1646  */
    break;

  case 1815:
#line 12327 "parser.y" /* yacc.c:1646  */
    {
	check_unreached = 1;
	(yyval) = NULL;
  }
#line 21580 "parser.c" /* yacc.c:1646  */
    break;

  case 1816:
#line 12332 "parser.y" /* yacc.c:1646  */
    {
	check_unreached = 0;
	(yyval) = (yyvsp[0]);
  }
#line 21589 "parser.c" /* yacc.c:1646  */
    break;

  case 1817:
#line 12343 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("GOBACK", 0);
	check_unreached = 1;
	if ((yyvsp[0])) {
		if (!current_program->cb_return_code) {
			cb_error_x ((yyvsp[0]), _("RETURNING/GIVING not allowed for non-returning runtime elements"));
		} else {
			cb_emit_move ((yyvsp[0]), CB_LIST_INIT (current_program->cb_return_code));
		}
	}
	cb_emit_exit (1U);
  }
#line 21606 "parser.c" /* yacc.c:1646  */
    break;

  case 1818:
#line 12362 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("IF", TERM_IF);
  }
#line 21614 "parser.c" /* yacc.c:1646  */
    break;

  case 1820:
#line 12371 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (5)]), (yyvsp[-3]), (yyvsp[0]));
  }
#line 21622 "parser.c" /* yacc.c:1646  */
    break;

  case 1821:
#line 12375 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (3)]), NULL, (yyvsp[0]));
	cb_verify (cb_missing_statement,
		_("IF without imperative statement"));
  }
#line 21632 "parser.c" /* yacc.c:1646  */
    break;

  case 1822:
#line 12381 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (2)]), (yyvsp[0]), NULL);
  }
#line 21640 "parser.c" /* yacc.c:1646  */
    break;

  case 1823:
#line 12387 "parser.y" /* yacc.c:1646  */
    {
	cb_save_cond ();
  }
#line 21648 "parser.c" /* yacc.c:1646  */
    break;

  case 1824:
#line 12391 "parser.y" /* yacc.c:1646  */
    {
	cb_save_cond ();
  }
#line 21656 "parser.c" /* yacc.c:1646  */
    break;

  case 1825:
#line 12397 "parser.y" /* yacc.c:1646  */
    {
	cb_true_side ();
  }
#line 21664 "parser.c" /* yacc.c:1646  */
    break;

  case 1826:
#line 12403 "parser.y" /* yacc.c:1646  */
    {
	cb_false_side ();
  }
#line 21672 "parser.c" /* yacc.c:1646  */
    break;

  case 1827:
#line 12410 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-4) - (0)]), IF);
	cb_terminate_cond ();
  }
#line 21681 "parser.c" /* yacc.c:1646  */
    break;

  case 1828:
#line 12415 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-4) - (1)]), IF);
	cb_terminate_cond ();
  }
#line 21690 "parser.c" /* yacc.c:1646  */
    break;

  case 1829:
#line 12426 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INITIALIZE", 0);
  }
#line 21698 "parser.c" /* yacc.c:1646  */
    break;

  case 1831:
#line 12435 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_initialize ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 21706 "parser.c" /* yacc.c:1646  */
    break;

  case 1832:
#line 12441 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21712 "parser.c" /* yacc.c:1646  */
    break;

  case 1833:
#line 12442 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 21718 "parser.c" /* yacc.c:1646  */
    break;

  case 1834:
#line 12446 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21724 "parser.c" /* yacc.c:1646  */
    break;

  case 1835:
#line 12447 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 21730 "parser.c" /* yacc.c:1646  */
    break;

  case 1836:
#line 12448 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-2]); }
#line 21736 "parser.c" /* yacc.c:1646  */
    break;

  case 1837:
#line 12453 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21744 "parser.c" /* yacc.c:1646  */
    break;

  case 1838:
#line 12457 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21752 "parser.c" /* yacc.c:1646  */
    break;

  case 1839:
#line 12464 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21760 "parser.c" /* yacc.c:1646  */
    break;

  case 1840:
#line 12469 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
  }
#line 21768 "parser.c" /* yacc.c:1646  */
    break;

  case 1841:
#line 12476 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-3]), (yyvsp[0]));
  }
#line 21776 "parser.c" /* yacc.c:1646  */
    break;

  case 1842:
#line 12482 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHABETIC); }
#line 21782 "parser.c" /* yacc.c:1646  */
    break;

  case 1843:
#line 12483 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC); }
#line 21788 "parser.c" /* yacc.c:1646  */
    break;

  case 1844:
#line 12484 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC); }
#line 21794 "parser.c" /* yacc.c:1646  */
    break;

  case 1845:
#line 12485 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC_EDITED); }
#line 21800 "parser.c" /* yacc.c:1646  */
    break;

  case 1846:
#line 12486 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC_EDITED); }
#line 21806 "parser.c" /* yacc.c:1646  */
    break;

  case 1847:
#line 12487 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL); }
#line 21812 "parser.c" /* yacc.c:1646  */
    break;

  case 1848:
#line 12488 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL_EDITED); }
#line 21818 "parser.c" /* yacc.c:1646  */
    break;

  case 1849:
#line 12500 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21826 "parser.c" /* yacc.c:1646  */
    break;

  case 1850:
#line 12504 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_true;
  }
#line 21834 "parser.c" /* yacc.c:1646  */
    break;

  case 1851:
#line 12513 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INITIATE", 0);
  }
#line 21842 "parser.c" /* yacc.c:1646  */
    break;

  case 1853:
#line 12521 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
		cb_emit_initiate ((yyvsp[0]));
	}
  }
#line 21853 "parser.c" /* yacc.c:1646  */
    break;

  case 1854:
#line 12528 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
		cb_emit_initiate ((yyvsp[0]));
	}
  }
#line 21864 "parser.c" /* yacc.c:1646  */
    break;

  case 1855:
#line 12540 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INQUIRE", 0);
	cobc_cs_check = CB_CS_INQUIRE_MODIFY;
  }
#line 21873 "parser.c" /* yacc.c:1646  */
    break;

  case 1856:
#line 12545 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 21881 "parser.c" /* yacc.c:1646  */
    break;

  case 1859:
#line 12559 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INSPECT", 0);
	inspect_keyword = 0;
  }
#line 21890 "parser.c" /* yacc.c:1646  */
    break;

  case 1869:
#line 12587 "parser.y" /* yacc.c:1646  */
    {
	previous_tallying_phrase = NO_PHRASE;
	cb_init_tallying ();
  }
#line 21899 "parser.c" /* yacc.c:1646  */
    break;

  case 1870:
#line 12592 "parser.y" /* yacc.c:1646  */
    {
	if (!(previous_tallying_phrase == CHARACTERS_PHRASE
	      || previous_tallying_phrase == VALUE_REGION_PHRASE)) {
		cb_error (_("TALLYING clause is incomplete"));
	} else {
		cb_emit_inspect ((yyvsp[-3]), (yyvsp[0]), TALLYING_CLAUSE);
	}

	(yyval) = (yyvsp[-3]);
  }
#line 21914 "parser.c" /* yacc.c:1646  */
    break;

  case 1871:
#line 12608 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_inspect ((yyvsp[-2]), (yyvsp[0]), REPLACING_CLAUSE);
	inspect_keyword = 0;
  }
#line 21923 "parser.c" /* yacc.c:1646  */
    break;

  case 1872:
#line 12618 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x = cb_build_converting ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
	cb_emit_inspect ((yyvsp[-5]), x, CONVERTING_CLAUSE);
  }
#line 21932 "parser.c" /* yacc.c:1646  */
    break;

  case 1873:
#line 12626 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21940 "parser.c" /* yacc.c:1646  */
    break;

  case 1874:
#line 12630 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
  }
#line 21948 "parser.c" /* yacc.c:1646  */
    break;

  case 1875:
#line 12637 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (FOR_PHRASE);
	(yyval) = cb_build_tallying_data ((yyvsp[-1]));
  }
#line 21957 "parser.c" /* yacc.c:1646  */
    break;

  case 1876:
#line 12642 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (CHARACTERS_PHRASE);
	(yyval) = cb_build_tallying_characters ((yyvsp[0]));
  }
#line 21966 "parser.c" /* yacc.c:1646  */
    break;

  case 1877:
#line 12647 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_all ();
  }
#line 21975 "parser.c" /* yacc.c:1646  */
    break;

  case 1878:
#line 12652 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_leading ();
  }
#line 21984 "parser.c" /* yacc.c:1646  */
    break;

  case 1879:
#line 12657 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_trailing ();
  }
#line 21993 "parser.c" /* yacc.c:1646  */
    break;

  case 1880:
#line 12662 "parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (VALUE_REGION_PHRASE);
	(yyval) = cb_build_tallying_value ((yyvsp[-1]), (yyvsp[0]));
  }
#line 22002 "parser.c" /* yacc.c:1646  */
    break;

  case 1881:
#line 12669 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22008 "parser.c" /* yacc.c:1646  */
    break;

  case 1882:
#line 12670 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 22014 "parser.c" /* yacc.c:1646  */
    break;

  case 1883:
#line 12675 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_replacing_characters ((yyvsp[-1]), (yyvsp[0]));
	inspect_keyword = 0;
  }
#line 22023 "parser.c" /* yacc.c:1646  */
    break;

  case 1884:
#line 12680 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 22031 "parser.c" /* yacc.c:1646  */
    break;

  case 1886:
#line 12687 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 1; }
#line 22037 "parser.c" /* yacc.c:1646  */
    break;

  case 1887:
#line 12688 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 2; }
#line 22043 "parser.c" /* yacc.c:1646  */
    break;

  case 1888:
#line 12689 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 3; }
#line 22049 "parser.c" /* yacc.c:1646  */
    break;

  case 1889:
#line 12690 "parser.y" /* yacc.c:1646  */
    { inspect_keyword = 4; }
#line 22055 "parser.c" /* yacc.c:1646  */
    break;

  case 1890:
#line 12695 "parser.y" /* yacc.c:1646  */
    {
	switch (inspect_keyword) {
		case 1:
			(yyval) = cb_build_replacing_all ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 2:
			(yyval) = cb_build_replacing_leading ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 3:
			(yyval) = cb_build_replacing_first ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 4:
			(yyval) = cb_build_replacing_trailing ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		default:
			cb_error_x (CB_TREE (current_statement),
				    _("INSPECT missing ALL/FIRST/LEADING/TRAILING"));
			(yyval) = cb_build_replacing_all ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
	}
  }
#line 22081 "parser.c" /* yacc.c:1646  */
    break;

  case 1891:
#line 12722 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_inspect_region_start ();
  }
#line 22089 "parser.c" /* yacc.c:1646  */
    break;

  case 1892:
#line 12726 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_build_inspect_region_start (), (yyvsp[0]));
  }
#line 22097 "parser.c" /* yacc.c:1646  */
    break;

  case 1893:
#line 12730 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_build_inspect_region_start (), (yyvsp[0]));
  }
#line 22105 "parser.c" /* yacc.c:1646  */
    break;

  case 1894:
#line 12734 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_list_add (cb_build_inspect_region_start (), (yyvsp[-1])), (yyvsp[0]));
  }
#line 22113 "parser.c" /* yacc.c:1646  */
    break;

  case 1895:
#line 12738 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_list_add (cb_build_inspect_region_start (), (yyvsp[-1])), (yyvsp[0]));
  }
#line 22121 "parser.c" /* yacc.c:1646  */
    break;

  case 1896:
#line 12745 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_FUNCALL_1 ("cob_inspect_before", (yyvsp[0]));
  }
#line 22129 "parser.c" /* yacc.c:1646  */
    break;

  case 1897:
#line 12752 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_FUNCALL_1 ("cob_inspect_after", (yyvsp[0]));
  }
#line 22137 "parser.c" /* yacc.c:1646  */
    break;

  case 1898:
#line 12761 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("JSON GENERATE", TERM_JSON);
	cb_verify (cb_json_generate, _("JSON GENERATE"));
	cobc_in_json_generate_body = 1;
	cobc_cs_check = CB_CS_JSON_GENERATE;
  }
#line 22148 "parser.c" /* yacc.c:1646  */
    break;

  case 1900:
#line 12774 "parser.y" /* yacc.c:1646  */
    {
	ml_suppress_list = NULL;
  }
#line 22156 "parser.c" /* yacc.c:1646  */
    break;

  case 1901:
#line 12779 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_json_generate_body = 0;
	cobc_cs_check = 0;
  }
#line 22165 "parser.c" /* yacc.c:1646  */
    break;

  case 1902:
#line 12784 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_json_generate ((yyvsp[-8]), (yyvsp[-6]), (yyvsp[-5]), (yyvsp[-3]), ml_suppress_list);
  }
#line 22173 "parser.c" /* yacc.c:1646  */
    break;

  case 1903:
#line 12791 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 22181 "parser.c" /* yacc.c:1646  */
    break;

  case 1904:
#line 12795 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 22189 "parser.c" /* yacc.c:1646  */
    break;

  case 1907:
#line 12807 "parser.y" /* yacc.c:1646  */
    {
	error_if_following_every_clause ();
	add_identifier_to_ml_suppress_conds ((yyvsp[0]));
  }
#line 22198 "parser.c" /* yacc.c:1646  */
    break;

  case 1908:
#line 12815 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), JSON);
  }
#line 22206 "parser.c" /* yacc.c:1646  */
    break;

  case 1909:
#line 12819 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), JSON);
  }
#line 22214 "parser.c" /* yacc.c:1646  */
    break;

  case 1910:
#line 12828 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("JSON PARSE", TERM_JSON);
	CB_PENDING (_("JSON PARSE"));
  }
#line 22223 "parser.c" /* yacc.c:1646  */
    break;

  case 1915:
#line 12853 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MERGE", 0);
	current_statement->flag_merge = 1;
  }
#line 22232 "parser.c" /* yacc.c:1646  */
    break;

  case 1917:
#line 12865 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MODIFY", TERM_MODIFY);
	cobc_cs_check = CB_CS_INQUIRE_MODIFY;
  }
#line 22241 "parser.c" /* yacc.c:1646  */
    break;

  case 1918:
#line 12871 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 22249 "parser.c" /* yacc.c:1646  */
    break;

  case 1921:
#line 12883 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), MODIFY);
  }
#line 22257 "parser.c" /* yacc.c:1646  */
    break;

  case 1922:
#line 12887 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), MODIFY);
  }
#line 22265 "parser.c" /* yacc.c:1646  */
    break;

  case 1923:
#line 12897 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MOVE", 0);
  }
#line 22273 "parser.c" /* yacc.c:1646  */
    break;

  case 1925:
#line 12905 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_move ((yyvsp[-2]), (yyvsp[0]));
  }
#line 22281 "parser.c" /* yacc.c:1646  */
    break;

  case 1926:
#line 12909 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_move_corresponding ((yyvsp[-2]), (yyvsp[0]));
  }
#line 22289 "parser.c" /* yacc.c:1646  */
    break;

  case 1927:
#line 12919 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MULTIPLY", TERM_MULTIPLY);
  }
#line 22297 "parser.c" /* yacc.c:1646  */
    break;

  case 1929:
#line 12928 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '*', (yyvsp[-3]));
  }
#line 22305 "parser.c" /* yacc.c:1646  */
    break;

  case 1930:
#line 12932 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-5]), '*', (yyvsp[-3])));
  }
#line 22313 "parser.c" /* yacc.c:1646  */
    break;

  case 1931:
#line 12939 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), MULTIPLY);
  }
#line 22321 "parser.c" /* yacc.c:1646  */
    break;

  case 1932:
#line 12943 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), MULTIPLY);
  }
#line 22329 "parser.c" /* yacc.c:1646  */
    break;

  case 1933:
#line 12953 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("OPEN", 0);
	cobc_cs_check = CB_CS_OPEN;
  }
#line 22338 "parser.c" /* yacc.c:1646  */
    break;

  case 1937:
#line 12967 "parser.y" /* yacc.c:1646  */
    {
	cb_tree l;
	cb_tree x;

	if (((yyvsp[-5]) && (yyvsp[-3])) || ((yyvsp[-5]) && (yyvsp[0])) || ((yyvsp[-3]) && (yyvsp[0]))) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s and %s are mutually exclusive"), "SHARING", _("LOCK clauses"));
	}
	if ((yyvsp[0])) {
		x = (yyvsp[0]);
	} else if ((yyvsp[-3])) {
		x = (yyvsp[-3]);
	} else {
		x = (yyvsp[-5]);
	}

	for (l = (yyvsp[-1]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			begin_implicit_statement ();
			cb_emit_open (CB_VALUE (l), (yyvsp[-4]), x);
		}
	}
  }
#line 22366 "parser.c" /* yacc.c:1646  */
    break;

  case 1938:
#line 12994 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22372 "parser.c" /* yacc.c:1646  */
    break;

  case 1939:
#line 12995 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 22378 "parser.c" /* yacc.c:1646  */
    break;

  case 1940:
#line 12999 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_INPUT); }
#line 22384 "parser.c" /* yacc.c:1646  */
    break;

  case 1941:
#line 13000 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_OUTPUT); }
#line 22390 "parser.c" /* yacc.c:1646  */
    break;

  case 1942:
#line 13001 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_I_O); }
#line 22396 "parser.c" /* yacc.c:1646  */
    break;

  case 1943:
#line 13002 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_EXTEND); }
#line 22402 "parser.c" /* yacc.c:1646  */
    break;

  case 1944:
#line 13006 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22408 "parser.c" /* yacc.c:1646  */
    break;

  case 1945:
#line 13007 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22414 "parser.c" /* yacc.c:1646  */
    break;

  case 1946:
#line 13011 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22420 "parser.c" /* yacc.c:1646  */
    break;

  case 1947:
#line 13012 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22426 "parser.c" /* yacc.c:1646  */
    break;

  case 1948:
#line 13013 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22432 "parser.c" /* yacc.c:1646  */
    break;

  case 1949:
#line 13017 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 22438 "parser.c" /* yacc.c:1646  */
    break;

  case 1950:
#line 13021 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22444 "parser.c" /* yacc.c:1646  */
    break;

  case 1951:
#line 13022 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22450 "parser.c" /* yacc.c:1646  */
    break;

  case 1952:
#line 13026 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 22456 "parser.c" /* yacc.c:1646  */
    break;

  case 1953:
#line 13028 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE);
	/* TODO: check for indexed; pass extra flag to fileio */
	CB_PENDING ("WITH MASS-UPDATE");
  }
#line 22466 "parser.c" /* yacc.c:1646  */
    break;

  case 1954:
#line 13034 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE);
	/* TODO: check for indexed; pass extra flag to fileio */
	CB_PENDING ("WITH BULK-ADDITION");
  }
#line 22476 "parser.c" /* yacc.c:1646  */
    break;

  case 1955:
#line 13042 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 22482 "parser.c" /* yacc.c:1646  */
    break;

  case 1956:
#line 13043 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22488 "parser.c" /* yacc.c:1646  */
    break;

  case 1957:
#line 13044 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22494 "parser.c" /* yacc.c:1646  */
    break;

  case 1961:
#line 13056 "parser.y" /* yacc.c:1646  */
    {
	/* FIXME: only allow for sequential files */
	/* FIXME: only allow with INPUT or OUTPUT */
	CB_PENDING ("OPEN WITH NO REWIND");
	(yyval) = NULL;
  }
#line 22505 "parser.c" /* yacc.c:1646  */
    break;

  case 1962:
#line 13063 "parser.y" /* yacc.c:1646  */
    {
	/* FIXME: only allow for sequential / line-sequential files */
	/* FIXME: only allow with INPUT */
	/* FIXME: add actual compiler configuration */
	if (cb_warn_obsolete == COBC_WARN_AS_ERROR) {
		(void)cb_verify (CB_OBSOLETE, "OPEN REVERSED");
	} else {
		/* FIXME: set file attribute */
		CB_PENDING ("OPEN REVERSED");
	};
	(yyval) = NULL;
  }
#line 22522 "parser.c" /* yacc.c:1646  */
    break;

  case 1963:
#line 13082 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("PERFORM", TERM_PERFORM);
	/* Turn off field debug - PERFORM is special */
	save_debug = start_debug;
	start_debug = 0;
	cobc_cs_check = CB_CS_PERFORM;
  }
#line 22534 "parser.c" /* yacc.c:1646  */
    break;

  case 1965:
#line 13097 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_perform ((yyvsp[0]), (yyvsp[-2]), (yyvsp[-3]), (yyvsp[-1]));
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 22544 "parser.c" /* yacc.c:1646  */
    break;

  case 1966:
#line 13105 "parser.y" /* yacc.c:1646  */
    {
	CB_ADD_TO_CHAIN ((yyvsp[-1]), perform_stack);
	/* Restore field debug before inline statements */
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 22555 "parser.c" /* yacc.c:1646  */
    break;

  case 1967:
#line 13112 "parser.y" /* yacc.c:1646  */
    {
	perform_stack = CB_CHAIN (perform_stack);
	cb_emit_perform ((yyvsp[-4]), (yyvsp[-1]), (yyvsp[-5]), (yyvsp[-3]));
  }
#line 22564 "parser.c" /* yacc.c:1646  */
    break;

  case 1968:
#line 13119 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_missing_statement,
		_("inline PERFORM without imperative statement"));
  }
#line 22573 "parser.c" /* yacc.c:1646  */
    break;

  case 1969:
#line 13124 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_perform ((yyvsp[-3]), NULL, (yyvsp[-4]), (yyvsp[-2]));
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 22583 "parser.c" /* yacc.c:1646  */
    break;

  case 1970:
#line 13133 "parser.y" /* yacc.c:1646  */
    {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING ((yyvsp[(-6) - (0)]), PERFORM);
	} else {
		TERMINATOR_ERROR ((yyvsp[(-6) - (0)]), PERFORM);
	}
  }
#line 22595 "parser.c" /* yacc.c:1646  */
    break;

  case 1971:
#line 13141 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-6) - (1)]), PERFORM);
  }
#line 22603 "parser.c" /* yacc.c:1646  */
    break;

  case 1972:
#line 13148 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-3) - (1)]), PERFORM);
  }
#line 22611 "parser.c" /* yacc.c:1646  */
    break;

  case 1973:
#line 13152 "parser.y" /* yacc.c:1646  */
    {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING ((yyvsp[(-3) - (1)]), PERFORM);
	} else {
		TERMINATOR_ERROR ((yyvsp[(-3) - (1)]), PERFORM);
	}
	/* Put the dot token back into the stack for reparse */
	cb_unput_dot ();
  }
#line 22625 "parser.c" /* yacc.c:1646  */
    break;

  case 1974:
#line 13165 "parser.y" /* yacc.c:1646  */
    {
	/* Return from $1 */
	CB_REFERENCE ((yyvsp[0]))->length = cb_true;
	CB_REFERENCE ((yyvsp[0]))->flag_decl_ok = 1;
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[0]));
  }
#line 22636 "parser.c" /* yacc.c:1646  */
    break;

  case 1975:
#line 13172 "parser.y" /* yacc.c:1646  */
    {
	/* Return from $3 */
	CB_REFERENCE ((yyvsp[0]))->length = cb_true;
	CB_REFERENCE ((yyvsp[-2]))->flag_decl_ok = 1;
	CB_REFERENCE ((yyvsp[0]))->flag_decl_ok = 1;
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
  }
#line 22648 "parser.c" /* yacc.c:1646  */
    break;

  case 1976:
#line 13183 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_once (NULL);
  }
#line 22656 "parser.c" /* yacc.c:1646  */
    break;

  case 1977:
#line 13187 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_times ((yyvsp[-1]));
	current_program->loop_counter++;
  }
#line 22665 "parser.c" /* yacc.c:1646  */
    break;

  case 1978:
#line 13192 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_forever (NULL);
  }
#line 22673 "parser.c" /* yacc.c:1646  */
    break;

  case 1979:
#line 13196 "parser.y" /* yacc.c:1646  */
    {
	cb_tree varying;

	if (!(yyvsp[0])) {
		(yyval) = cb_build_perform_forever (NULL);
	} else {
		if ((yyvsp[-2]) == CB_AFTER)
			cb_build_perform_after_until();
		varying = CB_LIST_INIT (cb_build_perform_varying (NULL, NULL, NULL, (yyvsp[0])));
		(yyval) = cb_build_perform_until ((yyvsp[-2]), varying);
	}
  }
#line 22690 "parser.c" /* yacc.c:1646  */
    break;

  case 1980:
#line 13209 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_until ((yyvsp[-2]), (yyvsp[0]));
  }
#line 22698 "parser.c" /* yacc.c:1646  */
    break;

  case 1981:
#line 13215 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BEFORE; }
#line 22704 "parser.c" /* yacc.c:1646  */
    break;

  case 1982:
#line 13216 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22710 "parser.c" /* yacc.c:1646  */
    break;

  case 1983:
#line 13220 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22716 "parser.c" /* yacc.c:1646  */
    break;

  case 1984:
#line 13221 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22722 "parser.c" /* yacc.c:1646  */
    break;

  case 1985:
#line 13224 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 22728 "parser.c" /* yacc.c:1646  */
    break;

  case 1986:
#line 13226 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 22734 "parser.c" /* yacc.c:1646  */
    break;

  case 1987:
#line 13231 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	int		data_type_ok = 1;

	if ((yyvsp[-5]) != cb_error_node
	 && (yyvsp[-3]) != cb_error_node
	 && (yyvsp[-2]) != cb_error_node) {

		if (cb_tree_category ((yyvsp[-5])) != CB_CATEGORY_NUMERIC) {
			x = cb_ref ((yyvsp[-5]));
			cb_error_x (CB_TREE (current_statement),
				_("PERFORM VARYING '%s' (line %d of %s) is not a numeric field"),
				cb_name (x),x->source_line, x->source_file);
			(yyval) = cb_int1;
			data_type_ok = 0;
		}
		if (cb_tree_category ((yyvsp[-3])) != CB_CATEGORY_NUMERIC) {
			x = cb_ref ((yyvsp[-3]));
			cb_error_x (CB_TREE (current_statement),
				_("PERFORM VARYING '%s' (line %d of %s) is not a numeric field"),
				cb_name (x),x->source_line, x->source_file);
			(yyval) = cb_int1;
			data_type_ok = 0;
		}
		if (cb_tree_category ((yyvsp[-2])) != CB_CATEGORY_NUMERIC) {
			x = cb_ref ((yyvsp[-2]));
			cb_error_x (CB_TREE (current_statement),
				_("PERFORM VARYING '%s' (line %d of %s) is not a numeric field"),
				cb_name (x),x->source_line, x->source_file);
			(yyval) = cb_int1;
			data_type_ok = 0;
		}

		if (data_type_ok) {
			(yyval) = cb_build_perform_varying ((yyvsp[-5]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[0]));
		}
	}
  }
#line 22777 "parser.c" /* yacc.c:1646  */
    break;

  case 1988:
#line 13273 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_perform_varying_without_by, _ ("PERFORM VARYING without BY phrase"));
	(yyval) = cb_build_numeric_literal (0, "1", 0);
  }
#line 22786 "parser.c" /* yacc.c:1646  */
    break;

  case 1989:
#line 13278 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 22794 "parser.c" /* yacc.c:1646  */
    break;

  case 1990:
#line 13287 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("PURGE", 0);
  }
#line 22802 "parser.c" /* yacc.c:1646  */
    break;

  case 1991:
#line 13291 "parser.y" /* yacc.c:1646  */
    {
  }
#line 22809 "parser.c" /* yacc.c:1646  */
    break;

  case 1992:
#line 13299 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RAISE", 0);
  }
#line 22817 "parser.c" /* yacc.c:1646  */
    break;

  case 1994:
#line 13307 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("RAISE statement");
	/* TODO: check for level 3 error here */
  }
#line 22826 "parser.c" /* yacc.c:1646  */
    break;

  case 1995:
#line 13312 "parser.y" /* yacc.c:1646  */
    {
	/* easy cheating here as we don't have any OO in */
	cb_error(_("'%s' is not an object-reference"), cb_name ((yyvsp[0])));
  }
#line 22835 "parser.c" /* yacc.c:1646  */
    break;

  case 1996:
#line 13322 "parser.y" /* yacc.c:1646  */
    {
	/* TODO:
	cb_tree exception = get_exception (CB_NAME($1));
	if (!exception) {
		cb_error (_("'%s' is not an exception-name"), CB_NAME ($1));
	}
	*/
  }
#line 22848 "parser.c" /* yacc.c:1646  */
    break;

  case 1997:
#line 13336 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("READ", TERM_READ);
	cobc_cs_check = CB_CS_READ;
  }
#line 22857 "parser.c" /* yacc.c:1646  */
    break;

  case 1999:
#line 13346 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;

	if (CB_VALID_TREE ((yyvsp[-6]))) {
		struct cb_file	*cf;

		cf = CB_FILE(cb_ref ((yyvsp[-6])));
		if ((yyvsp[-2]) && (cf->lock_mode & COB_LOCK_AUTOMATIC)) {
			cb_error_x (CB_TREE (current_statement),
				    _("LOCK clause invalid with file LOCK AUTOMATIC"));
		} else if ((yyvsp[-1]) &&
		      (cf->organization != COB_ORG_RELATIVE &&
		       cf->organization != COB_ORG_INDEXED)) {
			cb_error_x (CB_TREE (current_statement),
				    _("KEY clause invalid with this file type"));
		} else if (current_statement->handler_type == INVALID_KEY_HANDLER &&
			   (cf->organization != COB_ORG_RELATIVE &&
			    cf->organization != COB_ORG_INDEXED)) {
			cb_error_x (CB_TREE (current_statement),
				    _("INVALID KEY clause invalid with this file type"));
		} else {
			cb_emit_read ((yyvsp[-6]), (yyvsp[-5]), (yyvsp[-3]), (yyvsp[-1]), (yyvsp[-2]));
		}
	}
  }
#line 22887 "parser.c" /* yacc.c:1646  */
    break;

  case 2000:
#line 13374 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22893 "parser.c" /* yacc.c:1646  */
    break;

  case 2001:
#line 13375 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22899 "parser.c" /* yacc.c:1646  */
    break;

  case 2002:
#line 13380 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 22907 "parser.c" /* yacc.c:1646  */
    break;

  case 2003:
#line 13384 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int3;
  }
#line 22915 "parser.c" /* yacc.c:1646  */
    break;

  case 2004:
#line 13388 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 22923 "parser.c" /* yacc.c:1646  */
    break;

  case 2005:
#line 13392 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 22931 "parser.c" /* yacc.c:1646  */
    break;

  case 2008:
#line 13404 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("ADVANCING ON LOCK");
  }
#line 22939 "parser.c" /* yacc.c:1646  */
    break;

  case 2012:
#line 13417 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("RETRY");
	cobc_cs_check = 0;
  }
#line 22948 "parser.c" /* yacc.c:1646  */
    break;

  case 2018:
#line 13437 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 22956 "parser.c" /* yacc.c:1646  */
    break;

  case 2019:
#line 13441 "parser.y" /* yacc.c:1646  */
    {
   (yyval) = cb_int5;
  }
#line 22964 "parser.c" /* yacc.c:1646  */
    break;

  case 2020:
#line 13445 "parser.y" /* yacc.c:1646  */
    {
	/* TO-DO: Merge with RETRY phrase */
	(yyval) = cb_int4;
  }
#line 22973 "parser.c" /* yacc.c:1646  */
    break;

  case 2021:
#line 13452 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22979 "parser.c" /* yacc.c:1646  */
    break;

  case 2022:
#line 13453 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22985 "parser.c" /* yacc.c:1646  */
    break;

  case 2025:
#line 13463 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), READ);
  }
#line 22993 "parser.c" /* yacc.c:1646  */
    break;

  case 2026:
#line 13467 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), READ);
  }
#line 23001 "parser.c" /* yacc.c:1646  */
    break;

  case 2027:
#line 13477 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("READY TRACE", 0);
	cb_emit_ready_trace ();
  }
#line 23010 "parser.c" /* yacc.c:1646  */
    break;

  case 2028:
#line 13487 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RECEIVE", TERM_RECEIVE);
  }
#line 23018 "parser.c" /* yacc.c:1646  */
    break;

  case 2042:
#line 13530 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), RECEIVE);
  }
#line 23026 "parser.c" /* yacc.c:1646  */
    break;

  case 2043:
#line 13534 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), RECEIVE);
  }
#line 23034 "parser.c" /* yacc.c:1646  */
    break;

  case 2044:
#line 13543 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RELEASE", 0);
  }
#line 23042 "parser.c" /* yacc.c:1646  */
    break;

  case 2046:
#line 13551 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_release ((yyvsp[-1]), (yyvsp[0]));
  }
#line 23050 "parser.c" /* yacc.c:1646  */
    break;

  case 2047:
#line 13561 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RESET TRACE", 0);
	cb_emit_reset_trace ();
  }
#line 23059 "parser.c" /* yacc.c:1646  */
    break;

  case 2048:
#line 13571 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RETURN", TERM_RETURN);
  }
#line 23067 "parser.c" /* yacc.c:1646  */
    break;

  case 2050:
#line 13580 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_return ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 23075 "parser.c" /* yacc.c:1646  */
    break;

  case 2051:
#line 13587 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), RETURN);
  }
#line 23083 "parser.c" /* yacc.c:1646  */
    break;

  case 2052:
#line 13591 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), RETURN);
  }
#line 23091 "parser.c" /* yacc.c:1646  */
    break;

  case 2053:
#line 13601 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("REWRITE", TERM_REWRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 23102 "parser.c" /* yacc.c:1646  */
    break;

  case 2055:
#line 13613 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_rewrite ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]));
	start_debug = save_debug;
  }
#line 23111 "parser.c" /* yacc.c:1646  */
    break;

  case 2056:
#line 13621 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23119 "parser.c" /* yacc.c:1646  */
    break;

  case 2058:
#line 13629 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 23127 "parser.c" /* yacc.c:1646  */
    break;

  case 2059:
#line 13633 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int2;
  }
#line 23135 "parser.c" /* yacc.c:1646  */
    break;

  case 2060:
#line 13640 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), REWRITE);
  }
#line 23143 "parser.c" /* yacc.c:1646  */
    break;

  case 2061:
#line 13644 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), REWRITE);
  }
#line 23151 "parser.c" /* yacc.c:1646  */
    break;

  case 2062:
#line 13654 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ROLLBACK", 0);
	cb_emit_rollback ();
  }
#line 23160 "parser.c" /* yacc.c:1646  */
    break;

  case 2063:
#line 13665 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SEARCH", TERM_SEARCH);
  }
#line 23168 "parser.c" /* yacc.c:1646  */
    break;

  case 2065:
#line 13674 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_search ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 23176 "parser.c" /* yacc.c:1646  */
    break;

  case 2066:
#line 13679 "parser.y" /* yacc.c:1646  */
    {
	current_statement->name = (const char *)"SEARCH ALL";
	cb_emit_search_all ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 23185 "parser.c" /* yacc.c:1646  */
    break;

  case 2067:
#line 13686 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 23191 "parser.c" /* yacc.c:1646  */
    break;

  case 2068:
#line 13687 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 23197 "parser.c" /* yacc.c:1646  */
    break;

  case 2069:
#line 13692 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23205 "parser.c" /* yacc.c:1646  */
    break;

  case 2070:
#line 13697 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 23213 "parser.c" /* yacc.c:1646  */
    break;

  case 2071:
#line 13704 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 23221 "parser.c" /* yacc.c:1646  */
    break;

  case 2072:
#line 13708 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), (yyvsp[-1]));
  }
#line 23229 "parser.c" /* yacc.c:1646  */
    break;

  case 2073:
#line 13716 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_if_check_break ((yyvsp[-1]), (yyvsp[0]));
  }
#line 23237 "parser.c" /* yacc.c:1646  */
    break;

  case 2074:
#line 13723 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), SEARCH);
  }
#line 23245 "parser.c" /* yacc.c:1646  */
    break;

  case 2075:
#line 13727 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), SEARCH);
  }
#line 23253 "parser.c" /* yacc.c:1646  */
    break;

  case 2076:
#line 13737 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SEND", 0);
  }
#line 23261 "parser.c" /* yacc.c:1646  */
    break;

  case 2078:
#line 13745 "parser.y" /* yacc.c:1646  */
    {
  }
#line 23268 "parser.c" /* yacc.c:1646  */
    break;

  case 2079:
#line 13748 "parser.y" /* yacc.c:1646  */
    {
  }
#line 23275 "parser.c" /* yacc.c:1646  */
    break;

  case 2082:
#line 13759 "parser.y" /* yacc.c:1646  */
    {
  }
#line 23282 "parser.c" /* yacc.c:1646  */
    break;

  case 2089:
#line 13779 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SET", 0);
	set_attr_val_on = 0;
	set_attr_val_off = 0;
	cobc_cs_check = CB_CS_SET;
  }
#line 23293 "parser.c" /* yacc.c:1646  */
    break;

  case 2090:
#line 13786 "parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 23301 "parser.c" /* yacc.c:1646  */
    break;

  case 2099:
#line 13803 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 23307 "parser.c" /* yacc.c:1646  */
    break;

  case 2100:
#line 13804 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 23313 "parser.c" /* yacc.c:1646  */
    break;

  case 2101:
#line 13808 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 23319 "parser.c" /* yacc.c:1646  */
    break;

  case 2102:
#line 13809 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 23325 "parser.c" /* yacc.c:1646  */
    break;

  case 2103:
#line 13816 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_setenv ((yyvsp[-2]), (yyvsp[0]));
  }
#line 23333 "parser.c" /* yacc.c:1646  */
    break;

  case 2104:
#line 13825 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_attribute ((yyvsp[-2]), set_attr_val_on, set_attr_val_off);
  }
#line 23341 "parser.c" /* yacc.c:1646  */
    break;

  case 2107:
#line 13837 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_BELL);
  }
#line 23349 "parser.c" /* yacc.c:1646  */
    break;

  case 2108:
#line 13841 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_BLINK);
  }
#line 23357 "parser.c" /* yacc.c:1646  */
    break;

  case 2109:
#line 13845 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_HIGHLIGHT);
	check_not_highlight_and_lowlight (set_attr_val_on | set_attr_val_off,
					  COB_SCREEN_HIGHLIGHT);
  }
#line 23367 "parser.c" /* yacc.c:1646  */
    break;

  case 2110:
#line 13851 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_LOWLIGHT);
	check_not_highlight_and_lowlight (set_attr_val_on | set_attr_val_off,
					  COB_SCREEN_LOWLIGHT);
  }
#line 23377 "parser.c" /* yacc.c:1646  */
    break;

  case 2111:
#line 13857 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_REVERSE);
  }
#line 23385 "parser.c" /* yacc.c:1646  */
    break;

  case 2112:
#line 13861 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_UNDERLINE);
  }
#line 23393 "parser.c" /* yacc.c:1646  */
    break;

  case 2113:
#line 13865 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_LEFTLINE);
  }
#line 23401 "parser.c" /* yacc.c:1646  */
    break;

  case 2114:
#line 13869 "parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_OVERLINE);
  }
#line 23409 "parser.c" /* yacc.c:1646  */
    break;

  case 2115:
#line 13878 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_to ((yyvsp[-3]), cb_build_ppointer ((yyvsp[0])));
  }
#line 23417 "parser.c" /* yacc.c:1646  */
    break;

  case 2116:
#line 13882 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_to ((yyvsp[-2]), (yyvsp[0]));
  }
#line 23425 "parser.c" /* yacc.c:1646  */
    break;

  case 2117:
#line 13886 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_move (cb_build_length ((yyvsp[0])), (yyvsp[-4]));
  }
#line 23433 "parser.c" /* yacc.c:1646  */
    break;

  case 2118:
#line 13895 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_up_down ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[0]));
  }
#line 23441 "parser.c" /* yacc.c:1646  */
    break;

  case 2121:
#line 13909 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_on_off ((yyvsp[-2]), (yyvsp[0]));
  }
#line 23449 "parser.c" /* yacc.c:1646  */
    break;

  case 2124:
#line 13923 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_true ((yyvsp[-2]));
  }
#line 23457 "parser.c" /* yacc.c:1646  */
    break;

  case 2125:
#line 13927 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_false ((yyvsp[-2]));
  }
#line 23465 "parser.c" /* yacc.c:1646  */
    break;

  case 2126:
#line 13936 "parser.y" /* yacc.c:1646  */
    {
	  cb_emit_set_last_exception_to_off ();
  }
#line 23473 "parser.c" /* yacc.c:1646  */
    break;

  case 2127:
#line 13945 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_thread_priority ((yyvsp[-3]), (yyvsp[0]));
	CB_PENDING ("THREAD");
  }
#line 23482 "parser.c" /* yacc.c:1646  */
    break;

  case 2128:
#line 13956 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SORT", 0);
  }
#line 23490 "parser.c" /* yacc.c:1646  */
    break;

  case 2130:
#line 13964 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x = cb_ref ((yyvsp[-3]));

	(yyval) = NULL;
	if (CB_VALID_TREE (x)) {
		if ((yyvsp[-2]) == NULL) {
			if (CB_FILE_P (x)) {
				cb_error (_("file sort requires KEY phrase"));
			} else {
				struct cb_field	*f = CB_FIELD_PTR (x);
/* TODO: add compiler configuration cb_sort_without_keys
				if (f->nkeys
				 && cb_verify (cb_sort_without_keys, _("table SORT without keys"))) {
*/
				if (f->nkeys) {
					cb_tree lparm;
					/* create reference to first key */
					x = cb_ref (f->keys[0].key);
					/* use this as single sort key, with search order derived from definition */
					lparm = cb_list_add (NULL, x);
					CB_PURPOSE (lparm) = cb_int(f->keys[0].dir);
					(yyvsp[-2]) = cb_list_append (NULL, lparm);
					cb_emit_sort_init ((yyvsp[-3]), (yyvsp[-2]), alphanumeric_collation, national_collation);
					(yyval) = (yyvsp[-3]);
				} else {
					cb_error (_("table SORT requires KEY phrase"));
				}
			}
		} else if ((yyvsp[-2]) != cb_error_node) {
			cb_emit_sort_init ((yyvsp[-3]), (yyvsp[-2]), alphanumeric_collation, national_collation);
			(yyval) = (yyvsp[-3]);
		}
	}
  }
#line 23529 "parser.c" /* yacc.c:1646  */
    break;

  case 2131:
#line 13999 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2]) && CB_VALID_TREE ((yyvsp[-6]))) {
		cb_emit_sort_finish ((yyvsp[-6]));
	}
  }
#line 23539 "parser.c" /* yacc.c:1646  */
    break;

  case 2132:
#line 14008 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23547 "parser.c" /* yacc.c:1646  */
    break;

  case 2133:
#line 14013 "parser.y" /* yacc.c:1646  */
    {
	cb_tree lparm = (yyvsp[0]);
	cb_tree l;

	if (lparm == NULL) {
		lparm = CB_LIST_INIT (NULL);
	}
	for (l = lparm; l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = (yyvsp[-2]);
	}
	(yyval) = cb_list_append ((yyvsp[-4]), lparm);
  }
#line 23564 "parser.c" /* yacc.c:1646  */
    break;

  case 2134:
#line 14028 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 23570 "parser.c" /* yacc.c:1646  */
    break;

  case 2135:
#line 14029 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 23576 "parser.c" /* yacc.c:1646  */
    break;

  case 2137:
#line 14034 "parser.y" /* yacc.c:1646  */
    {
	/* The GnuCOBOL sort is a stable sort. ie. dups are per default in order */
	/* Therefore nothing to do here */
  }
#line 23585 "parser.c" /* yacc.c:1646  */
    break;

  case 2138:
#line 14042 "parser.y" /* yacc.c:1646  */
    {
	alphanumeric_collation = national_collation = NULL;
  }
#line 23593 "parser.c" /* yacc.c:1646  */
    break;

  case 2140:
#line 14050 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) && CB_FILE_P (cb_ref ((yyvsp[0])))) {
		cb_error (_("file sort requires USING or INPUT PROCEDURE"));
	}
  }
#line 23603 "parser.c" /* yacc.c:1646  */
    break;

  case 2141:
#line 14056 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[-2])))) {
			cb_error (_("USING invalid with table SORT"));
		} else {
			cb_emit_sort_using ((yyvsp[-2]), (yyvsp[0]));
		}
	}
  }
#line 23617 "parser.c" /* yacc.c:1646  */
    break;

  case 2142:
#line 14066 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-4])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[-4])))) {
			cb_error (_("INPUT PROCEDURE invalid with table SORT"));
		} else if (current_statement->flag_merge) {
			cb_error (_("INPUT PROCEDURE invalid with MERGE"));
		} else {
			cb_emit_sort_input ((yyvsp[0]));
		}
	}
	cobc_cs_check = 0;
  }
#line 23634 "parser.c" /* yacc.c:1646  */
    break;

  case 2143:
#line 14082 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (0)]) && CB_FILE_P (cb_ref ((yyvsp[(-1) - (0)])))) {
		cb_error (_("file sort requires GIVING or OUTPUT PROCEDURE"));
	}
  }
#line 23644 "parser.c" /* yacc.c:1646  */
    break;

  case 2144:
#line 14088 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (2)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(-1) - (2)])))) {
			cb_error (_("GIVING invalid with table SORT"));
		} else {
			cb_emit_sort_giving ((yyvsp[(-1) - (2)]), (yyvsp[0]));
		}
	}
  }
#line 23658 "parser.c" /* yacc.c:1646  */
    break;

  case 2145:
#line 14098 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (4)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(-1) - (4)])))) {
			cb_error (_("OUTPUT PROCEDURE invalid with table SORT"));
		} else {
			cb_emit_sort_output ((yyvsp[0]));
		}
	}
	cobc_cs_check = 0;
  }
#line 23673 "parser.c" /* yacc.c:1646  */
    break;

  case 2146:
#line 14115 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("START", TERM_START);
	start_tree = cb_int (COB_EQ);
  }
#line 23682 "parser.c" /* yacc.c:1646  */
    break;

  case 2148:
#line 14125 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-1]) && !(yyvsp[-2])) {
		cb_error_x (CB_TREE (current_statement),
			    _("SIZE/LENGTH invalid here"));
	} else {
		cb_emit_start ((yyvsp[-3]), start_tree, (yyvsp[-2]), (yyvsp[-1]));
	}
  }
#line 23695 "parser.c" /* yacc.c:1646  */
    break;

  case 2149:
#line 14137 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23703 "parser.c" /* yacc.c:1646  */
    break;

  case 2150:
#line 14141 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 23711 "parser.c" /* yacc.c:1646  */
    break;

  case 2151:
#line 14148 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23719 "parser.c" /* yacc.c:1646  */
    break;

  case 2152:
#line 14152 "parser.y" /* yacc.c:1646  */
    {
	start_tree = (yyvsp[-1]);
	(yyval) = (yyvsp[0]);
  }
#line 23728 "parser.c" /* yacc.c:1646  */
    break;

  case 2153:
#line 14157 "parser.y" /* yacc.c:1646  */
    {
	start_tree = cb_int (COB_FI);
	(yyval) = NULL;
  }
#line 23737 "parser.c" /* yacc.c:1646  */
    break;

  case 2154:
#line 14162 "parser.y" /* yacc.c:1646  */
    {
	start_tree = cb_int (COB_LA);
	(yyval) = NULL;
  }
#line 23746 "parser.c" /* yacc.c:1646  */
    break;

  case 2155:
#line 14169 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_EQ); }
#line 23752 "parser.c" /* yacc.c:1646  */
    break;

  case 2156:
#line 14170 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_LE : COB_GT); }
#line 23758 "parser.c" /* yacc.c:1646  */
    break;

  case 2157:
#line 14171 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_GE : COB_LT); }
#line 23764 "parser.c" /* yacc.c:1646  */
    break;

  case 2158:
#line 14172 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_LT : COB_GE); }
#line 23770 "parser.c" /* yacc.c:1646  */
    break;

  case 2159:
#line 14173 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_GT : COB_LE); }
#line 23776 "parser.c" /* yacc.c:1646  */
    break;

  case 2160:
#line 14174 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_NE); }
#line 23782 "parser.c" /* yacc.c:1646  */
    break;

  case 2161:
#line 14179 "parser.y" /* yacc.c:1646  */
    {
	cb_error_x (CB_TREE (current_statement),
		    _("NOT EQUAL condition not allowed on START statement"));
  }
#line 23791 "parser.c" /* yacc.c:1646  */
    break;

  case 2164:
#line 14192 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), START);
  }
#line 23799 "parser.c" /* yacc.c:1646  */
    break;

  case 2165:
#line 14196 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), START);
  }
#line 23807 "parser.c" /* yacc.c:1646  */
    break;

  case 2166:
#line 14206 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP RUN", 0);
	cobc_cs_check = CB_CS_STOP;
  }
#line 23816 "parser.c" /* yacc.c:1646  */
    break;

  case 2167:
#line 14211 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_stop_run ((yyvsp[0]));
	check_unreached = 1;
	cobc_cs_check = 0;
  }
#line 23826 "parser.c" /* yacc.c:1646  */
    break;

  case 2168:
#line 14217 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP", 0);
	cb_emit_display (CB_LIST_INIT ((yyvsp[0])), cb_int0, cb_int1, NULL,
			 NULL, 1, DEVICE_DISPLAY);
	cb_emit_accept (cb_null, NULL, NULL);
	cobc_cs_check = 0;
  }
#line 23838 "parser.c" /* yacc.c:1646  */
    break;

  case 2169:
#line 14225 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP THREAD", 0);
	cb_emit_stop_thread ((yyvsp[0]));
	cobc_cs_check = 0;
	cb_warning_x (COBC_WARN_FILLER, (yyvsp[0]), _("%s is replaced by %s"), "STOP THREAD", "STOP RUN");
  }
#line 23849 "parser.c" /* yacc.c:1646  */
    break;

  case 2170:
#line 14235 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->cb_return_code) {
		(yyval) = current_program->cb_return_code;
	} else {
		(yyval) = cb_int0;
	}
  }
#line 23861 "parser.c" /* yacc.c:1646  */
    break;

  case 2171:
#line 14243 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 23869 "parser.c" /* yacc.c:1646  */
    break;

  case 2172:
#line 14247 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 23877 "parser.c" /* yacc.c:1646  */
    break;

  case 2173:
#line 14251 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = cb_int1;
	}
  }
#line 23889 "parser.c" /* yacc.c:1646  */
    break;

  case 2174:
#line 14259 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = cb_int0;
	}
  }
#line 23901 "parser.c" /* yacc.c:1646  */
    break;

  case 2175:
#line 14270 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23909 "parser.c" /* yacc.c:1646  */
    break;

  case 2176:
#line 14274 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 23917 "parser.c" /* yacc.c:1646  */
    break;

  case 2177:
#line 14281 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_stop_literal_statement, _("STOP literal"));
  }
#line 23925 "parser.c" /* yacc.c:1646  */
    break;

  case 2178:
#line 14285 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_stop_identifier_statement, _("STOP identifier"));
  }
#line 23933 "parser.c" /* yacc.c:1646  */
    break;

  case 2179:
#line 14291 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 23939 "parser.c" /* yacc.c:1646  */
    break;

  case 2180:
#line 14292 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 23945 "parser.c" /* yacc.c:1646  */
    break;

  case 2181:
#line 14293 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 23951 "parser.c" /* yacc.c:1646  */
    break;

  case 2182:
#line 14294 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 23957 "parser.c" /* yacc.c:1646  */
    break;

  case 2183:
#line 14301 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STRING", TERM_STRING);
  }
#line 23965 "parser.c" /* yacc.c:1646  */
    break;

  case 2185:
#line 14310 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_string ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 23973 "parser.c" /* yacc.c:1646  */
    break;

  case 2186:
#line 14316 "parser.y" /* yacc.c:1646  */
    {
	save_tree = NULL;
  }
#line 23981 "parser.c" /* yacc.c:1646  */
    break;

  case 2187:
#line 14320 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = save_tree;
  }
#line 23989 "parser.c" /* yacc.c:1646  */
    break;

  case 2190:
#line 14332 "parser.y" /* yacc.c:1646  */
    {
	if (!save_tree) {
		save_tree = CB_LIST_INIT ((yyvsp[-1]));
	} else {
		save_tree = cb_list_add (save_tree, (yyvsp[-1]));
	}
	if ((yyvsp[0])) {
		save_tree = cb_list_add (save_tree, (yyvsp[0]));
	}
  }
#line 24004 "parser.c" /* yacc.c:1646  */
    break;

  case 2191:
#line 14345 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 24010 "parser.c" /* yacc.c:1646  */
    break;

  case 2192:
#line 14347 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 24016 "parser.c" /* yacc.c:1646  */
    break;

  case 2193:
#line 14351 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR (cb_int0, NULL); }
#line 24022 "parser.c" /* yacc.c:1646  */
    break;

  case 2194:
#line 14352 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR ((yyvsp[0]), NULL); }
#line 24028 "parser.c" /* yacc.c:1646  */
    break;

  case 2195:
#line 14356 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 24034 "parser.c" /* yacc.c:1646  */
    break;

  case 2196:
#line 14357 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 24040 "parser.c" /* yacc.c:1646  */
    break;

  case 2197:
#line 14362 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), STRING);
  }
#line 24048 "parser.c" /* yacc.c:1646  */
    break;

  case 2198:
#line 14366 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), STRING);
  }
#line 24056 "parser.c" /* yacc.c:1646  */
    break;

  case 2199:
#line 14376 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SUBTRACT", TERM_SUBTRACT);
  }
#line 24064 "parser.c" /* yacc.c:1646  */
    break;

  case 2201:
#line 14385 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '-', cb_build_binary_list ((yyvsp[-3]), '+'));
  }
#line 24072 "parser.c" /* yacc.c:1646  */
    break;

  case 2202:
#line 14389 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_list (CB_BUILD_CHAIN ((yyvsp[-3]), (yyvsp[-5])), '-'));
  }
#line 24080 "parser.c" /* yacc.c:1646  */
    break;

  case 2203:
#line 14393 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_corresponding (cb_build_sub, (yyvsp[-2]), (yyvsp[-4]), (yyvsp[-1]));
  }
#line 24088 "parser.c" /* yacc.c:1646  */
    break;

  case 2204:
#line 14397 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SUBTRACT TABLE");
	cb_emit_tab_arithmetic (cb_build_sub, (yyvsp[-4]), (yyvsp[-6]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 24097 "parser.c" /* yacc.c:1646  */
    break;

  case 2205:
#line 14405 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), SUBTRACT);
  }
#line 24105 "parser.c" /* yacc.c:1646  */
    break;

  case 2206:
#line 14409 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), SUBTRACT);
  }
#line 24113 "parser.c" /* yacc.c:1646  */
    break;

  case 2207:
#line 14419 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SUPPRESS", 0);
	if (!in_declaratives) {
		cb_error_x (CB_TREE (current_statement),
			    _("SUPPRESS statement must be within DECLARATIVES"));
	}
	cb_emit_suppress (control_field);
  }
#line 24126 "parser.c" /* yacc.c:1646  */
    break;

  case 2210:
#line 14437 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("TERMINATE", 0);
  }
#line 24134 "parser.c" /* yacc.c:1646  */
    break;

  case 2212:
#line 14445 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	    cb_emit_terminate ((yyvsp[0]));
	}
  }
#line 24145 "parser.c" /* yacc.c:1646  */
    break;

  case 2213:
#line 14452 "parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
		cb_emit_terminate ((yyvsp[0]));
	}
  }
#line 24156 "parser.c" /* yacc.c:1646  */
    break;

  case 2214:
#line 14464 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("TRANSFORM", 0);
  }
#line 24164 "parser.c" /* yacc.c:1646  */
    break;

  case 2216:
#line 14472 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_build_converting ((yyvsp[-2]), (yyvsp[0]), cb_build_inspect_region_start ());
	cb_emit_inspect ((yyvsp[-4]), x, TRANSFORM_STATEMENT);
  }
#line 24175 "parser.c" /* yacc.c:1646  */
    break;

  case 2217:
#line 14485 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("UNLOCK", 0);
  }
#line 24183 "parser.c" /* yacc.c:1646  */
    break;

  case 2219:
#line 14493 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[-1]))) {
		if (CB_FILE (cb_ref ((yyvsp[-1])))->organization == COB_ORG_SORT) {
			cb_error_x (CB_TREE (current_statement),
				    _("UNLOCK invalid for SORT files"));
		} else {
			cb_emit_unlock ((yyvsp[-1]));
		}
	}
  }
#line 24198 "parser.c" /* yacc.c:1646  */
    break;

  case 2220:
#line 14509 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("UNSTRING", TERM_UNSTRING);
  }
#line 24206 "parser.c" /* yacc.c:1646  */
    break;

  case 2222:
#line 14520 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_unstring ((yyvsp[-5]), (yyvsp[-4]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 24214 "parser.c" /* yacc.c:1646  */
    break;

  case 2223:
#line 14526 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 24220 "parser.c" /* yacc.c:1646  */
    break;

  case 2224:
#line 14528 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 24226 "parser.c" /* yacc.c:1646  */
    break;

  case 2225:
#line 14532 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 24232 "parser.c" /* yacc.c:1646  */
    break;

  case 2226:
#line 14534 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 24238 "parser.c" /* yacc.c:1646  */
    break;

  case 2227:
#line 14539 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_unstring_delimited ((yyvsp[-1]), (yyvsp[0]));
  }
#line 24246 "parser.c" /* yacc.c:1646  */
    break;

  case 2228:
#line 14545 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 24252 "parser.c" /* yacc.c:1646  */
    break;

  case 2229:
#line 14547 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 24258 "parser.c" /* yacc.c:1646  */
    break;

  case 2230:
#line 14552 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_unstring_into ((yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 24266 "parser.c" /* yacc.c:1646  */
    break;

  case 2231:
#line 14558 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 24272 "parser.c" /* yacc.c:1646  */
    break;

  case 2232:
#line 14559 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 24278 "parser.c" /* yacc.c:1646  */
    break;

  case 2233:
#line 14563 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 24284 "parser.c" /* yacc.c:1646  */
    break;

  case 2234:
#line 14564 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 24290 "parser.c" /* yacc.c:1646  */
    break;

  case 2235:
#line 14569 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), UNSTRING);
  }
#line 24298 "parser.c" /* yacc.c:1646  */
    break;

  case 2236:
#line 14573 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), UNSTRING);
  }
#line 24306 "parser.c" /* yacc.c:1646  */
    break;

  case 2237:
#line 14582 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("VALIDATE", 0);
  }
#line 24314 "parser.c" /* yacc.c:1646  */
    break;

  case 2238:
#line 14586 "parser.y" /* yacc.c:1646  */
    {
#if 0	/* FIXME: at least add syntax checks here */
	cb_emit_validate ((yyvsp[0]));
#else
	CB_PENDING ("VALIDATE");
#endif
  }
#line 24326 "parser.c" /* yacc.c:1646  */
    break;

  case 2239:
#line 14597 "parser.y" /* yacc.c:1646  */
    {
	check_validate_item ((yyvsp[0]));
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 24335 "parser.c" /* yacc.c:1646  */
    break;

  case 2240:
#line 14602 "parser.y" /* yacc.c:1646  */
    {
	check_validate_item ((yyvsp[0]));
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 24344 "parser.c" /* yacc.c:1646  */
    break;

  case 2241:
#line 14613 "parser.y" /* yacc.c:1646  */
    {
	skip_statements = 0;
	in_debugging = 0;
  }
#line 24353 "parser.c" /* yacc.c:1646  */
    break;

  case 2248:
#line 14631 "parser.y" /* yacc.c:1646  */
    {
	if (!in_declaratives) {
		cb_error (_("USE statement must be within DECLARATIVES"));
	} else if (!current_section) {
		cb_error (_("SECTION header missing before USE statement"));
	} else {
		current_section->flag_begin = 1;
		current_section->flag_return = 1;
		current_section->flag_declarative_exit = 1;
		current_section->flag_real_label = 1;
		current_section->flag_skip_label = 0;
		CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;
		if (use_global_ind) {
			current_section->flag_global = 1;
			current_program->global_list =
				cb_list_add (current_program->global_list,
					     CB_TREE (current_section));
		}
		emit_statement (cb_build_comment ("USE AFTER ERROR"));
	}
  }
#line 24379 "parser.c" /* yacc.c:1646  */
    break;

  case 2249:
#line 14656 "parser.y" /* yacc.c:1646  */
    {
	use_global_ind = 0;
  }
#line 24387 "parser.c" /* yacc.c:1646  */
    break;

  case 2250:
#line 14660 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else {
		use_global_ind = 1;
		current_program->flag_global_use = 1;
	}
  }
#line 24400 "parser.c" /* yacc.c:1646  */
    break;

  case 2251:
#line 14672 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			setup_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
		}
	}
  }
#line 24414 "parser.c" /* yacc.c:1646  */
    break;

  case 2252:
#line 14682 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_INPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_INPUT].handler_prog = current_program;
  }
#line 24423 "parser.c" /* yacc.c:1646  */
    break;

  case 2253:
#line 14687 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_OUTPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_OUTPUT].handler_prog = current_program;
  }
#line 24432 "parser.c" /* yacc.c:1646  */
    break;

  case 2254:
#line 14692 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_I_O].handler_label = current_section;
	current_program->global_handler[COB_OPEN_I_O].handler_prog = current_program;
  }
#line 24441 "parser.c" /* yacc.c:1646  */
    break;

  case 2255:
#line 14697 "parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_EXTEND].handler_label = current_section;
	current_program->global_handler[COB_OPEN_EXTEND].handler_prog = current_program;
  }
#line 24450 "parser.c" /* yacc.c:1646  */
    break;

  case 2256:
#line 14705 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		plabel;
	char		name[64];

	cb_verify (cb_use_for_debugging, "USE FOR DEBUGGING");

	if (!in_declaratives) {
		cb_error (_("USE statement must be within DECLARATIVES"));
	} else if (current_program->nested_level) {
		cb_error (_("USE DEBUGGING not supported in contained program"));
	} else {
		in_debugging = 1;
		current_section->flag_begin = 1;
		current_section->flag_return = 1;
		current_section->flag_declarative_exit = 1;
		current_section->flag_real_label = 0;
		current_section->flag_is_debug_sect = 1;
		if (!needs_debug_item) {
			needs_debug_item = 1;
			cb_build_debug_item ();
		}
		if (!current_program->flag_debugging) {
			skip_statements = 1;
			current_section->flag_skip_label = 1;
		} else {
			current_program->flag_gen_debug = 1;
			sprintf (name, "EXIT SECTION %d", cb_id);
			plabel = cb_build_reference (name);
			plabel = cb_build_label (plabel, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
			current_section->exit_label = plabel;
			emit_statement (cb_build_comment ("USE FOR DEBUGGING"));
		}
	}
  }
#line 24491 "parser.c" /* yacc.c:1646  */
    break;

  case 2259:
#line 14750 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;
	cb_tree		x;
	cb_tree		z;

	if (current_program->flag_debugging) {
		CB_REFERENCE ((yyvsp[0]))->debug_section = current_section;
		CB_REFERENCE ((yyvsp[0]))->flag_debug_code = 1;
		CB_REFERENCE ((yyvsp[0]))->flag_all_debug = 0;

		z = CB_LIST_INIT ((yyvsp[0]));
		current_program->debug_list =
			cb_list_append (current_program->debug_list, z);
		/* Check backward refs to file/data names */
		if (CB_WORD_COUNT ((yyvsp[0])) > 0) {
			l = CB_VALUE (CB_WORD_ITEMS ((yyvsp[0])));
			switch (CB_TREE_TAG (l)) {
			case CB_TAG_CD:
				if (CB_CD (l)->flag_field_debug) {
					cb_error_x ((yyvsp[0]), _("duplicate DEBUGGING target: '%s'"),
					    cb_name (l));
				} else {
					CB_CD (l)->debug_section = current_section;
					CB_CD (l)->flag_field_debug = 1;
				}
				break;
			case CB_TAG_FILE:
				if (CB_FILE (l)->flag_fl_debug) {
					cb_error_x ((yyvsp[0]), _("duplicate DEBUGGING target: '%s'"),
					    cb_name (l));
				} else {
					CB_FILE (l)->debug_section = current_section;
					CB_FILE (l)->flag_fl_debug = 1;
				}
				break;
			case CB_TAG_FIELD:
				x = cb_ref ((yyvsp[0]));
				if (CB_INVALID_TREE (x)) {
					break;
				}
				if (CB_FIELD (x)->flag_field_debug) {
					cb_error_x ((yyvsp[0]), _("duplicate DEBUGGING target: '%s'"),
					    cb_name (x));
				} else {
					needs_field_debug = 1;
					CB_FIELD (x)->debug_section = current_section;
					CB_FIELD (x)->flag_field_debug = 1;
					CB_PURPOSE (z) = x;
				}
				break;
			default:
				/* Label refs will be checked later (forward/backward ref) */
				break;
			}
		}
	}
  }
#line 24553 "parser.c" /* yacc.c:1646  */
    break;

  case 2260:
#line 14808 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_debugging) {
		if (current_program->all_procedure) {
			cb_error (_("duplicate USE DEBUGGING ON ALL PROCEDURES"));
		} else {
			current_program->all_procedure = current_section;
		}
	}
  }
#line 24567 "parser.c" /* yacc.c:1646  */
    break;

  case 2261:
#line 14818 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	if (current_program->flag_debugging) {
		/* Reference must be a data item */
		x = cb_ref ((yyvsp[0]));
		if (CB_INVALID_TREE (x) || !CB_FIELD_P (x)) {
			cb_error (_("invalid target for %s"), "DEBUGGING ALL");
		} else {
			if (CB_FIELD (x)->flag_field_debug) {
				cb_error_x ((yyvsp[0]), _("duplicate DEBUGGING target: '%s'"),
				    cb_name (x));
			} else {
				needs_field_debug = 1;
				CB_FIELD (x)->debug_section = current_section;
				CB_FIELD (x)->flag_field_debug = 1;
				CB_FIELD (x)->flag_all_debug = 1;
				CB_REFERENCE ((yyvsp[0]))->debug_section = current_section;
				CB_REFERENCE ((yyvsp[0]))->flag_debug_code = 1;
				CB_REFERENCE ((yyvsp[0]))->flag_all_debug = 1;
				CB_CHAIN_PAIR (current_program->debug_list, x, (yyvsp[0]));
			}
		}
	}
  }
#line 24597 "parser.c" /* yacc.c:1646  */
    break;

  case 2266:
#line 14853 "parser.y" /* yacc.c:1646  */
    {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "USE AT");
	}
  }
#line 24607 "parser.c" /* yacc.c:1646  */
    break;

  case 2267:
#line 14862 "parser.y" /* yacc.c:1646  */
    {
	emit_statement (cb_build_comment ("USE AT PROGRAM START"));
	backup_current_pos ();
	CB_PENDING ("USE AT PROGRAM START");
	/* emit_entry ("_AT_START", 0, NULL, NULL); */
  }
#line 24618 "parser.c" /* yacc.c:1646  */
    break;

  case 2268:
#line 14869 "parser.y" /* yacc.c:1646  */
    {
	emit_statement (cb_build_comment ("USE AT PROGRAM END"));
	backup_current_pos ();
	CB_PENDING ("USE AT PROGRAM END");
	/* emit_entry ("_AT_END", 0, NULL, NULL); */
  }
#line 24629 "parser.c" /* yacc.c:1646  */
    break;

  case 2269:
#line 14880 "parser.y" /* yacc.c:1646  */
    {
	char wrk[80];
	cb_tree x;
	struct cb_field		*f;
	struct cb_report	*r;

	x = cb_ref ((yyvsp[0]));
	if (!CB_FIELD_P (x)) {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report group"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	} else {
		control_field = f = CB_FIELD (x);
		f->report_decl_id = current_section->id;
		if ((r = f->report) != NULL) {
			r->has_declarative = 1;
		}
	}
	sprintf(wrk,"USE BEFORE REPORTING %s is l_%d",cb_name((yyvsp[0])),current_section->id);
	current_section->flag_real_label = 1;
	current_section->flag_declaratives = 1;
	current_section->flag_begin = 1;
	current_section->flag_return = 1;
	current_section->flag_declarative_exit = 1;
	current_section->flag_real_label = 1;
	current_section->flag_skip_label = 0;
	emit_statement (cb_build_comment (strdup(wrk)));
  }
#line 24661 "parser.c" /* yacc.c:1646  */
    break;

  case 2272:
#line 14916 "parser.y" /* yacc.c:1646  */
    {
	current_section->flag_real_label = 1;
	emit_statement (cb_build_comment ("USE AFTER EXCEPTION CONDITION"));
	CB_PENDING ("USE AFTER EXCEPTION CONDITION");
  }
#line 24671 "parser.c" /* yacc.c:1646  */
    break;

  case 2273:
#line 14922 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			setup_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
		}
	}
	current_section->flag_real_label = 1;
	emit_statement(cb_build_comment("USE AFTER EXCEPTION CONDITION"));
	CB_PENDING("USE AFTER EXCEPTION CONDITION");
  }
#line 24688 "parser.c" /* yacc.c:1646  */
    break;

  case 2276:
#line 14945 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("WRITE", TERM_WRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 24699 "parser.c" /* yacc.c:1646  */
    break;

  case 2278:
#line 14957 "parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[-5]))) {
		cb_emit_write ((yyvsp[-5]), (yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]));
	}
	start_debug = save_debug;
  }
#line 24710 "parser.c" /* yacc.c:1646  */
    break;

  case 2279:
#line 14966 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 24716 "parser.c" /* yacc.c:1646  */
    break;

  case 2280:
#line 14967 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 24722 "parser.c" /* yacc.c:1646  */
    break;

  case 2281:
#line 14972 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 24730 "parser.c" /* yacc.c:1646  */
    break;

  case 2282:
#line 14976 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_lines ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 24738 "parser.c" /* yacc.c:1646  */
    break;

  case 2283:
#line 14980 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_mnemonic ((yyvsp[-2]), (yyvsp[0]));
  }
#line 24746 "parser.c" /* yacc.c:1646  */
    break;

  case 2284:
#line 14984 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_page ((yyvsp[-2]));
  }
#line 24754 "parser.c" /* yacc.c:1646  */
    break;

  case 2285:
#line 14990 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BEFORE; }
#line 24760 "parser.c" /* yacc.c:1646  */
    break;

  case 2286:
#line 14991 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_AFTER; }
#line 24766 "parser.c" /* yacc.c:1646  */
    break;

  case 2290:
#line 15002 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), WRITE);
  }
#line 24774 "parser.c" /* yacc.c:1646  */
    break;

  case 2291:
#line 15006 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), WRITE);
  }
#line 24782 "parser.c" /* yacc.c:1646  */
    break;

  case 2292:
#line 15015 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("XML GENERATE", TERM_XML);
	cb_verify (cb_xml_generate, _("XML GENERATE"));
	cobc_in_xml_generate_body = 1;
	cobc_cs_check = CB_CS_XML_GENERATE;
  }
#line 24793 "parser.c" /* yacc.c:1646  */
    break;

  case 2294:
#line 15028 "parser.y" /* yacc.c:1646  */
    {
	xml_encoding = NULL;
	with_xml_dec = 0;
	with_attrs = 0;
	ml_suppress_list = NULL;
  }
#line 24804 "parser.c" /* yacc.c:1646  */
    break;

  case 2295:
#line 15039 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_xml_generate_body = 0;
	cobc_cs_check = 0;
  }
#line 24813 "parser.c" /* yacc.c:1646  */
    break;

  case 2296:
#line 15044 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_xml_generate ((yyvsp[-11]), (yyvsp[-9]), (yyvsp[-8]), xml_encoding, with_xml_dec,
			      with_attrs, (yyvsp[-5]), (yyvsp[-4]), (yyvsp[-3]), ml_suppress_list);
  }
#line 24822 "parser.c" /* yacc.c:1646  */
    break;

  case 2302:
#line 15066 "parser.y" /* yacc.c:1646  */
    {
	xml_encoding = (yyvsp[0]);
	if (with_xml_dec) {
		cb_error (_("ENCODING clause must come before XML-DECLARATION"));
	} else if (with_attrs) {
		cb_error (_("ENCODING clause must come before ATTRIBUTES"));
	}
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE ENCODING clause"));
	CB_PENDING ("XML GENERATE ENCODING");
  }
#line 24838 "parser.c" /* yacc.c:1646  */
    break;

  case 2303:
#line 15078 "parser.y" /* yacc.c:1646  */
    {
	with_xml_dec = 1;
	if (with_attrs) {
		cb_error (_("XML-DECLARATION clause must come before ATTRIBUTES"));
	}
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE XML-DECLARATION clause"));
  }
#line 24851 "parser.c" /* yacc.c:1646  */
    break;

  case 2304:
#line 15087 "parser.y" /* yacc.c:1646  */
    {
	with_attrs = 1;
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE WITH ATTRIBUTES clause"));
  }
#line 24861 "parser.c" /* yacc.c:1646  */
    break;

  case 2305:
#line 15096 "parser.y" /* yacc.c:1646  */
    {
	 (yyval) = NULL;
  }
#line 24869 "parser.c" /* yacc.c:1646  */
    break;

  case 2306:
#line 15100 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-1]), (yyvsp[0]));
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE NAMESPACE clause"));
  }
#line 24879 "parser.c" /* yacc.c:1646  */
    break;

  case 2307:
#line 15109 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 24887 "parser.c" /* yacc.c:1646  */
    break;

  case 2308:
#line 15113 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 24895 "parser.c" /* yacc.c:1646  */
    break;

  case 2309:
#line 15120 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 24903 "parser.c" /* yacc.c:1646  */
    break;

  case 2310:
#line 15124 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE NAME OF clause"));
  }
#line 24913 "parser.c" /* yacc.c:1646  */
    break;

  case 2311:
#line 15133 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 24921 "parser.c" /* yacc.c:1646  */
    break;

  case 2312:
#line 15137 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 24929 "parser.c" /* yacc.c:1646  */
    break;

  case 2313:
#line 15144 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
  }
#line 24937 "parser.c" /* yacc.c:1646  */
    break;

  case 2314:
#line 15151 "parser.y" /* yacc.c:1646  */
    {
       (yyval) = NULL;
  }
#line 24945 "parser.c" /* yacc.c:1646  */
    break;

  case 2315:
#line 15155 "parser.y" /* yacc.c:1646  */
    {
       (yyval) = (yyvsp[0]);
       	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE TYPE OF clause"));
  }
#line 24955 "parser.c" /* yacc.c:1646  */
    break;

  case 2316:
#line 15164 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 24963 "parser.c" /* yacc.c:1646  */
    break;

  case 2317:
#line 15168 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 24971 "parser.c" /* yacc.c:1646  */
    break;

  case 2318:
#line 15175 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
  }
#line 24979 "parser.c" /* yacc.c:1646  */
    break;

  case 2319:
#line 15182 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int) CB_ML_ANY_TYPE);
  }
#line 24987 "parser.c" /* yacc.c:1646  */
    break;

  case 2321:
#line 15189 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((int) CB_ML_ATTRIBUTE); }
#line 24993 "parser.c" /* yacc.c:1646  */
    break;

  case 2322:
#line 15190 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((int) CB_ML_ELEMENT); }
#line 24999 "parser.c" /* yacc.c:1646  */
    break;

  case 2323:
#line 15191 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((int) CB_ML_CONTENT); }
#line 25005 "parser.c" /* yacc.c:1646  */
    break;

  case 2325:
#line 15197 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE SUPPRESS clause"));
  }
#line 25014 "parser.c" /* yacc.c:1646  */
    break;

  case 2328:
#line 15210 "parser.y" /* yacc.c:1646  */
    {
	error_if_following_every_clause ();
	add_identifier_to_ml_suppress_conds ((yyvsp[0]));
  }
#line 25023 "parser.c" /* yacc.c:1646  */
    break;

  case 2329:
#line 15215 "parser.y" /* yacc.c:1646  */
    {
	error_if_following_every_clause ();
	add_type_to_ml_suppress_conds (ml_suppress_category, (enum cb_ml_type) CB_INTEGER ((yyvsp[0]))->val);
  }
#line 25032 "parser.c" /* yacc.c:1646  */
    break;

  case 2330:
#line 15220 "parser.y" /* yacc.c:1646  */
    {
	add_when_to_ml_suppress_conds ((yyvsp[0]));
  }
#line 25040 "parser.c" /* yacc.c:1646  */
    break;

  case 2331:
#line 15227 "parser.y" /* yacc.c:1646  */
    {
	ml_suppress_category = CB_ML_SUPPRESS_CAT_NUMERIC;
	(yyval) = (yyvsp[0]);
  }
#line 25049 "parser.c" /* yacc.c:1646  */
    break;

  case 2332:
#line 15232 "parser.y" /* yacc.c:1646  */
    {
	ml_suppress_category = CB_ML_SUPPRESS_CAT_NONNUMERIC;
	(yyval) = (yyvsp[0]);
  }
#line 25058 "parser.c" /* yacc.c:1646  */
    break;

  case 2333:
#line 15237 "parser.y" /* yacc.c:1646  */
    {
	ml_suppress_category = CB_ML_SUPPRESS_CAT_ANY;
	(yyval) = (yyvsp[0]);
  }
#line 25067 "parser.c" /* yacc.c:1646  */
    break;

  case 2334:
#line 15245 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 25075 "parser.c" /* yacc.c:1646  */
    break;

  case 2335:
#line 15249 "parser.y" /* yacc.c:1646  */
    {
       (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0]));
  }
#line 25083 "parser.c" /* yacc.c:1646  */
    break;

  case 2336:
#line 15256 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), XML);
  }
#line 25091 "parser.c" /* yacc.c:1646  */
    break;

  case 2337:
#line 15260 "parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), XML);
  }
#line 25099 "parser.c" /* yacc.c:1646  */
    break;

  case 2338:
#line 15270 "parser.y" /* yacc.c:1646  */
    {
	begin_statement ("XML PARSE", TERM_XML);
	/*kamal079 - xml parse changes */
	cb_verify (cb_xml_parse, _("XML PARSE"));
	cobc_in_xml_parse_body = 1;
	cobc_cs_check = CB_CS_XML_PARSE;
  }
#line 25111 "parser.c" /* yacc.c:1646  */
    break;

  case 2340:
#line 15289 "parser.y" /* yacc.c:1646  */
    {
	cobc_in_xml_parse_body = 0;
	cobc_cs_check = 0;
  }
#line 25120 "parser.c" /* yacc.c:1646  */
    break;

  case 2341:
#line 15294 "parser.y" /* yacc.c:1646  */
    {
	cb_emit_xml_parse((yyvsp[-6]),(yyvsp[-2]));
  }
#line 25128 "parser.c" /* yacc.c:1646  */
    break;

  case 2344:
#line 15334 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 25139 "parser.c" /* yacc.c:1646  */
    break;

  case 2345:
#line 15344 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25147 "parser.c" /* yacc.c:1646  */
    break;

  case 2346:
#line 15348 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 25155 "parser.c" /* yacc.c:1646  */
    break;

  case 2347:
#line 15355 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 25164 "parser.c" /* yacc.c:1646  */
    break;

  case 2352:
#line 15373 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 25173 "parser.c" /* yacc.c:1646  */
    break;

  case 2357:
#line 15389 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 25184 "parser.c" /* yacc.c:1646  */
    break;

  case 2358:
#line 15399 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25192 "parser.c" /* yacc.c:1646  */
    break;

  case 2359:
#line 15403 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 25200 "parser.c" /* yacc.c:1646  */
    break;

  case 2360:
#line 15410 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 25209 "parser.c" /* yacc.c:1646  */
    break;

  case 2363:
#line 15423 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 25218 "parser.c" /* yacc.c:1646  */
    break;

  case 2366:
#line 15433 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			   _("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 25229 "parser.c" /* yacc.c:1646  */
    break;

  case 2367:
#line 15443 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25237 "parser.c" /* yacc.c:1646  */
    break;

  case 2368:
#line 15447 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 25245 "parser.c" /* yacc.c:1646  */
    break;

  case 2369:
#line 15454 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = XML_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 25254 "parser.c" /* yacc.c:1646  */
    break;

  case 2372:
#line 15467 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = XML_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 25263 "parser.c" /* yacc.c:1646  */
    break;

  case 2375:
#line 15477 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			   _("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 25274 "parser.c" /* yacc.c:1646  */
    break;

  case 2376:
#line 15487 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25282 "parser.c" /* yacc.c:1646  */
    break;

  case 2377:
#line 15491 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 25290 "parser.c" /* yacc.c:1646  */
    break;

  case 2378:
#line 15498 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = JSON_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 25299 "parser.c" /* yacc.c:1646  */
    break;

  case 2381:
#line 15511 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = JSON_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 25308 "parser.c" /* yacc.c:1646  */
    break;

  case 2384:
#line 15523 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT SIZE ERROR before SIZE ERROR"));
	}
  }
#line 25319 "parser.c" /* yacc.c:1646  */
    break;

  case 2385:
#line 15533 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25327 "parser.c" /* yacc.c:1646  */
    break;

  case 2386:
#line 15537 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 25335 "parser.c" /* yacc.c:1646  */
    break;

  case 2387:
#line 15544 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 25344 "parser.c" /* yacc.c:1646  */
    break;

  case 2390:
#line 15557 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 25353 "parser.c" /* yacc.c:1646  */
    break;

  case 2393:
#line 15569 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT OVERFLOW before OVERFLOW"));
	}
  }
#line 25364 "parser.c" /* yacc.c:1646  */
    break;

  case 2394:
#line 15579 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25372 "parser.c" /* yacc.c:1646  */
    break;

  case 2395:
#line 15583 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 25380 "parser.c" /* yacc.c:1646  */
    break;

  case 2396:
#line 15590 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 25389 "parser.c" /* yacc.c:1646  */
    break;

  case 2399:
#line 15603 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 25398 "parser.c" /* yacc.c:1646  */
    break;

  case 2401:
#line 15615 "parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
  }
#line 25406 "parser.c" /* yacc.c:1646  */
    break;

  case 2403:
#line 15624 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
	}
  }
#line 25416 "parser.c" /* yacc.c:1646  */
    break;

  case 2404:
#line 15633 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25424 "parser.c" /* yacc.c:1646  */
    break;

  case 2405:
#line 15637 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 25432 "parser.c" /* yacc.c:1646  */
    break;

  case 2406:
#line 15644 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 25441 "parser.c" /* yacc.c:1646  */
    break;

  case 2409:
#line 15657 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 25450 "parser.c" /* yacc.c:1646  */
    break;

  case 2411:
#line 15668 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT AT END-OF-PAGE before AT END-OF-PAGE"));
	}
  }
#line 25461 "parser.c" /* yacc.c:1646  */
    break;

  case 2412:
#line 15678 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25469 "parser.c" /* yacc.c:1646  */
    break;

  case 2413:
#line 15682 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 25477 "parser.c" /* yacc.c:1646  */
    break;

  case 2414:
#line 15689 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 25486 "parser.c" /* yacc.c:1646  */
    break;

  case 2417:
#line 15702 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 25495 "parser.c" /* yacc.c:1646  */
    break;

  case 2421:
#line 15718 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT INVALID KEY before INVALID KEY"));
	}
  }
#line 25506 "parser.c" /* yacc.c:1646  */
    break;

  case 2422:
#line 15728 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25514 "parser.c" /* yacc.c:1646  */
    break;

  case 2423:
#line 15732 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 25522 "parser.c" /* yacc.c:1646  */
    break;

  case 2424:
#line 15739 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 25531 "parser.c" /* yacc.c:1646  */
    break;

  case 2427:
#line 15752 "parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 25540 "parser.c" /* yacc.c:1646  */
    break;

  case 2428:
#line 15762 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25548 "parser.c" /* yacc.c:1646  */
    break;

  case 2429:
#line 15766 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
	CB_PENDING ("THREAD");
  }
#line 25557 "parser.c" /* yacc.c:1646  */
    break;

  case 2430:
#line 15774 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25565 "parser.c" /* yacc.c:1646  */
    break;

  case 2431:
#line 15778 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_PENDING ("THREAD");
  }
#line 25574 "parser.c" /* yacc.c:1646  */
    break;

  case 2432:
#line 15786 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 25582 "parser.c" /* yacc.c:1646  */
    break;

  case 2433:
#line 15790 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25590 "parser.c" /* yacc.c:1646  */
    break;

  case 2434:
#line 15799 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_one;
  }
#line 25598 "parser.c" /* yacc.c:1646  */
    break;

  case 2435:
#line 15803 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 25606 "parser.c" /* yacc.c:1646  */
    break;

  case 2436:
#line 15809 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 25612 "parser.c" /* yacc.c:1646  */
    break;

  case 2437:
#line 15810 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 25618 "parser.c" /* yacc.c:1646  */
    break;

  case 2438:
#line 15817 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_cond ((yyvsp[0]));
	cb_end_cond ((yyval));
  }
#line 25627 "parser.c" /* yacc.c:1646  */
    break;

  case 2439:
#line 15822 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_error_node;
	cb_end_cond ((yyval));
  }
#line 25636 "parser.c" /* yacc.c:1646  */
    break;

  case 2440:
#line 15830 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_expr ((yyvsp[0]));
  }
#line 25644 "parser.c" /* yacc.c:1646  */
    break;

  case 2441:
#line 15836 "parser.y" /* yacc.c:1646  */
    {
	current_expr = NULL;
	cb_exp_line = cb_source_line;
  }
#line 25653 "parser.c" /* yacc.c:1646  */
    break;

  case 2442:
#line 15841 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_reverse (current_expr);
  }
#line 25661 "parser.c" /* yacc.c:1646  */
    break;

  case 2445:
#line 15852 "parser.y" /* yacc.c:1646  */
    { push_expr ('x', (yyvsp[0])); }
#line 25667 "parser.c" /* yacc.c:1646  */
    break;

  case 2448:
#line 15857 "parser.y" /* yacc.c:1646  */
    { push_expr ('x', cb_zero); }
#line 25673 "parser.c" /* yacc.c:1646  */
    break;

  case 2449:
#line 15859 "parser.y" /* yacc.c:1646  */
    { push_expr ('(', NULL); }
#line 25679 "parser.c" /* yacc.c:1646  */
    break;

  case 2450:
#line 15860 "parser.y" /* yacc.c:1646  */
    { push_expr (')', NULL); }
#line 25685 "parser.c" /* yacc.c:1646  */
    break;

  case 2451:
#line 15862 "parser.y" /* yacc.c:1646  */
    { push_expr ('+', NULL); }
#line 25691 "parser.c" /* yacc.c:1646  */
    break;

  case 2452:
#line 15863 "parser.y" /* yacc.c:1646  */
    { push_expr ('-', NULL); }
#line 25697 "parser.c" /* yacc.c:1646  */
    break;

  case 2453:
#line 15864 "parser.y" /* yacc.c:1646  */
    { push_expr ('*', NULL); }
#line 25703 "parser.c" /* yacc.c:1646  */
    break;

  case 2454:
#line 15865 "parser.y" /* yacc.c:1646  */
    { push_expr ('/', NULL); }
#line 25709 "parser.c" /* yacc.c:1646  */
    break;

  case 2455:
#line 15866 "parser.y" /* yacc.c:1646  */
    { push_expr ('^', NULL); }
#line 25715 "parser.c" /* yacc.c:1646  */
    break;

  case 2457:
#line 15869 "parser.y" /* yacc.c:1646  */
    { push_expr ('&', NULL); }
#line 25721 "parser.c" /* yacc.c:1646  */
    break;

  case 2458:
#line 15870 "parser.y" /* yacc.c:1646  */
    { push_expr ('|', NULL); }
#line 25727 "parser.c" /* yacc.c:1646  */
    break;

  case 2461:
#line 15879 "parser.y" /* yacc.c:1646  */
    { push_expr ('!', NULL); }
#line 25733 "parser.c" /* yacc.c:1646  */
    break;

  case 2462:
#line 15882 "parser.y" /* yacc.c:1646  */
    { push_expr ('C', (yyvsp[0])); }
#line 25739 "parser.c" /* yacc.c:1646  */
    break;

  case 2463:
#line 15884 "parser.y" /* yacc.c:1646  */
    { push_expr ('=', NULL); }
#line 25745 "parser.c" /* yacc.c:1646  */
    break;

  case 2464:
#line 15885 "parser.y" /* yacc.c:1646  */
    { push_expr ('>', NULL); }
#line 25751 "parser.c" /* yacc.c:1646  */
    break;

  case 2465:
#line 15886 "parser.y" /* yacc.c:1646  */
    { push_expr ('<', NULL); }
#line 25757 "parser.c" /* yacc.c:1646  */
    break;

  case 2466:
#line 15887 "parser.y" /* yacc.c:1646  */
    { push_expr (']', NULL); }
#line 25763 "parser.c" /* yacc.c:1646  */
    break;

  case 2467:
#line 15888 "parser.y" /* yacc.c:1646  */
    { push_expr ('[', NULL); }
#line 25769 "parser.c" /* yacc.c:1646  */
    break;

  case 2468:
#line 15889 "parser.y" /* yacc.c:1646  */
    { push_expr ('~', NULL); }
#line 25775 "parser.c" /* yacc.c:1646  */
    break;

  case 2469:
#line 15891 "parser.y" /* yacc.c:1646  */
    { push_expr ('O', NULL); }
#line 25781 "parser.c" /* yacc.c:1646  */
    break;

  case 2470:
#line 15892 "parser.y" /* yacc.c:1646  */
    { push_expr ('9', NULL); }
#line 25787 "parser.c" /* yacc.c:1646  */
    break;

  case 2471:
#line 15893 "parser.y" /* yacc.c:1646  */
    { push_expr ('A', NULL); }
#line 25793 "parser.c" /* yacc.c:1646  */
    break;

  case 2472:
#line 15894 "parser.y" /* yacc.c:1646  */
    { push_expr ('L', NULL); }
#line 25799 "parser.c" /* yacc.c:1646  */
    break;

  case 2473:
#line 15895 "parser.y" /* yacc.c:1646  */
    { push_expr ('U', NULL); }
#line 25805 "parser.c" /* yacc.c:1646  */
    break;

  case 2474:
#line 15898 "parser.y" /* yacc.c:1646  */
    { push_expr ('P', NULL); }
#line 25811 "parser.c" /* yacc.c:1646  */
    break;

  case 2475:
#line 15899 "parser.y" /* yacc.c:1646  */
    { push_expr ('N', NULL); }
#line 25817 "parser.c" /* yacc.c:1646  */
    break;

  case 2484:
#line 15929 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 25825 "parser.c" /* yacc.c:1646  */
    break;

  case 2485:
#line 15933 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0]));
  }
#line 25833 "parser.c" /* yacc.c:1646  */
    break;

  case 2489:
#line 15945 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '+', (yyvsp[0])); }
#line 25839 "parser.c" /* yacc.c:1646  */
    break;

  case 2490:
#line 15946 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '-', (yyvsp[0])); }
#line 25845 "parser.c" /* yacc.c:1646  */
    break;

  case 2491:
#line 15947 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 25851 "parser.c" /* yacc.c:1646  */
    break;

  case 2492:
#line 15951 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '*', (yyvsp[0])); }
#line 25857 "parser.c" /* yacc.c:1646  */
    break;

  case 2493:
#line 15952 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '/', (yyvsp[0])); }
#line 25863 "parser.c" /* yacc.c:1646  */
    break;

  case 2494:
#line 15953 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 25869 "parser.c" /* yacc.c:1646  */
    break;

  case 2495:
#line 15958 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_binary_op ((yyvsp[-2]), '^', (yyvsp[0]));
  }
#line 25877 "parser.c" /* yacc.c:1646  */
    break;

  case 2496:
#line 15961 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 25883 "parser.c" /* yacc.c:1646  */
    break;

  case 2497:
#line 15965 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 25889 "parser.c" /* yacc.c:1646  */
    break;

  case 2498:
#line 15966 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op (cb_zero, '-', (yyvsp[0])); }
#line 25895 "parser.c" /* yacc.c:1646  */
    break;

  case 2499:
#line 15967 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 25901 "parser.c" /* yacc.c:1646  */
    break;

  case 2500:
#line 15970 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 25907 "parser.c" /* yacc.c:1646  */
    break;

  case 2501:
#line 15971 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 25913 "parser.c" /* yacc.c:1646  */
    break;

  case 2502:
#line 15982 "parser.y" /* yacc.c:1646  */
    {
	if (current_linage > 1) {
		cb_error (_("LINAGE-COUNTER must be qualified here"));
		(yyval) = cb_error_node;
	} else if (current_linage == 0) {
		cb_error (_("invalid LINAGE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = linage_file->linage_ctr;
	}
  }
#line 25929 "parser.c" /* yacc.c:1646  */
    break;

  case 2503:
#line 15994 "parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = CB_FILE (cb_ref ((yyvsp[0])))->linage_ctr;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 25942 "parser.c" /* yacc.c:1646  */
    break;

  case 2504:
#line 16003 "parser.y" /* yacc.c:1646  */
    {
	if (report_count > 1) {
		if (current_report != NULL) {
			(yyval) = current_report->line_counter;
		} else {
			cb_error (_("LINE-COUNTER must be qualified here"));
			(yyval) = cb_error_node;
		}
	} else if (report_count == 0) {
		cb_error (_("invalid LINE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = report_instance->line_counter;
	}
  }
#line 25962 "parser.c" /* yacc.c:1646  */
    break;

  case 2505:
#line 16019 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REF_OR_REPORT_P ((yyvsp[0]))) {
		(yyval) = CB_REPORT_PTR ((yyvsp[0]))->line_counter;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 25975 "parser.c" /* yacc.c:1646  */
    break;

  case 2506:
#line 16028 "parser.y" /* yacc.c:1646  */
    {
	if (report_count > 1) {
		if (current_report != NULL) {
			(yyval) = current_report->page_counter;
		} else {
			cb_error (_("PAGE-COUNTER must be qualified here"));
			(yyval) = cb_error_node;
		}
	} else if (report_count == 0) {
		cb_error (_("invalid PAGE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = report_instance->page_counter;
	}
  }
#line 25995 "parser.c" /* yacc.c:1646  */
    break;

  case 2507:
#line 16044 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REF_OR_REPORT_P ((yyvsp[0]))) {
		(yyval) = CB_REPORT_PTR ((yyvsp[0]))->page_counter;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 26008 "parser.c" /* yacc.c:1646  */
    break;

  case 2508:
#line 16058 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 26014 "parser.c" /* yacc.c:1646  */
    break;

  case 2509:
#line 16060 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 26020 "parser.c" /* yacc.c:1646  */
    break;

  case 2510:
#line 16065 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-1]));
  }
#line 26028 "parser.c" /* yacc.c:1646  */
    break;

  case 2511:
#line 16073 "parser.y" /* yacc.c:1646  */
    { cb_build_identifier ((yyvsp[0]), 0); }
#line 26034 "parser.c" /* yacc.c:1646  */
    break;

  case 2512:
#line 16080 "parser.y" /* yacc.c:1646  */
    {
	if (!CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("%s requires a record name as subject"),
			current_statement->name);
		(yyval) = cb_error_node;
	}
  }
#line 26048 "parser.c" /* yacc.c:1646  */
    break;

  case 2513:
#line 16090 "parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 26061 "parser.c" /* yacc.c:1646  */
    break;

  case 2514:
#line 16104 "parser.y" /* yacc.c:1646  */
    {
	cb_tree x;

	x = cb_ref ((yyvsp[0]));
	if (!CB_FIELD_P (x)) {
		(yyval) = cb_error_node;
	} else if (!CB_FIELD (x)->index_list) {
		cb_error_x ((yyvsp[0]), _("'%s' not indexed"), cb_name ((yyvsp[0])));
		listprint_suppress ();
		cb_error_x (x, _("'%s' defined here"), cb_name (x));
		listprint_restore ();
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 26082 "parser.c" /* yacc.c:1646  */
    break;

  case 2515:
#line 16126 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 26090 "parser.c" /* yacc.c:1646  */
    break;

  case 2516:
#line 16130 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	if (CB_VALID_TREE ((yyvsp[0]))) {
		for (l = (yyvsp[-1]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l)) &&
			    !strcasecmp (CB_NAME ((yyvsp[0])), CB_NAME (CB_VALUE (l)))) {
				cb_error_x ((yyvsp[0]), _("multiple reference to '%s' "),
					    CB_NAME ((yyvsp[0])));
				break;
			}
		}
		if (!l) {
			(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
		}
	}
  }
#line 26112 "parser.c" /* yacc.c:1646  */
    break;

  case 2517:
#line 16151 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 26120 "parser.c" /* yacc.c:1646  */
    break;

  case 2518:
#line 16155 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	if (CB_VALID_TREE ((yyvsp[0]))) {
		for (l = (yyvsp[-2]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l)) &&
			    !strcasecmp (CB_NAME ((yyvsp[0])), CB_NAME (CB_VALUE (l)))) {
				cb_error_x ((yyvsp[0]), _("multiple reference to '%s' "),
					    CB_NAME ((yyvsp[-1])));
				break;
			}
		}
		if (!l) {
			(yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0]));
		}
	}
  }
#line 26142 "parser.c" /* yacc.c:1646  */
    break;

  case 2519:
#line 16176 "parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 26155 "parser.c" /* yacc.c:1646  */
    break;

  case 2520:
#line 16188 "parser.y" /* yacc.c:1646  */
    {
	if (CB_CD_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a CD name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 26168 "parser.c" /* yacc.c:1646  */
    break;

  case 2521:
#line 16202 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REF_OR_REPORT_P ((yyvsp[0]))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error (_("'%s' is not a valid report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 26181 "parser.c" /* yacc.c:1646  */
    break;

  case 2522:
#line 16215 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 26187 "parser.c" /* yacc.c:1646  */
    break;

  case 2523:
#line 16217 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 26193 "parser.c" /* yacc.c:1646  */
    break;

  case 2524:
#line 16221 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 26199 "parser.c" /* yacc.c:1646  */
    break;

  case 2525:
#line 16227 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 26205 "parser.c" /* yacc.c:1646  */
    break;

  case 2526:
#line 16229 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 26211 "parser.c" /* yacc.c:1646  */
    break;

  case 2527:
#line 16234 "parser.y" /* yacc.c:1646  */
    {
	struct cb_reference *r = CB_REFERENCE ((yyvsp[0]));

	r->offset = CB_TREE (current_section);
	r->flag_in_decl = !!in_declaratives;
	r->flag_ignored = cb_set_ignore_error (-1);

	(yyval) = (yyvsp[0]);
	CB_ADD_TO_CHAIN ((yyvsp[0]), current_program->label_list);
  }
#line 26226 "parser.c" /* yacc.c:1646  */
    break;

  case 2530:
#line 16250 "parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-2]))->chain = (yyvsp[0]);
  }
#line 26234 "parser.c" /* yacc.c:1646  */
    break;

  case 2531:
#line 16257 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_reference ((char *)(CB_LITERAL ((yyvsp[0]))->data));
	(yyval)->source_file = (yyvsp[0])->source_file;
	(yyval)->source_line = (yyvsp[0])->source_line;
  }
#line 26244 "parser.c" /* yacc.c:1646  */
    break;

  case 2532:
#line 16267 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 26250 "parser.c" /* yacc.c:1646  */
    break;

  case 2533:
#line 16268 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 26256 "parser.c" /* yacc.c:1646  */
    break;

  case 2534:
#line 16273 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 26265 "parser.c" /* yacc.c:1646  */
    break;

  case 2535:
#line 16280 "parser.y" /* yacc.c:1646  */
    {(yyval) = NULL;}
#line 26271 "parser.c" /* yacc.c:1646  */
    break;

  case 2536:
#line 16281 "parser.y" /* yacc.c:1646  */
    {(yyval) = (yyvsp[0]);}
#line 26277 "parser.c" /* yacc.c:1646  */
    break;

  case 2537:
#line 16285 "parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 26283 "parser.c" /* yacc.c:1646  */
    break;

  case 2538:
#line 16286 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 26289 "parser.c" /* yacc.c:1646  */
    break;

  case 2539:
#line 16291 "parser.y" /* yacc.c:1646  */
    {
	CB_ADD_TO_CHAIN ((yyvsp[0]), current_program->reference_list);
  }
#line 26297 "parser.c" /* yacc.c:1646  */
    break;

  case 2540:
#line 16301 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 26305 "parser.c" /* yacc.c:1646  */
    break;

  case 2541:
#line 16305 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 26313 "parser.c" /* yacc.c:1646  */
    break;

  case 2542:
#line 16312 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_REFERENCE((yyval))->flag_optional = 1;
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 26323 "parser.c" /* yacc.c:1646  */
    break;

  case 2545:
#line 16328 "parser.y" /* yacc.c:1646  */
    {
	if (CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 26336 "parser.c" /* yacc.c:1646  */
    break;

  case 2546:
#line 16337 "parser.y" /* yacc.c:1646  */
    {
	yyclearin;
	yyerrok;
	(yyval) = cb_error_node;
  }
#line 26346 "parser.c" /* yacc.c:1646  */
    break;

  case 2547:
#line 16348 "parser.y" /* yacc.c:1646  */
    {
	if (CB_REFERENCE ((yyvsp[0]))->flag_duped || CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
		(yyval) = NULL;
	} else {
		CB_WORD_COUNT ((yyvsp[0]))++;
		(yyval) = (yyvsp[0]);
	}
  }
#line 26360 "parser.c" /* yacc.c:1646  */
    break;

  case 2548:
#line 16365 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 26368 "parser.c" /* yacc.c:1646  */
    break;

  case 2549:
#line 16369 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 26376 "parser.c" /* yacc.c:1646  */
    break;

  case 2552:
#line 16378 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_address ((yyvsp[0]));
  }
#line 26384 "parser.c" /* yacc.c:1646  */
    break;

  case 2553:
#line 16384 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 26390 "parser.c" /* yacc.c:1646  */
    break;

  case 2554:
#line 16385 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 26396 "parser.c" /* yacc.c:1646  */
    break;

  case 2555:
#line 16390 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 26404 "parser.c" /* yacc.c:1646  */
    break;

  case 2556:
#line 16394 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 26412 "parser.c" /* yacc.c:1646  */
    break;

  case 2564:
#line 16414 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 26420 "parser.c" /* yacc.c:1646  */
    break;

  case 2565:
#line 16418 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 26428 "parser.c" /* yacc.c:1646  */
    break;

  case 2566:
#line 16422 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 26436 "parser.c" /* yacc.c:1646  */
    break;

  case 2567:
#line 16426 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_ppointer ((yyvsp[0]));
  }
#line 26444 "parser.c" /* yacc.c:1646  */
    break;

  case 2568:
#line 16430 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_address ((yyvsp[0]));
  }
#line 26452 "parser.c" /* yacc.c:1646  */
    break;

  case 2569:
#line 16434 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("EXTFH address");
  }
#line 26460 "parser.c" /* yacc.c:1646  */
    break;

  case 2570:
#line 16438 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("EXTFH address");
  }
#line 26468 "parser.c" /* yacc.c:1646  */
    break;

  case 2571:
#line 16442 "parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	cb_tree		switch_id;

	x = cb_ref ((yyvsp[0]));
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME (x)->category != CB_SWITCH_NAME) {
			cb_error_x ((yyvsp[0]), _("invalid mnemonic identifier"));
			(yyval) = cb_error_node;
		} else {
			switch_id = cb_int (CB_SYSTEM_NAME (x)->token);
			(yyval) = CB_BUILD_FUNCALL_1 ("cob_switch_value", switch_id);
		}
	} else {
		(yyval) = cb_error_node;
	}
  }
#line 26490 "parser.c" /* yacc.c:1646  */
    break;

  case 2572:
#line 16463 "parser.y" /* yacc.c:1646  */
    {
	/* FIXME: check with "lookup_register ("LENGTH OF") != NULL"
	          if we actually want to do this,
	          otherwise raise an error "not defined in this dialect"
	*/
  }
#line 26501 "parser.c" /* yacc.c:1646  */
    break;

  case 2573:
#line 16472 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 26509 "parser.c" /* yacc.c:1646  */
    break;

  case 2574:
#line 16476 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 26517 "parser.c" /* yacc.c:1646  */
    break;

  case 2582:
#line 16493 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 26525 "parser.c" /* yacc.c:1646  */
    break;

  case 2583:
#line 16497 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 26533 "parser.c" /* yacc.c:1646  */
    break;

  case 2584:
#line 16501 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 26541 "parser.c" /* yacc.c:1646  */
    break;

  case 2588:
#line 16511 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 26549 "parser.c" /* yacc.c:1646  */
    break;

  case 2589:
#line 16515 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 26557 "parser.c" /* yacc.c:1646  */
    break;

  case 2590:
#line 16519 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 26565 "parser.c" /* yacc.c:1646  */
    break;

  case 2591:
#line 16526 "parser.y" /* yacc.c:1646  */
    {
	if (CB_TREE_CATEGORY ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error_x ((yyvsp[0]), _("a numeric literal is expected here"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 26578 "parser.c" /* yacc.c:1646  */
    break;

  case 2592:
#line 16538 "parser.y" /* yacc.c:1646  */
    {
	if (CB_TREE_CATEGORY ((yyvsp[0])) == CB_CATEGORY_NUMERIC) {
		cb_error_x ((yyvsp[0]), _("a non-numeric literal is expected here"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 26591 "parser.c" /* yacc.c:1646  */
    break;

  case 2593:
#line 16550 "parser.y" /* yacc.c:1646  */
    {
	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC
	 || cb_get_int ((yyvsp[0])) == 0) {
		cb_error (_("non-zero value expected"));
		(yyval) = cb_int1;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 26605 "parser.c" /* yacc.c:1646  */
    break;

  case 2598:
#line 16574 "parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 26613 "parser.c" /* yacc.c:1646  */
    break;

  case 2599:
#line 16581 "parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 26621 "parser.c" /* yacc.c:1646  */
    break;

  case 2601:
#line 16589 "parser.y" /* yacc.c:1646  */
    {
	  error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 26629 "parser.c" /* yacc.c:1646  */
    break;

  case 2603:
#line 16597 "parser.y" /* yacc.c:1646  */
    {
	  error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 26637 "parser.c" /* yacc.c:1646  */
    break;

  case 2609:
#line 16615 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = check_not_88_level ((yyvsp[0]));
  }
#line 26645 "parser.c" /* yacc.c:1646  */
    break;

  case 2611:
#line 16623 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = check_not_88_level ((yyvsp[0]));
  }
#line 26653 "parser.c" /* yacc.c:1646  */
    break;

  case 2614:
#line 16632 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = check_not_88_level ((yyvsp[0]));
  }
#line 26661 "parser.c" /* yacc.c:1646  */
    break;

  case 2617:
#line 16641 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = check_not_88_level ((yyvsp[0]));
  }
#line 26669 "parser.c" /* yacc.c:1646  */
    break;

  case 2619:
#line 16646 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_zero;
  }
#line 26677 "parser.c" /* yacc.c:1646  */
    break;

  case 2620:
#line 16655 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = check_not_88_level ((yyvsp[0]));
  }
#line 26685 "parser.c" /* yacc.c:1646  */
    break;

  case 2624:
#line 16671 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = check_not_88_level ((yyvsp[0]));
  }
#line 26693 "parser.c" /* yacc.c:1646  */
    break;

  case 2626:
#line 16679 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = check_not_88_level ((yyvsp[0]));
  }
#line 26701 "parser.c" /* yacc.c:1646  */
    break;

  case 2629:
#line 16689 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_identifier ((yyvsp[0]), 0); }
#line 26707 "parser.c" /* yacc.c:1646  */
    break;

  case 2630:
#line 16693 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_identifier ((yyvsp[0]), 1); }
#line 26713 "parser.c" /* yacc.c:1646  */
    break;

  case 2631:
#line 16697 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 26719 "parser.c" /* yacc.c:1646  */
    break;

  case 2632:
#line 16698 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 26725 "parser.c" /* yacc.c:1646  */
    break;

  case 2633:
#line 16703 "parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 26733 "parser.c" /* yacc.c:1646  */
    break;

  case 2634:
#line 16710 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) != cb_error_node
	    && cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error_x ((yyvsp[0]), _("'%s' is not numeric"), cb_name ((yyvsp[0])));
	}
  }
#line 26744 "parser.c" /* yacc.c:1646  */
    break;

  case 2635:
#line 16720 "parser.y" /* yacc.c:1646  */
    {
	cb_tree x = NULL;
	if (CB_REFERENCE_P ((yyvsp[0]))) {
		x = cb_ref ((yyvsp[0]));
	}
	if (x && (CB_FIELD_P (x) || CB_FILE_P (x))) {
		(yyval) = cb_build_identifier ((yyvsp[0]), 0);
	} else {
		if (x != cb_error_node) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a field or file"), cb_name ((yyvsp[0])));
		}
		(yyval) = cb_error_node;
	}
  }
#line 26763 "parser.c" /* yacc.c:1646  */
    break;

  case 2636:
#line 16738 "parser.y" /* yacc.c:1646  */
    {
	cb_tree x = NULL;
	if (CB_REFERENCE_P ((yyvsp[0]))) {
		x = cb_ref ((yyvsp[0]));
	}
	if (x && CB_FIELD_P (x)) {
		(yyval) = cb_build_identifier ((yyvsp[0]), 0);
	} else {
		if (x != cb_error_node) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a field"), cb_name ((yyvsp[0])));
		}
		(yyval) = cb_error_node;
	}
  }
#line 26782 "parser.c" /* yacc.c:1646  */
    break;

  case 2637:
#line 16756 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-2]));
	}
  }
#line 26793 "parser.c" /* yacc.c:1646  */
    break;

  case 2638:
#line 16763 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 26804 "parser.c" /* yacc.c:1646  */
    break;

  case 2639:
#line 16770 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 26815 "parser.c" /* yacc.c:1646  */
    break;

  case 2640:
#line 16777 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[0]));
	}
  }
#line 26826 "parser.c" /* yacc.c:1646  */
    break;

  case 2641:
#line 16787 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 26834 "parser.c" /* yacc.c:1646  */
    break;

  case 2642:
#line 16791 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 26842 "parser.c" /* yacc.c:1646  */
    break;

  case 2643:
#line 16798 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_identifier ((yyvsp[0]), 0);
  }
#line 26850 "parser.c" /* yacc.c:1646  */
    break;

  case 2644:
#line 16802 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_identifier ((yyvsp[0]), 0);
  }
#line 26858 "parser.c" /* yacc.c:1646  */
    break;

  case 2645:
#line 16809 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	if (CB_REFERENCE_P ((yyvsp[-2]))) {
		CB_REFERENCE ((yyvsp[-2]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-2]));
	}
  }
#line 26872 "parser.c" /* yacc.c:1646  */
    break;

  case 2646:
#line 16819 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (CB_REFERENCE_P ((yyvsp[-1]))) {
		CB_REFERENCE ((yyvsp[-1]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 26886 "parser.c" /* yacc.c:1646  */
    break;

  case 2647:
#line 16829 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (CB_REFERENCE_P ((yyvsp[-1]))) {
		CB_REFERENCE ((yyvsp[-1]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 26900 "parser.c" /* yacc.c:1646  */
    break;

  case 2648:
#line 16839 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	if (CB_REFERENCE_P ((yyvsp[0]))) {
		CB_REFERENCE ((yyvsp[0]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[0]));
	}
  }
#line 26914 "parser.c" /* yacc.c:1646  */
    break;

  case 2649:
#line 16852 "parser.y" /* yacc.c:1646  */
    {
	cb_tree x = NULL;
	(yyval) = (yyvsp[0]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[0]));
	}
	if (CB_REFERENCE_P ((yyvsp[0]))) {
		x = cb_ref ((yyvsp[0]));
	}
	if (x && CB_FIELD_P (x)) {
		(yyval) = cb_build_identifier ((yyvsp[0]), 0);
		error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
	} else if (x && CB_ALPHABET_NAME_P (x)) {
		/* TODO: add check for subscript/ ref-mod here [not allowed] */
		(yyval) = cb_build_identifier ((yyvsp[0]), 0);
	} else {
		if (x != cb_error_node) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a field or alphabet"), cb_name ((yyvsp[0])));
		}
		(yyval) = cb_error_node;
	}
  }
#line 26941 "parser.c" /* yacc.c:1646  */
    break;

  case 2650:
#line 16878 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 26949 "parser.c" /* yacc.c:1646  */
    break;

  case 2651:
#line 16882 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	CB_REFERENCE ((yyvsp[-2]))->chain = (yyvsp[0]);
  }
#line 26958 "parser.c" /* yacc.c:1646  */
    break;

  case 2652:
#line 16889 "parser.y" /* yacc.c:1646  */
    {
	start_tree = NULL;	// actually not needed - initialized for clarity only
  }
#line 26966 "parser.c" /* yacc.c:1646  */
    break;

  case 2653:
#line 16893 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == cb_error_node) {
		cb_error_x (start_tree, _("a subscripted data-item cannot be used here"));
	}
	(yyval) = start_tree;
  }
#line 26977 "parser.c" /* yacc.c:1646  */
    break;

  case 2654:
#line 16903 "parser.y" /* yacc.c:1646  */
    {
	start_tree = (yyvsp[0]);
	(yyval) = (yyvsp[0]);
  }
#line 26986 "parser.c" /* yacc.c:1646  */
    break;

  case 2655:
#line 16908 "parser.y" /* yacc.c:1646  */
    {
	start_tree = (yyvsp[-2]);
	(yyval) = cb_error_node;
  }
#line 26995 "parser.c" /* yacc.c:1646  */
    break;

  case 2656:
#line 16916 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-3]);
	CB_REFERENCE ((yyvsp[-3]))->subs = cb_list_reverse ((yyvsp[-1]));
  }
#line 27004 "parser.c" /* yacc.c:1646  */
    break;

  case 2657:
#line 16924 "parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-4]))->offset = (yyvsp[-2]);
  }
#line 27012 "parser.c" /* yacc.c:1646  */
    break;

  case 2658:
#line 16928 "parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-5]))->offset = (yyvsp[-3]);
	CB_REFERENCE ((yyvsp[-5]))->length = (yyvsp[-1]);
  }
#line 27021 "parser.c" /* yacc.c:1646  */
    break;

  case 2659:
#line 16938 "parser.y" /* yacc.c:1646  */
    {
	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC
	    || !CB_LITERAL_P((yyvsp[0]))
	    || CB_LITERAL ((yyvsp[0]))->sign < 0
	    || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("non-negative integer value expected"));
		(yyval) = cb_build_numeric_literal(-1, "1", 0);
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 27037 "parser.c" /* yacc.c:1646  */
    break;

  case 2660:
#line 16953 "parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error (_("integer value expected"));
		(yyval) = cb_int1;
	} else if (CB_LITERAL_P ((yyvsp[0]))
		&& (CB_LITERAL ((yyvsp[0]))->sign || CB_LITERAL ((yyvsp[0]))->scale)) {
		cb_error (_("integer value expected"));
		(yyval) = cb_int1;
	} else {
		n = cb_get_int ((yyvsp[0]));
		if (n < 1 || n > 256) {
			cb_error (_("invalid symbolic integer"));
			(yyval) = cb_int1;
		} else {
			(yyval) = (yyvsp[0]);
		}
	}
  }
#line 27062 "parser.c" /* yacc.c:1646  */
    break;

  case 2661:
#line 16977 "parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC
	 || !CB_LITERAL_P((yyvsp[0]))
	 || CB_LITERAL ((yyvsp[0]))->sign
	 || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("unsigned positive integer value expected"));
		(yyval) = cb_int1;	} else {
		n = cb_get_int ((yyvsp[0]));
		if (n < 1) {
			cb_error (_("unsigned positive integer value expected"));
			(yyval) = cb_int1;
		} else {
			(yyval) = (yyvsp[0]);
		}
	}
  }
#line 27085 "parser.c" /* yacc.c:1646  */
    break;

  case 2662:
#line 16999 "parser.y" /* yacc.c:1646  */
    {
	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error (_("Integer value expected"));
		(yyval) = cb_int1;
	} else if (!CB_LITERAL_P((yyvsp[0])) ||
	    CB_LITERAL ((yyvsp[0]))->sign < 0 || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("positive integer value expected"));
		(yyval) = cb_int1;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 27102 "parser.c" /* yacc.c:1646  */
    break;

  case 2663:
#line 17012 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 27110 "parser.c" /* yacc.c:1646  */
    break;

  case 2664:
#line 17019 "parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) == CB_CATEGORY_NUMERIC) {
		if (CB_LITERAL ((yyvsp[0]))->sign || CB_LITERAL ((yyvsp[0]))->scale) {
			cb_error (_("integer value expected"));
		} else {
			n = cb_get_int ((yyvsp[0]));
			if (n < 1 || n > 256) {
				cb_error (_("invalid CLASS value"));
			}
		}
	}
	(yyval) = (yyvsp[0]);
  }
#line 27130 "parser.c" /* yacc.c:1646  */
    break;

  case 2665:
#line 17034 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 27136 "parser.c" /* yacc.c:1646  */
    break;

  case 2666:
#line 17035 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 27142 "parser.c" /* yacc.c:1646  */
    break;

  case 2667:
#line 17036 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 27148 "parser.c" /* yacc.c:1646  */
    break;

  case 2668:
#line 17037 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_high; }
#line 27154 "parser.c" /* yacc.c:1646  */
    break;

  case 2669:
#line 17038 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_low; }
#line 27160 "parser.c" /* yacc.c:1646  */
    break;

  case 2670:
#line 17039 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 27166 "parser.c" /* yacc.c:1646  */
    break;

  case 2671:
#line 17044 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 27174 "parser.c" /* yacc.c:1646  */
    break;

  case 2672:
#line 17048 "parser.y" /* yacc.c:1646  */
    {
	struct cb_literal	*l;

	if (CB_LITERAL_P ((yyvsp[0]))) {
		/* We must not alter the original definition */
		l = cobc_parse_malloc (sizeof(struct cb_literal));
		*l = *(CB_LITERAL((yyvsp[0])));
		l->all = 1;
		(yyval) = CB_TREE (l);
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 27192 "parser.c" /* yacc.c:1646  */
    break;

  case 2673:
#line 17065 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 27200 "parser.c" /* yacc.c:1646  */
    break;

  case 2674:
#line 17069 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_concat_literals ((yyvsp[-2]), (yyvsp[0]));
  }
#line 27208 "parser.c" /* yacc.c:1646  */
    break;

  case 2675:
#line 17075 "parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 27214 "parser.c" /* yacc.c:1646  */
    break;

  case 2676:
#line 17076 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 27220 "parser.c" /* yacc.c:1646  */
    break;

  case 2677:
#line 17077 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 27226 "parser.c" /* yacc.c:1646  */
    break;

  case 2678:
#line 17078 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 27232 "parser.c" /* yacc.c:1646  */
    break;

  case 2679:
#line 17079 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_high; }
#line 27238 "parser.c" /* yacc.c:1646  */
    break;

  case 2680:
#line 17080 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_low; }
#line 27244 "parser.c" /* yacc.c:1646  */
    break;

  case 2681:
#line 17081 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 27250 "parser.c" /* yacc.c:1646  */
    break;

  case 2682:
#line 17085 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 27256 "parser.c" /* yacc.c:1646  */
    break;

  case 2683:
#line 17086 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 27262 "parser.c" /* yacc.c:1646  */
    break;

  case 2684:
#line 17087 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_high; }
#line 27268 "parser.c" /* yacc.c:1646  */
    break;

  case 2685:
#line 17088 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_low; }
#line 27274 "parser.c" /* yacc.c:1646  */
    break;

  case 2686:
#line 17095 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), NULL, (yyvsp[0]), 0);
  }
#line 27282 "parser.c" /* yacc.c:1646  */
    break;

  case 2687:
#line 17099 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), CB_LIST_INIT ((yyvsp[-2])), (yyvsp[0]), 0);
  }
#line 27290 "parser.c" /* yacc.c:1646  */
    break;

  case 2688:
#line 17103 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 27298 "parser.c" /* yacc.c:1646  */
    break;

  case 2689:
#line 17107 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 27306 "parser.c" /* yacc.c:1646  */
    break;

  case 2690:
#line 17111 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-3]), (yyvsp[-1]), NULL, 0);
  }
#line 27314 "parser.c" /* yacc.c:1646  */
    break;

  case 2691:
#line 17115 "parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("PHYSICAL argument for LENGTH functions"));
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), NULL, 0);
  }
#line 27323 "parser.c" /* yacc.c:1646  */
    break;

  case 2692:
#line 17120 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-3]), (yyvsp[-1]), NULL, 0);
  }
#line 27331 "parser.c" /* yacc.c:1646  */
    break;

  case 2693:
#line 17124 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 27339 "parser.c" /* yacc.c:1646  */
    break;

  case 2694:
#line 17128 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 27347 "parser.c" /* yacc.c:1646  */
    break;

  case 2695:
#line 17132 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 27355 "parser.c" /* yacc.c:1646  */
    break;

  case 2696:
#line 17136 "parser.y" /* yacc.c:1646  */
    {
	  (yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 27363 "parser.c" /* yacc.c:1646  */
    break;

  case 2697:
#line 17140 "parser.y" /* yacc.c:1646  */
    {
	  (yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 27371 "parser.c" /* yacc.c:1646  */
    break;

  case 2698:
#line 17144 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), (yyvsp[0]), NULL, 0);
  }
#line 27379 "parser.c" /* yacc.c:1646  */
    break;

  case 2699:
#line 17148 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), (yyvsp[0]), NULL, 1);
  }
#line 27387 "parser.c" /* yacc.c:1646  */
    break;

  case 2711:
#line 17175 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 27395 "parser.c" /* yacc.c:1646  */
    break;

  case 2712:
#line 17179 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), NULL);
  }
#line 27403 "parser.c" /* yacc.c:1646  */
    break;

  case 2713:
#line 17183 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 27411 "parser.c" /* yacc.c:1646  */
    break;

  case 2714:
#line 17190 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 27419 "parser.c" /* yacc.c:1646  */
    break;

  case 2715:
#line 17194 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 27427 "parser.c" /* yacc.c:1646  */
    break;

  case 2716:
#line 17198 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 27435 "parser.c" /* yacc.c:1646  */
    break;

  case 2717:
#line 17205 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_int0);
  }
#line 27446 "parser.c" /* yacc.c:1646  */
    break;

  case 2718:
#line 17212 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_int1);
  }
#line 27457 "parser.c" /* yacc.c:1646  */
    break;

  case 2719:
#line 17219 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_int2);
  }
#line 27468 "parser.c" /* yacc.c:1646  */
    break;

  case 2720:
#line 17228 "parser.y" /* yacc.c:1646  */
    {
	suppress_data_exceptions = 1;
  }
#line 27476 "parser.c" /* yacc.c:1646  */
    break;

  case 2721:
#line 17232 "parser.y" /* yacc.c:1646  */
    {
	suppress_data_exceptions = 0;
	if (CB_NUMERIC_LITERAL_P((yyvsp[0]))) {
		cb_error_x ((yyvsp[0]), _("a non-numeric literal is expected here"));
		(yyval) = CB_LIST_INIT (cb_error_node);
	} else {
		(yyval) = CB_LIST_INIT ((yyvsp[0]));
	}
  }
#line 27490 "parser.c" /* yacc.c:1646  */
    break;

  case 2722:
#line 17245 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_null);
  }
#line 27501 "parser.c" /* yacc.c:1646  */
    break;

  case 2723:
#line 17252 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, (yyvsp[0]));
  }
#line 27512 "parser.c" /* yacc.c:1646  */
    break;

  case 2724:
#line 17262 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_null);
  }
#line 27523 "parser.c" /* yacc.c:1646  */
    break;

  case 2725:
#line 17269 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_ref ((yyvsp[0])));
  }
#line 27534 "parser.c" /* yacc.c:1646  */
    break;

  case 2726:
#line 17279 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), cb_int0);
  }
#line 27542 "parser.c" /* yacc.c:1646  */
    break;

  case 2727:
#line 17283 "parser.y" /* yacc.c:1646  */
    {
	const int	num_args = cb_list_length ((yyvsp[-2]));

	if (num_args == 4) {
		cb_error_x ((yyvsp[-2]), _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	(yyval) = cb_list_add ((yyvsp[-2]), cb_int1);
  }
#line 27556 "parser.c" /* yacc.c:1646  */
    break;

  case 2728:
#line 17296 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), cb_int0);
  }
#line 27564 "parser.c" /* yacc.c:1646  */
    break;

  case 2729:
#line 17300 "parser.y" /* yacc.c:1646  */
    {
	const int	num_args = cb_list_length ((yyvsp[-2]));

	if (num_args == 3) {
		cb_error_x ((yyvsp[-2]), _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	(yyval) = cb_list_add ((yyvsp[-2]), cb_int1);
  }
#line 27578 "parser.c" /* yacc.c:1646  */
    break;

  case 2730:
#line 17314 "parser.y" /* yacc.c:1646  */
    {
	non_const_word = 1;
  }
#line 27586 "parser.c" /* yacc.c:1646  */
    break;

  case 2731:
#line 17322 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 27592 "parser.c" /* yacc.c:1646  */
    break;

  case 2732:
#line 17323 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 27598 "parser.c" /* yacc.c:1646  */
    break;

  case 2733:
#line 17327 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 27604 "parser.c" /* yacc.c:1646  */
    break;

  case 2734:
#line 17328 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 27610 "parser.c" /* yacc.c:1646  */
    break;

  case 2735:
#line 17329 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 27616 "parser.c" /* yacc.c:1646  */
    break;

  case 2736:
#line 17333 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 27622 "parser.c" /* yacc.c:1646  */
    break;

  case 2737:
#line 17334 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 27628 "parser.c" /* yacc.c:1646  */
    break;

  case 2738:
#line 17339 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 27636 "parser.c" /* yacc.c:1646  */
    break;

  case 2739:
#line 17343 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 27644 "parser.c" /* yacc.c:1646  */
    break;

  case 2740:
#line 17350 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 27652 "parser.c" /* yacc.c:1646  */
    break;

  case 2741:
#line 17354 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 27660 "parser.c" /* yacc.c:1646  */
    break;

  case 2742:
#line 17361 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 27666 "parser.c" /* yacc.c:1646  */
    break;

  case 2743:
#line 17362 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 27672 "parser.c" /* yacc.c:1646  */
    break;

  case 2744:
#line 17363 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 27678 "parser.c" /* yacc.c:1646  */
    break;

  case 2745:
#line 17367 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 27684 "parser.c" /* yacc.c:1646  */
    break;

  case 2746:
#line 17368 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 27690 "parser.c" /* yacc.c:1646  */
    break;

  case 2747:
#line 17372 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (cb_flag_optional_file); }
#line 27696 "parser.c" /* yacc.c:1646  */
    break;

  case 2748:
#line 17373 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 27702 "parser.c" /* yacc.c:1646  */
    break;

  case 2749:
#line 17374 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 27708 "parser.c" /* yacc.c:1646  */
    break;

  case 2750:
#line 17379 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 27716 "parser.c" /* yacc.c:1646  */
    break;

  case 2751:
#line 17383 "parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = default_rounded_mode;
	}
	cobc_cs_check = 0;
  }
#line 27729 "parser.c" /* yacc.c:1646  */
    break;

  case 2752:
#line 17395 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
	cobc_cs_check = 0;
  }
#line 27738 "parser.c" /* yacc.c:1646  */
    break;

  case 2753:
#line 17400 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	cobc_cs_check = 0;
  }
#line 27747 "parser.c" /* yacc.c:1646  */
    break;

  case 2754:
#line 17408 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_AWAY_FROM_ZERO);
  }
#line 27755 "parser.c" /* yacc.c:1646  */
    break;

  case 2755:
#line 17412 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_AWAY_FROM_ZERO);
  }
#line 27763 "parser.c" /* yacc.c:1646  */
    break;

  case 2756:
#line 17416 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_EVEN);
  }
#line 27771 "parser.c" /* yacc.c:1646  */
    break;

  case 2757:
#line 17420 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_TOWARD_ZERO);
  }
#line 27779 "parser.c" /* yacc.c:1646  */
    break;

  case 2758:
#line 17424 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_PROHIBITED);
  }
#line 27787 "parser.c" /* yacc.c:1646  */
    break;

  case 2759:
#line 17428 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_GREATER);
  }
#line 27795 "parser.c" /* yacc.c:1646  */
    break;

  case 2760:
#line 17432 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_LESSER);
  }
#line 27803 "parser.c" /* yacc.c:1646  */
    break;

  case 2761:
#line 17436 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TRUNCATION);
  }
#line 27811 "parser.c" /* yacc.c:1646  */
    break;

  case 2762:
#line 17442 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 27817 "parser.c" /* yacc.c:1646  */
    break;

  case 2763:
#line 17443 "parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 27823 "parser.c" /* yacc.c:1646  */
    break;

  case 2764:
#line 17447 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 27829 "parser.c" /* yacc.c:1646  */
    break;

  case 2765:
#line 17449 "parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-3]));
	(yyval) = cb_list_add (x, (yyvsp[-1]));
  }
#line 27840 "parser.c" /* yacc.c:1646  */
    break;

  case 2766:
#line 17458 "parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 27846 "parser.c" /* yacc.c:1646  */
    break;

  case 2767:
#line 17460 "parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 27854 "parser.c" /* yacc.c:1646  */
    break;

  case 2768:
#line 17469 "parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 1;
  }
#line 27862 "parser.c" /* yacc.c:1646  */
    break;

  case 2769:
#line 17473 "parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 1;
  }
#line 27870 "parser.c" /* yacc.c:1646  */
    break;

  case 2770:
#line 17477 "parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 0;
  }
#line 27878 "parser.c" /* yacc.c:1646  */
    break;

  case 2771:
#line 17481 "parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 0;
  }
#line 27886 "parser.c" /* yacc.c:1646  */
    break;


#line 27890 "parser.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 17673 "parser.y" /* yacc.c:1906  */

