/*
   Copyright (C) 2002-2012, 2014-2019 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman

   This file is part of GnuCOBOL.

   The GnuCOBOL runtime library is free software: you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.
*/


#include "config.h"

#define _LFS64_LARGEFILE		1
#define _LFS64_STDIO			1
#define _FILE_OFFSET_BITS		64
#define _LARGEFILE64_SOURCE		1
#ifdef	_AIX
#define _LARGE_FILES			1
#endif	/* _AIX */
#if defined(__hpux__) && !defined(__LP64__)
#define _APP32_64BIT_OFF_T		1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef	HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef	HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef	_WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <direct.h>
#include <io.h>
#if defined (__WATCOMC__) || defined (__ORANGEC__)
#define	fdcobsync	fsync
#else
#define	fdcobsync	_commit
#endif
#if !defined(__BORLANDC__) && !defined(__WATCOMC__) && !defined(__ORANGEC__)
#define	getcwd		_getcwd
#define	chdir		_chdir
#define	mkdir		_mkdir
#define	rmdir		_rmdir
#define	open		_open
#define	close		_close
#define	unlink		_unlink
#define	fdopen		_fdopen
#ifndef lseek
#define lseek		_lseeki64
#endif
#endif

#ifndef off_t
#define off_t		cob_s64_t
#endif


#ifndef	_O_TEMPORARY
#define	_O_TEMPORARY	0
#endif

#else	/* _WIN32 */

#if	defined(HAVE_FDATASYNC)
#define	fdcobsync	fdatasync
#else
#define	fdcobsync	fsync
#endif

#ifndef	O_BINARY
#define	O_BINARY	0
#endif

#endif	/* _WIN32 */

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "libcob.h"
#include "coblocal.h"

#if !defined (EDEADLK) && defined (EDEADLOCK)
#define EDEADLK EDEADLOCK
#endif

#ifdef	WITH_DB

#include <db.h>

#elif	defined(WITH_CISAM) || defined(WITH_DISAM) || defined(WITH_VBISAM)

#define	WITH_ANY_ISAM
#include <signal.h>

#define	COB_WITH_STATUS_02

#ifdef	WITH_CISAM
#include <isam.h>
#define	isfullclose(x)	isclose (x)
#define ISRECNUM isrecnum
#define ISERRNO  iserrno
#define ISRECLEN isreclen
#endif

#ifdef	WITH_DISAM
#ifndef DISAM_NO_ISCONFIG
#include <isconfig.h>
#ifndef ISCOBOL_STATS
#undef	COB_WITH_STATUS_02
#endif
#endif
#include <disam.h>
#define	isfullclose(x)	isclose (x)
#define ISRECNUM isrecnum
#define ISERRNO  iserrno
#define ISRECLEN isreclen
#endif

#ifdef	WITH_VBISAM
#include <vbisam.h>
/* VB-ISAM does not set dup key status */
#undef	COB_WITH_STATUS_02
#if defined(VB_RTD)
/* Since VBISAM 2.1.1: access to isrecnum iserrno etc is no longer global */
static	vb_rtd_t *vbisam_rtd = NULL;

#define ISRECNUM vbisam_rtd->isrecnum
#define ISERRNO  vbisam_rtd->iserrno
#define ISRECLEN vbisam_rtd->isreclen
#else
#define ISRECNUM isrecnum
#define ISERRNO  iserrno
#define ISRECLEN isreclen
#endif
#endif

/* Isam File handler packet */

struct indexfile {
	char		*filename;	/* ISAM data file name */
	char		*savekey;	/* Area to save last primary key read */
	char		*recwrk;	/* Record work/save area */
	size_t		nkeys;		/* Actual keys in file */
	int		isfd;		/* ISAM file number */
	int		recnum;		/* Last record number read */
	int		saverecnum;	/* isrecnum of next record to process */
	int		saveerrno;	/* savefileposition errno */
	int		lmode;		/* File lock mode for 'isread' */
	int		curkey;		/* Current active index */
	int		startcond;	/* Previous 'start' condition value */
	int		readdir;	/* Read direction: ISPREV or ISNEXT */
	int		lenkey;		/* Length of savekey area */
	int		eofpending;	/* End of file pending */
	int		readdone;	/* A 'read' has been successfully done */
	int		startiscur;	/* The 'start' record is current */
	int		wrkhasrec;	/* 'recwrk' holds the next|prev record */
	struct keydesc	key[1];		/* Table of key information */
					/* keydesc is defined in (d|c|vb)isam.h */
};

/* Return total length of the key */
static int
indexed_keylen (struct indexfile *fh, int idx)
{
	int totlen, part;
	totlen = 0;
	for (part = 0; part < fh->key[idx].k_nparts; part++) {
		totlen += fh->key[idx].k_part[part].kp_leng;
	}
	return totlen;
}

/* Save key for given index into 'savekey'
   Return total length of the key */
static int
indexed_savekey (struct indexfile *fh, unsigned char *data, int idx)
{
	int totlen, part;
	totlen = 0;
	if (data == NULL) {
		data = (unsigned char*)fh->recwrk;
	}
	for (part = 0; part < fh->key[idx].k_nparts; part++) {
		memcpy (fh->savekey + totlen,
			data  + fh->key[idx].k_part[part].kp_start,
			fh->key[idx].k_part[part].kp_leng);
		totlen += fh->key[idx].k_part[part].kp_leng;
	}
	return totlen;
}

/* Copy key for given index from 'savekey' back to recwrk
   Return total length of the key */
static int
indexed_restorekey (struct indexfile *fh, unsigned char *data, int idx)
{
	int totlen, part;
	totlen = 0;
	if (data == NULL) {
		data = (unsigned char*)fh->recwrk;
	}
	for (part = 0; part < fh->key[idx].k_nparts; part++) {
		memcpy (data  + fh->key[idx].k_part[part].kp_start,
			fh->savekey + totlen,
			fh->key[idx].k_part[part].kp_leng);
		totlen += fh->key[idx].k_part[part].kp_leng;
	}
	return totlen;
}

/* Compare key for given index 'savekey' to recwrk
   Return compare status */
static int
indexed_cmpkey (struct indexfile *fh, unsigned char *data, int idx, int partlen)
{
	int sts, part, totlen,cl;
	totlen = sts = 0;
	if (partlen <= 0) {
		partlen = indexed_keylen(fh, idx);
	}
	for (part = 0; part < fh->key[idx].k_nparts && partlen > 0; part++) {
		cl = partlen > fh->key[idx].k_part[part].kp_leng ? fh->key[idx].k_part[part].kp_leng : partlen;
		sts = memcmp(	data  + fh->key[idx].k_part[part].kp_start,
				fh->savekey + totlen,
				cl);
		if (sts != 0) {
			return sts;
		}
		totlen += fh->key[idx].k_part[part].kp_leng;
		partlen -= fh->key[idx].k_part[part].kp_leng;
	}
	return sts;
}

/* Build 'keydesc' from 'cob_file_key'
   Return total length of the key */
static int
indexed_keydesc (cob_file *f, struct keydesc *kd, cob_file_key *key)
{
	int	keylen,part;
	memset (kd,0,sizeof (struct keydesc));
	kd->k_flags = key->tf_duplicates ? ISDUPS : ISNODUPS;
	if (key->count_components <= 1) {
		kd->k_nparts = 1;		/* Single field key */
		kd->k_start = key->offset;
		kd->k_leng = key->field->size;
		kd->k_type = CHARTYPE;
#ifdef NULLKEY
		if (key->tf_suppress) {
			kd->k_flags |= NULLKEY;
			kd->k_type = CHARTYPE | (key->char_suppress << 8);
		}
#endif
		keylen = kd->k_leng;
	} else {
		keylen = 0;
		for (part=0; part < key->count_components && part < COB_MAX_KEYCOMP; part++) {
			kd->k_part[part].kp_start = key->component[part]->data - f->record->data;
			kd->k_part[part].kp_leng = key->component[part]->size;
			keylen += kd->k_part[part].kp_leng;
			kd->k_part[part].kp_type = CHARTYPE;
#ifdef NULLKEY
			if (key->tf_suppress) {
				kd->k_flags |= NULLKEY;
				kd->k_part[part].kp_type = CHARTYPE | (key->char_suppress << 8);
			}
#endif
		}
		kd->k_nparts = part;
	}
#if defined(WITH_DISAM) || defined(WITH_VBISAM)
	kd->k_len = keylen;		/* Total length of this key */
#endif
	return keylen;
}

/* Compare 'keydesc' to 'keydesc'
   Return 0 if equal, else 1 */
static int
indexed_keycmp (struct keydesc *k1, struct keydesc *k2)
{
	int	part;
	if (k1->k_flags != k2->k_flags) {
		return 1;
	}
	if (k1->k_nparts != k2->k_nparts) {
		return 1;
	}
	for (part=0; part < k1->k_nparts; part++) {
		if (k1->k_part[part].kp_start != k2->k_part[part].kp_start) {
			return 1;
		}
		if (k1->k_part[part].kp_leng != k2->k_part[part].kp_leng) {
			return 1;
		}
		if (k1->k_part[part].kp_type != k2->k_part[part].kp_type) {
			return 1;
		}
	}
	return 0;
}

/* Return index number for given key */
static int
indexed_findkey (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen)
{
	int 	k,part;
	*fullkeylen = *partlen = 0;
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].field
		&&  f->keys[k].count_components <= 1
		&&  f->keys[k].field->data == kf->data) {
			*fullkeylen = f->keys[k].field->size;
			*partlen = kf->size;
			return k;
		}
	}
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].count_components > 1) {
			if ((f->keys[k].field
			&&  f->keys[k].field->data == kf->data
			&&  f->keys[k].field->size == kf->size)
			||  (f->keys[k].component[0]->data == kf->data)) {
				for (part=0; part < f->keys[k].count_components; part++) {
					*fullkeylen += f->keys[k].component[part]->size;
				}
				if (f->keys[k].field && f->keys[k].field->data == kf->data) {
					*partlen = kf->size;
				} else {
					*partlen = *fullkeylen;
				}
				return k;
			}
		}
	}
	return -1;
}

#endif

struct file_list {
	struct file_list	*next;
	cob_file		*file;
};

#ifdef	WORDS_BIGENDIAN
#define	COB_MAYSWAP_16(x)	((unsigned short)(x))
#define	COB_MAYSWAP_32(x)	((unsigned int)(x))
#else
#define	COB_MAYSWAP_16(x)	(COB_BSWAP_16((unsigned short)(x)))
#define	COB_MAYSWAP_32(x)	(COB_BSWAP_32((unsigned int)(x)))
#endif

/* SORT definitions */

#define COBSORTEND		1
#define COBSORTABORT		2
#define COBSORTFILEERR		3
#define COBSORTNOTOPEN		4


/* Sort item */
struct cobitem {
	struct cobitem		*next;
	unsigned char		end_of_block;
	unsigned char		block_byte;
	unsigned char		unique[sizeof (size_t)];
	unsigned char		item[1];
};

/* Sort memory chunk */
struct sort_mem_struct {
	struct sort_mem_struct	*next;
	unsigned char		*mem_ptr;
};

/* Sort queue structure */
struct queue_struct {
	struct cobitem		*first;
	struct cobitem		*last;
	size_t			count;
};

/* Sort temporary file structure */
struct file_struct {
	FILE			*fp;
	size_t			count;	/* Count of blocks in temporary files */
};

/* Sort base structure */
struct cobsort {
	void			*pointer;
	struct cobitem		*empty;
	void			*sort_return;
	cob_field		*fnstatus;
	struct sort_mem_struct	*mem_base;
	size_t			unique;
	size_t			size;
	size_t			alloc_size;
	size_t			mem_size;
	size_t			mem_used;
	size_t			mem_total;
	size_t			chunk_size;
	size_t			r_size;
	size_t			w_size;
	size_t			switch_to_file;
	unsigned int		retrieving;
	unsigned int		files_used;
	int			destination_file;
	int			retrieval_queue;
	struct queue_struct	queue[4];
	struct file_struct	file[4];
};

/* End SORT definitions */


/* Local variables */

static cob_global	*cobglobptr = NULL;
static cob_settings	*cobsetptr = NULL;

static unsigned int	eop_status = 0;
static unsigned int	check_eop_status = 0;

static struct file_list	*file_cache = NULL;

static char		*file_open_env = NULL;
static char		*file_open_name = NULL;
static char		*file_open_buff = NULL;
static unsigned char* file_open_io_env = NULL;	/* IO_filename env value */

static char		*runtime_buffer = NULL;

static const int	status_exception[] = {
	0,				/* 0x */
	COB_EC_I_O_AT_END,		/* 1x */
	COB_EC_I_O_INVALID_KEY,		/* 2x */
	COB_EC_I_O_PERMANENT_ERROR,	/* 3x */
	COB_EC_I_O_LOGIC_ERROR,		/* 4x */
	COB_EC_I_O_RECORD_OPERATION,	/* 5x */
	COB_EC_I_O_FILE_SHARING,	/* 6x */
	COB_EC_I_O,			/* Unused */
	COB_EC_I_O,			/* Unused */
	COB_EC_I_O_IMP			/* 9x */
};

static const char	* const prefix[] = { "DD_", "dd_", "" };
#define NUM_PREFIX	sizeof (prefix) / sizeof (char *)

static int dummy_delete		(cob_file *);
static int dummy_read		(cob_file *, cob_field *, const int);
static int dummy_start		(cob_file *, const int, cob_field *);

static int cob_file_open	(cob_file *, char *, const int, const int);
static int cob_file_close	(cob_file *, const int);
static int cob_file_write_opt	(cob_file *, const int);

static int sequential_read	(cob_file *, const int);
static int sequential_write	(cob_file *, const int);
static int sequential_rewrite	(cob_file *, const int);
static int lineseq_read		(cob_file *, const int);
static int lineseq_write	(cob_file *, const int);
//kamal079 - add rewrite
static int lineseq_rewrite(cob_file*, const int);
static int relative_start	(cob_file *, const int, cob_field *);
static int relative_read	(cob_file *, cob_field *, const int);
static int relative_read_next	(cob_file *, const int);
static int relative_write	(cob_file *, const int);
static int relative_rewrite	(cob_file *, const int);
static int relative_delete	(cob_file *);
//kamal079  - remove ext for VBISAM
static void remove_ext(char*);
static int indexed_open		(cob_file *, char *, const int, const int);
static int indexed_close	(cob_file *, const int);
static int indexed_start	(cob_file *, const int, cob_field *);
static int indexed_read		(cob_file *, cob_field *, const int);
static int indexed_read_next	(cob_file *, const int);
static int indexed_write	(cob_file *, const int);
static int indexed_delete	(cob_file *);
static int indexed_rewrite	(cob_file *, const int);

static const struct cob_fileio_funcs indexed_funcs = {
	indexed_open,
	indexed_close,
	indexed_start,
	indexed_read,
	indexed_read_next,
	indexed_write,
	indexed_rewrite,
	indexed_delete
};

static const struct cob_fileio_funcs sequential_funcs = {
	cob_file_open,
	cob_file_close,
	dummy_start,
	dummy_read,
	sequential_read,
	sequential_write,
	sequential_rewrite,
	dummy_delete
};

static const struct cob_fileio_funcs lineseq_funcs = {
	cob_file_open,
	cob_file_close,
	dummy_start,
	dummy_read,
	lineseq_read,
	lineseq_write,
	lineseq_rewrite,
	dummy_delete
};

static const struct cob_fileio_funcs relative_funcs = {
	cob_file_open,
	cob_file_close,
	relative_start,
	relative_read,
	relative_read_next,
	relative_write,
	relative_rewrite,
	relative_delete
};

static const struct cob_fileio_funcs	*fileio_funcs[COB_ORG_MAX] = {
	&sequential_funcs,
	&lineseq_funcs,
	&relative_funcs,
	&indexed_funcs,
	NULL
};

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
extern void	extfh_cob_init_fileio	(const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					const struct cob_fileio_funcs *,
					int (*)(cob_file *, const int));
extern void	extfh_cob_exit_fileio	(void);
#endif

#ifdef	WITH_INDEX_EXTFH
extern void extfh_indexed_unlock	(cob_file *);
extern int extfh_indexed_locate		(cob_file *, char *);
extern int extfh_indexed_open		(cob_file *, char *, const int, const int);
extern int extfh_indexed_close		(cob_file *, const int);
extern int extfh_indexed_start		(cob_file *, const int, cob_field *);
extern int extfh_indexed_read		(cob_file *, cob_field *, const int);
extern int extfh_indexed_read_next	(cob_file *, const int);
extern int extfh_indexed_write		(cob_file *, const int);
extern int extfh_indexed_delete		(cob_file *);
extern int extfh_indexed_rewrite	(cob_file *, const int);
#endif

#ifdef	WITH_SEQRA_EXTFH
extern void extfh_seqra_unlock		(cob_file *);
extern int extfh_seqra_locate		(cob_file *, char *);
extern int extfh_cob_file_open		(cob_file *, char *, const int, const int);
extern int extfh_cob_file_close		(cob_file *, const int);
extern int extfh_sequential_read	(cob_file *, const int);
extern int extfh_sequential_write	(cob_file *, const int);
extern int extfh_sequential_rewrite	(cob_file *, const int);
extern int extfh_relative_start		(cob_file *, const int, cob_field *);
extern int extfh_relative_read		(cob_file *, cob_field *, const int);
extern int extfh_relative_read_next	(cob_file *, const int);
extern int extfh_relative_write		(cob_file *, const int);
extern int extfh_relative_rewrite	(cob_file *, const int);
extern int extfh_relative_delete	(cob_file *);
#endif

#ifdef	WITH_DB

static DB_ENV		*bdb_env = NULL;
static char		*bdb_buff = NULL;
static const char	**bdb_data_dir = NULL;
static void		*record_lock_object = NULL;
static size_t		rlo_size = 0;
static unsigned int	bdb_lock_id = 0;

#define DB_PUT(db,flags)	db->put (db, NULL, &p->key, &p->data, flags)
#define DB_GET(db,flags)	db->get (db, NULL, &p->key, &p->data, flags)
#define DB_SEQ(db,flags)	db->c_get (db, &p->key, &p->data, flags)
#define DB_DEL(db,key,flags)	db->del (db, NULL, key, flags)
#define DB_CLOSE(db)		db->close (db, 0)
#define DB_SYNC(db)		db->sync (db, 0)
#define	cob_dbtsize_t		u_int32_t

#if	defined(WORDS_BIGENDIAN)
/* Big Endian then leave 'int' alone */
#define	COB_DUPSWAP(x)		((unsigned int)(x))

#elif	defined(COB_BDB_BAD_DUPNO) || 1 /* FIXME: may be added to a file specific flag */
/* Want to retain incorrect storing of Little Endian value backwards */
#define	COB_DUPSWAP(x)		((unsigned int)(x))

#else
/* Little Endian so swap byte around to have dupno value stored in bigendian sequence */
#define	COB_DUPSWAP(x)		(COB_BSWAP_32((unsigned int)(x)))
#endif

#define DBT_SET(key,fld)			\
	key.data = fld->data;			\
	key.size = (cob_dbtsize_t) fld->size

struct indexed_file {
	DB		**db;		/* Database handlers */
	DBC		**cursor;
	char		*filename;	/* Needed for record locks */
	unsigned char	*last_key;	/* The last key written */
	unsigned char	*temp_key;	/* Used for temporary storage */
	unsigned char	**last_readkey;	/* The last key read */
	unsigned int	*last_dupno;	/* The last number of duplicates read */
	int		*rewrite_sec_key;
	int		maxkeylen;
	int		primekeylen;
	unsigned char	*savekey;	/* Work area for saving key value */
	unsigned char	*suppkey;	/* Work area for saving key value */
	unsigned char	*saverec;	/* For saving copy of record */
	DBT		key;
	DBT		data;
	DB_LOCK		bdb_file_lock;
	DB_LOCK		bdb_record_lock;
	size_t		key_index;
	unsigned int	bdb_lock_id;
	int		write_cursor_open;
	int		record_locked;
	int		filenamelen;
};

static int
bdb_findkey (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen)
{
	int 	k, part;

	*fullkeylen = *partlen = 0;
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].field
		&&  f->keys[k].count_components <= 1
		&&  f->keys[k].field->data == kf->data) {
			*fullkeylen = f->keys[k].field->size;
			*partlen = kf->size;
			return k;
		}
	}
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].count_components > 1) {
			if ( (f->keys[k].field
			   && f->keys[k].field->data == kf->data
			   && f->keys[k].field->size == kf->size)
			 ||  (f->keys[k].component[0]->data == kf->data)) {
				for (part = 0; part < f->keys[k].count_components; part++) {
					*fullkeylen += f->keys[k].component[part]->size;
				}
				if (f->keys[k].field && f->keys[k].field->data == kf->data) {
					*partlen = kf->size;
				} else {
					*partlen = *fullkeylen;
				}
				return k;
			}
		}
	}
	return -1;
}

/* Return total length of the key */
static int
bdb_keylen (cob_file *f, int idx)
{
	int totlen, part;

	if (idx < 0 || idx > f->nkeys) {
		return -1;
	}
	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components; part++) {
			totlen += f->keys[idx].component[part]->size;
		}
		return totlen;
	}
	return f->keys[idx].field->size;
}

/* Save key for given index from 'record' into 'keyarea',
   returns total length of the key */
static int
bdb_savekey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx)
{
	int totlen, part;

	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components; part++) {
			memcpy (keyarea + totlen,
				record + (f->keys[idx].component[part]->data - f->record->data),
				f->keys[idx].component[part]->size);
			totlen += f->keys[idx].component[part]->size;
		}
		return totlen;
	}
	memcpy (keyarea,
		record + (f->keys[idx].field->data - f->record->data),
		f->keys[idx].field->size);
	return f->keys[idx].field->size;
}

static void
bdb_setkey (cob_file *f, int idx)
{
	struct indexed_file	*p;
	int	len;

	p = f->file;
	memset (p->savekey, 0, p->maxkeylen);
	len = bdb_savekey (f, p->savekey, f->record->data, idx);
	p->key.data = p->savekey;
	p->key.size = (cob_dbtsize_t) len;
}

/* Compare key for given index 'keyarea' to 'record'.
   returns compare status */
static int
bdb_cmpkey (cob_file *f, unsigned char *keyarea, unsigned char *record, int idx, int partlen)
{
	int sts, part, totlen;
	size_t	cl;

	if (partlen <= 0) {
		partlen = bdb_keylen(f, idx);
	}
	if (f->keys[idx].count_components > 0) {
		totlen = 0;
		for (part = 0; part < f->keys[idx].count_components && partlen > 0; part++) {
			cl = partlen > f->keys[idx].component[part]->size ? f->keys[idx].component[part]->size : partlen;
			sts = memcmp (keyarea + totlen,
					record + (f->keys[idx].component[part]->data - f->record->data),
					cl);
			if (sts != 0) {
				return sts;
			}
			totlen += f->keys[idx].component[part]->size;
			partlen -= f->keys[idx].component[part]->size;
		}
		return 0;
	}
	cl = partlen > f->keys[idx].field->size ? f->keys[idx].field->size : partlen;
	return memcmp (keyarea,
			record  + (f->keys[idx].field->data - f->record->data),
			cl);
}

/* Is given key data all SUPPRESS char,
   returns 1 if key has all SUPPRESS char */
static int
bdb_suppresskey (cob_file *f, int idx)
{
	unsigned char ch_sprs;
	int 	i,len;
	struct indexed_file	*p;

	if (!f->keys[idx].tf_suppress) {
		return 0;
	}
	ch_sprs = f->keys[idx].char_suppress & 0xFF;
	p = f->file;
	len = bdb_savekey(f, p->suppkey, f->record->data, idx);
	for (i = 0; i < len; i++) {
		if (p->suppkey[i] != ch_sprs)
			return 0;
	}
	return 1;
}

/* Open the 'write cursor' if needed and return 0 is already open */
static int
bdb_open_cursor(cob_file *f, int for_write)
{
	struct indexed_file	*p;
	int		flags;

	p = f->file;
	if(p->write_cursor_open)
		return 0;		/* It is already open */
	if (bdb_env && for_write) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], flags);
	p->write_cursor_open = 1;
	return 1;
}

/* Close the 'write cursor' if needed and return 0 is already closed */
static int
bdb_close_cursor(cob_file *f)
{
	struct indexed_file	*p;

	p = f->file;
	p->write_cursor_open = 0;
	if(p->cursor[0] == NULL)
		return 0;		/* It is already closed */
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 6))
	p->cursor[0]->close (p->cursor[0]);
#else
	p->cursor[0]->c_close (p->cursor[0]);
#endif
	p->cursor[0] = NULL;
	return 1;
}

/* Close the 'cursor' on a specific index */
static int
bdb_close_index (cob_file *f, int index)
{
	struct indexed_file	*p;

	p = f->file;
	if (index == 0)
		p->write_cursor_open = 0;
	if(p->cursor[index] == NULL)
		return 0;		/* It is already closed */
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 6))
	p->cursor[index]->close (p->cursor[index]);
#else
	p->cursor[index]->c_close (p->cursor[index]);
#endif
	p->cursor[index] = NULL;
	return 1;
}
#endif	/* WITH_DB */


/* Local functions */

static int
dummy_delete (cob_file *f)
{
	COB_UNUSED (f);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_read (cob_file *f, cob_field *key, const int read_opts)
{
	COB_UNUSED (f);
	COB_UNUSED (key);
	COB_UNUSED (read_opts);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static int
dummy_start (cob_file *f, const int cond, cob_field *key)
{
	COB_UNUSED (f);
	COB_UNUSED (cond);
	COB_UNUSED (key);

	return COB_STATUS_91_NOT_AVAILABLE;
}

static char *
cob_chk_file_env (cob_file* f, const char *src)
{
	char		*p;
	char		*q;
	char		*s;
	const char* t;
	size_t		i;

	if (unlikely (cobsetptr->cob_env_mangle)) {
		q = cob_strdup (src);
		s = q;
		for (i = 0; i < strlen (s); ++i) {
			if (!isalnum ((int)s[i])) {
				s[i] = '_';
			}
		}
	} else {
		q = NULL;
		s = (char *)src;
	}

	//kamal079 - merge mf file changes
	/* Check for IO_filename with file specific options */
	file_open_io_env = NULL;
	snprintf(file_open_env, (size_t)COB_FILE_MAX, "%s%s", "IO_", s);
	if ((file_open_io_env = (unsigned char*)getenv(file_open_env)) == NULL) {
		snprintf(file_open_env, (size_t)COB_FILE_MAX, "%s%s", "io_", s);
		if ((file_open_io_env = (unsigned char*)getenv(file_open_env)) == NULL) {
			for (i = 0; file_open_env[i] != 0; ++i) {	/* Try all Upper Case */
				if (islower((unsigned char)file_open_env[i]))
					file_open_env[i] = toupper((unsigned char)file_open_env[i]);
			}
			file_open_io_env = (unsigned char*)getenv(file_open_env);
		}
	}
	if (file_open_io_env == NULL) {
		/* Re-check for IO_fdname */
		snprintf(file_open_env, (size_t)COB_FILE_MAX, "%s%s", "IO_", f->select_name);
		if ((file_open_io_env = (unsigned char*)getenv(file_open_env)) == NULL) {
			snprintf(file_open_env, (size_t)COB_FILE_MAX, "%s%s", "io_", f->select_name);
			if ((file_open_io_env = (unsigned char*)getenv(file_open_env)) == NULL) {
				for (i = 0; file_open_env[i] != 0; ++i) {	/* Try all Upper Case */
					if (islower((unsigned char)file_open_env[i]))
						file_open_env[i] = toupper((unsigned char)file_open_env[i]);
				}
				file_open_io_env = (unsigned char*)getenv(file_open_env);
			}
		}
	}

	if (file_open_io_env == NULL) {		/* Re-check for xx_OPTIONS where 'xx' depends on file type */
		if (f->organization == COB_ORG_INDEXED) {
			t = "IX";
		} else if (f->organization == COB_ORG_SEQUENTIAL) {
			t = "SQ";
		} else if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->flag_line_adv)
				t = "LA";
			else
				t = "LS";
		} else if (f->organization == COB_ORG_RELATIVE) {
			t = "RL";
		} else {
			t = "IO";
		}
		snprintf(file_open_env, (size_t)COB_FILE_MAX, "%s_OPTIONS", t);
		if ((file_open_io_env = (unsigned char*)getenv(file_open_env)) == NULL) {
			snprintf(file_open_env, (size_t)COB_FILE_MAX, "%s_options", t);
			file_open_env[0] = tolower(file_open_env[0]);
			file_open_env[1] = tolower(file_open_env[1]);
			file_open_io_env = (unsigned char*)getenv(file_open_env);
		}
	}
	//kamal079 - merge mf file changes

	p = NULL;
	for (i = 0; i < NUM_PREFIX; ++i) {
		snprintf (file_open_env, (size_t)COB_FILE_MAX, "%s%s",
			  prefix[i], s);
		file_open_env[COB_FILE_MAX] = 0;
		p = getenv (file_open_env);
		if (p) {
			break;
		}
	}
	//kamal079 - merge mf file changes
	if (p == NULL) {		/* Try all Upper case env var name */
		for (i = 0; i < NUM_PREFIX; ++i) {
			snprintf(file_open_env, (size_t)COB_FILE_MAX, "%s%s", prefix[i], s);
			for (i = 0; file_open_env[i] != 0; ++i) {
				if (islower((unsigned char)file_open_env[i]))
					file_open_env[i] = toupper((unsigned char)file_open_env[i]);
			}
			if ((p = getenv(file_open_env)) != NULL)
				break;
		}
	}

	if (unlikely (q)) {
		cob_free (q);
	}
	return p;
}

static void
cob_chk_file_mapping(cob_file* f)
{
	char		*p;
	char		*src;
	char		*dst;
	char		*saveptr;
	char		*orig;
	unsigned int	dollar;
	//kamal079 - merge mf file changes
	if (*file_open_name == 0) {
		strcpy(file_open_name, f->select_name);
	}

	if (unlikely (!COB_MODULE_PTR->flag_filename_mapping)) {
		return;
	}

	/* Misuse "dollar" here to indicate a separator */
	dollar = 0;
	for (p = file_open_name; *p; p++) {
		if (*p == '/' || *p == '\\') {
			dollar = 1;
			break;
		}
	}

	src = file_open_name;

	/* Simple case - No separators */
	if (likely(dollar == 0)) {
		/* Ignore leading dollar */
		if (*src == '$') {
			src++;
		}
		/* Check for DD_xx, dd_xx, xx environment variables */
		/* If not found, use as is including the dollar character */
		if ((p = cob_chk_file_env (f,src)) != NULL) {
			strncpy (file_open_name, p, (size_t)COB_FILE_MAX);
		} else if (cobsetptr->cob_file_path) {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s%c%s",
				  cobsetptr->cob_file_path, SLASH_CHAR, file_open_name);
			file_open_buff[COB_FILE_MAX] = 0;
			strncpy (file_open_name, file_open_buff,
				 (size_t)COB_FILE_MAX);
		}
		return;
	}

	/* Complex */
	/* Isolate first element (everything before the slash) */
	/* If it starts with a slash, it's absolute, do nothing */
	/* Else if it starts with a $, mark and skip over the $ */
	/* Try mapping on resultant string - DD_xx, dd_xx, xx */
	/* If successful, use the mapping */
	/* If not, use original element EXCEPT if we started */
	/* with a $, in which case, we ignore the element AND */
	/* the following slash */

	dollar = 0;
	dst = file_open_buff;
	*dst = 0;

	if (*src == '$') {
		dollar = 1;
		src++;
	}

	orig = cob_strdup (src);
	saveptr = orig;

	/* strtok strips leading delimiters */
	if (*src == '/' || *src == '\\') {
		strcpy (file_open_buff, SLASH_STR);
	} else {
		file_open_buff[COB_FILE_MAX] = 0;
		p = strtok (orig, "/\\");
		orig = NULL;
		if ((src = cob_chk_file_env (f,p)) != NULL) {
			strncpy (file_open_buff, src, (size_t)COB_FILE_MAX);
			dollar = 0;
		} else if (!dollar) {
			strncpy (file_open_buff, p, (size_t)COB_FILE_MAX);
		}
	}
	/* First element completed, loop through remaining */
	/* elements delimited by slash */
	/* Check each for $ mapping */
	for (; ;) {
		p = strtok (orig, "/\\");
		if (!p) {
			break;
		}
		if (!orig) {
			if (dollar) {
				dollar = 0;
			} else {
				strcat (file_open_buff, SLASH_STR);
			}
		} else {
			orig = NULL;
		}
		if (*p == '$' && (src = cob_chk_file_env (f,p + 1)) != NULL) {
			strncat (file_open_buff, src, (size_t)COB_FILE_MAX);
		} else {
			strncat (file_open_buff, p, (size_t)COB_FILE_MAX);
		}
	}
	strcpy (file_open_name, file_open_buff);
	cob_free (saveptr);
}

static void
cob_sync (cob_file *f)
{
#ifdef	WITH_DB
	struct indexed_file	*p;
	size_t			i;
#elif	defined(WITH_ANY_ISAM)
	struct indexfile	*fh;
#endif

	if (f->organization == COB_ORG_INDEXED) {
#ifdef	WITH_DB
		p = f->file;
		if (p) {
			for (i = 0; i < f->nkeys; ++i) {
				if (p->db[i]) {
					DB_SYNC (p->db[i]);
				}
			}
		}
#elif	defined(WITH_ANY_ISAM)
		fh = f->file;
		if (fh) {
			isflush (fh->isfd);
		}
#endif
		return;
	}
	if (f->organization != COB_ORG_SORT) {
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->file) {
				fflush ((FILE *)f->file);
			}
		}
		if (f->fd >= 0) {
			fdcobsync (f->fd);
		}
	}
}

static void
cob_cache_file (cob_file *f)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (f == l->file) {
			return;
		}
	}
	l = cob_malloc (sizeof (struct file_list));
	l->file = f;
	l->next = file_cache;
	file_cache = l;
}

/*
 * Set file format based on defaults, runtime.cfg and IO_filename options
 */

static void
set_file_format(cob_file* f)
{
	int	i, j, settrue;
	char	option[32], value[30];

	f->trace_io = cobsetptr->cob_trace_io ? 1 : 0;
	f->io_stats = cobsetptr->cob_stats_record ? 1 : 0;
	f->flag_keycheck = cobsetptr->cob_keycheck ? 1 : 0;
	if (cobsetptr->cob_do_sync)
		f->file_features |= COB_FILE_SYNC;
	else
		f->file_features &= ~COB_FILE_SYNC;
	f->dflt_times = cobsetptr->cob_retry_times;
	f->dflt_seconds = cobsetptr->cob_retry_seconds;
	f->dflt_share = cobsetptr->cob_share_mode;
	f->dflt_retry = cobsetptr->cob_retry_mode;
	if (f->dflt_retry == 0) {
		if (f->dflt_times > 0)
			f->dflt_retry |= COB_RETRY_TIMES;
		if (f->dflt_seconds > 0)
			f->dflt_retry |= COB_RETRY_SECONDS;
	}

	if (f->file_format == 255) {	/* File type not set by compiler; Set default */
		if (f->organization == COB_ORG_SEQUENTIAL) {
			f->file_format = COB_FILE_IS_GC;
			if (f->record_min != f->record_max) {
				f->file_format = cobsetptr->cob_varseq_type;
			} else {
				f->file_format = 0;
			}
		} else
			if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
				f->file_format = COB_FILE_IS_GC;
			} else
				if (f->organization == COB_ORG_RELATIVE) {
					if (f->record_min != f->record_max) {
						f->file_format = cobsetptr->cob_varrel_type;
					} else {
						f->file_format = cobsetptr->cob_fixrel_type;
					}
				}
	}

	if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
		if (cobsetptr->cob_ls_fixed)
			f->file_features |= COB_FILE_LS_FIXED;
		else
			f->file_features &= ~COB_FILE_LS_FIXED;
#ifdef	_WIN32
		if (cobsetptr->cob_unix_lf)
			f->file_features |= COB_FILE_LS_LF;
		else
			f->file_features |= COB_FILE_LS_CRLF;
#else
		f->file_features |= COB_FILE_LS_LF;
#endif
		if (cobsetptr->cob_ls_uses_cr)
			f->file_features |= COB_FILE_LS_CRLF;

		if (f->file_format == COB_FILE_IS_MF) {			/* Micro Focus format LINE SEQUENTIAL */
			if (cobsetptr->cob_mf_ls_split)
				f->file_features |= COB_FILE_LS_SPLIT;
			else
				f->file_features &= ~COB_FILE_LS_SPLIT;
			if (cobsetptr->cob_mf_ls_nulls)
				f->file_features |= COB_FILE_LS_NULLS;
			else
				f->file_features &= ~COB_FILE_LS_NULLS;
			if (cobsetptr->cob_mf_ls_validate
				&& !f->flag_line_adv)
				f->file_features |= COB_FILE_LS_VALIDATE;
			else
				f->file_features &= ~COB_FILE_LS_VALIDATE;
		}
		else {										/* GnuCOBOL default format LINE SEQUENTIAL */
			if (cobsetptr->cob_ls_split)
				f->file_features |= COB_FILE_LS_SPLIT;
			else
				f->file_features &= ~COB_FILE_LS_SPLIT;
			if (cobsetptr->cob_ls_nulls)
				f->file_features |= COB_FILE_LS_NULLS;
			else
				f->file_features &= ~COB_FILE_LS_NULLS;
			if (cobsetptr->cob_ls_validate
				&& !f->flag_line_adv)
				f->file_features |= COB_FILE_LS_VALIDATE;
			else
				f->file_features &= ~COB_FILE_LS_VALIDATE;
		}
	}

	/*
	 * IO_filename was found
	*/
	if (file_open_io_env != NULL) {		/* Special options for just this file */
		f->dflt_retry = 0;
		for (i = 0; file_open_io_env[i] != 0; ) {
			while (isspace(file_open_io_env[i])	/* Skip option separators */
				|| file_open_io_env[i] == ','
				|| file_open_io_env[i] == ';') i++;
			if (file_open_io_env[i] == 0)
				break;
			for (j = 0; j < sizeof(option) - 1 && !isspace(file_open_io_env[i])
				&& file_open_io_env[i] != ','
				&& file_open_io_env[i] != ';'
				&& file_open_io_env[i] != '='
				&& file_open_io_env[i] != 0; ) {	/* Collect one option */
				option[j++] = file_open_io_env[i++];
			}
			option[j] = 0;
			value[0] = 0;
			settrue = 1;
			if (strncasecmp(option, "no-", 3) == 0) {
				memmove(option, &option[3], j);
				settrue = 0;
			}
			else
				if (strncasecmp(option, "no_", 3) == 0) {
					memmove(option, &option[3], j);
					settrue = 0;
				}
				else
					if (strncasecmp(option, "no", 2) == 0) {
						memmove(option, &option[2], j);
						settrue = 0;
					}
			if (file_open_io_env[i] == '=') {
				i++;
				for (j = 0; j < sizeof(value) - 1 && !isspace(file_open_io_env[i])
					&& file_open_io_env[i] != ','
					&& file_open_io_env[i] != ';'
					&& file_open_io_env[i] != 0; ) {	/* Collect one option */
					value[j++] = file_open_io_env[i++];
				}
				value[j] = 0;
				if (value[0] == '1'
					|| toupper((unsigned char)value[0]) == 'T'
					|| strcasecmp(value, "on") == 0)
					settrue = 1;
				if (value[0] == '0'
					|| toupper((unsigned char)value[0]) == 'F'
					|| strcasecmp(value, "off") == 0)
					settrue = 0;
			}
			if (strcasecmp(option, "sync") == 0) {
				if (settrue)
					f->file_features |= COB_FILE_SYNC;
				else
					f->file_features &= ~COB_FILE_SYNC;
				continue;
			}
			if (strcasecmp(option, "trace") == 0) {
				f->trace_io = settrue ? 1 : 0;
				continue;
			}
			if (strcasecmp(option, "stats") == 0) {
				f->io_stats = settrue ? 1 : 0;
				continue;
			}
			if (strcasecmp(option, "keycheck") == 0) {
				f->flag_keycheck = settrue ? 1 : 0;
				continue;
			}
			if (strcasecmp(option, "retry_times") == 0) {
				f->dflt_times = atoi(value);
				f->dflt_retry |= COB_RETRY_TIMES;
				continue;
			}
			if (strcasecmp(option, "retry_seconds") == 0) {
				f->dflt_seconds = atoi(value);
				f->dflt_retry |= COB_RETRY_SECONDS;
				continue;
			}
			if (strcasecmp(option, "retry_forever") == 0) {
				f->dflt_retry = COB_RETRY_FOREVER;
				continue;
			}
			if (strcasecmp(option, "retry_never") == 0) {
				f->dflt_retry = COB_RETRY_NEVER;
				continue;
			}
			if (strcasecmp(option, "ignore_lock") == 0) {
				f->dflt_retry |= COB_IGNORE_LOCK;
				continue;
			}
			if (strcasecmp(option, "advancing_lock") == 0) {
				f->dflt_retry |= COB_ADVANCING_LOCK;
				continue;
			}
			if (strcasecmp(option, "share_all") == 0) {
				f->dflt_share = COB_SHARE_ALL_OTHER;
				continue;
			}
			if (strcasecmp(option, "share_read") == 0) {
				f->dflt_share = COB_SHARE_READ_ONLY;
				continue;
			}
			if (strcasecmp(option, "share_no") == 0) {
				f->dflt_share = COB_SHARE_NO_OTHER;
				continue;
			}
			if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
				if (strcasecmp(option, "ls_nulls") == 0) {
					if (settrue)
						f->file_features |= COB_FILE_LS_NULLS;
					else
						f->file_features &= ~COB_FILE_LS_NULLS;
					continue;
				}
				if (strcasecmp(option, "ls_fixed") == 0) {
					if (settrue)
						f->file_features |= COB_FILE_LS_FIXED;
					else
						f->file_features &= ~COB_FILE_LS_FIXED;
					continue;
				}
				if (strcasecmp(option, "ls_split") == 0) {
					if (settrue)
						f->file_features |= COB_FILE_LS_SPLIT;
					else
						f->file_features &= ~COB_FILE_LS_SPLIT;
					continue;
				}
				if (strcasecmp(option, "ls_validate") == 0) {
					if (settrue)
						f->file_features |= COB_FILE_LS_VALIDATE;
					else
						f->file_features &= ~COB_FILE_LS_VALIDATE;
					continue;
				}
				if (strcasecmp(option, "crlf") == 0) {
					if (settrue)
						f->file_features |= COB_FILE_LS_CRLF;
					else
						f->file_features &= ~COB_FILE_LS_CRLF;
					continue;
				}
				if (strcasecmp(option, "lf") == 0) {
					if (settrue) {
						f->file_features &= ~COB_FILE_LS_CRLF;
						f->file_features |= COB_FILE_LS_LF;
					} else {
						f->file_features &= ~COB_FILE_LS_LF;
					}
					continue;
				}
				if (strcasecmp(option, "mf") == 0) {	/* LS file like MF would do */
					f->file_features &= ~COB_FILE_LS_FIXED;
					f->file_features |= COB_FILE_LS_NULLS;
					f->file_features |= COB_FILE_LS_SPLIT;
					f->file_features &= ~COB_FILE_LS_VALIDATE;
#ifdef	_WIN32
					f->file_features |= COB_FILE_LS_CRLF;
#else
					f->file_features |= COB_FILE_LS_LF;
#endif
				}
				if (strcasecmp(option, "gc") == 0) {	/* LS file like GNUCobol used to do */
					f->file_features &= ~COB_FILE_LS_FIXED;
					f->file_features &= ~COB_FILE_LS_NULLS;
					f->file_features &= ~COB_FILE_LS_SPLIT;
					f->file_features &= ~COB_FILE_LS_VALIDATE;
#ifdef	_WIN32
					f->file_features |= COB_FILE_LS_CRLF;
#else
					f->file_features |= COB_FILE_LS_LF;
#endif
				}
			}
			if (strcasecmp(option, "mf") == 0) {
				if (settrue) {
					f->file_format = COB_FILE_IS_MF;
					continue;
				}
				continue;
			}
			if (strcasecmp(option, "gc") == 0) {
				if (settrue) {
					f->file_format = COB_FILE_IS_GC;
					continue;
				}
				continue;
			}
			if (f->organization == COB_ORG_SEQUENTIAL
				&& f->record_min != f->record_max) {		/* Variable length Sequential */
				if (strcasecmp(option, "0") == 0) {
					f->file_format = COB_FILE_IS_GCVS0;
				} else
					if (strcasecmp(option, "1") == 0) {
						f->file_format = COB_FILE_IS_GCVS1;
					} else
						if (strcasecmp(option, "2") == 0) {
							f->file_format = COB_FILE_IS_GCVS2;
						} else
							if (strcasecmp(option, "3") == 0) {
								f->file_format = COB_FILE_IS_GCVS3;
							} else
								if (strcasecmp(option, "b4") == 0
									|| strcasecmp(option, "b32") == 0) {
									f->file_format = COB_FILE_IS_B32;
								} else
									if (strcasecmp(option, "l4") == 0
										|| strcasecmp(option, "l32") == 0) {
										f->file_format = COB_FILE_IS_L32;
									}
			}
			if (f->organization == COB_ORG_RELATIVE) {	/* Relative format */
				if (strcasecmp(option, "b4") == 0
					|| strcasecmp(option, "b32") == 0) {
					f->file_format = COB_FILE_IS_B32;
				} else
					if (strcasecmp(option, "l4") == 0
						|| strcasecmp(option, "l32") == 0) {
						f->file_format = COB_FILE_IS_L32;
					} else
						if (strcasecmp(option, "b8") == 0
							|| strcasecmp(option, "b64") == 0) {
							f->file_format = COB_FILE_IS_B64;
						} else
							if (strcasecmp(option, "l8") == 0
								|| strcasecmp(option, "l64") == 0) {
								f->file_format = COB_FILE_IS_L64;
							}
			}
		}
		/* If SHARE or RETRY given, then override application choices */
		if (f->dflt_share != 0)
			f->share_mode = f->dflt_share;
		if (f->dflt_retry != 0) {
			f->retry_mode = f->dflt_retry;
			f->retry_times = f->dflt_times;
			f->retry_seconds = f->dflt_seconds;
		}
	}

	f->record_off = -1;
	f->flag_begin_of_file = 1;
	f->record_prefix = 0;
	f->file_header = 0;
	/* Set File type specific values */
	if (f->organization == COB_ORG_SEQUENTIAL) {
		if (f->record_min != f->record_max) {
			if (f->file_format == COB_FILE_IS_GCVS0
				|| f->file_format == COB_FILE_IS_GCVS1
				|| f->file_format == COB_FILE_IS_GCVS2) {
				f->record_prefix = 4;
			}
			else
				if (f->file_format == COB_FILE_IS_GCVS3) {
					f->record_prefix = 2;
				}
				else
					if (f->file_format == COB_FILE_IS_L32
						|| f->file_format == COB_FILE_IS_B32) {
						f->record_prefix = 4;
					}
					else
						if (f->file_format == COB_FILE_IS_MF) {
							f->record_prefix = 4;
							f->file_header = 128;
							f->record_slot = f->record_max + f->record_prefix + 1;
						}
						else {
							f->record_prefix = 4;
						}
		}
		f->record_slot = f->record_max + f->record_prefix;
	}
	else
		if (f->organization == COB_ORG_RELATIVE) {
			f->record_prefix = sizeof(size_t);
			f->record_slot = f->record_max + f->record_prefix;
			if (f->file_format == COB_FILE_IS_B32
				|| f->file_format == COB_FILE_IS_L32) {
				f->record_prefix = 4;
				f->record_slot = f->record_max + f->record_prefix;
			}
			else
				if (f->file_format == COB_FILE_IS_B64
					|| f->file_format == COB_FILE_IS_L64) {
					f->record_prefix = 8;
					f->record_slot = f->record_max + f->record_prefix;
				}
				else
					if (f->file_format == COB_FILE_IS_MF) {
						if (f->record_min != f->record_max) {	/* Variable length Relative */
							if (f->record_max < 4096) {
								f->record_prefix = 2;
							}
							else {
								f->record_prefix = 4;
							}
							f->file_header = 128;
							f->record_slot = f->record_max + f->record_prefix + 1;
						}
						else {
							f->record_prefix = 0;
							f->record_slot = f->record_max + 1;
						}
					}
		}
}

#ifdef	HAVE_FCNTL
#if defined(HAVE_SIGACTION) && defined(SIGALRM)
static void catch_alarm(int sig) { }
#endif

/*
 * Issue File/Record lock
 */
static int
lock_record(
	cob_file* f,
	unsigned int recnum,
	int 	forwrite,
	int* errsts)
{
#if defined(HAVE_SIGACTION) && defined(SIGALRM)
	struct sigaction sigact, prvact;
	int		wait_time;
#endif
	int		lock_type, retry, interval;
	unsigned long	pos;
	unsigned int	rcsz;
	struct flock	lck;

	lock_type = forwrite ? F_WRLCK : F_RDLCK;
	retry = interval = 0;
	if (f->retry_mode == 0) {
		/* Nothing else to do */
	}
	else
		if ((f->retry_mode & COB_RETRY_FOREVER)) {
			retry = -1;
		}
		else
			if ((f->retry_mode & COB_RETRY_SECONDS)) {
				retry = 1;
				interval = f->retry_seconds > 0 ? f->retry_seconds :
					(cobsetptr->cob_retry_seconds > 0 ? cobsetptr->cob_retry_seconds : 1);
			}
			else
				if ((f->retry_mode & COB_RETRY_TIMES)) {
					retry = f->retry_times > 0 ? f->retry_times :
						(cobsetptr->cob_retry_times > 0 ? cobsetptr->cob_retry_times : 1);
					interval = cobsetptr->cob_retry_seconds > 0 ? cobsetptr->cob_retry_seconds : 1;
				}
	if (recnum == 0) {			/* Lock entire file */
		pos = 0;
		rcsz = 0;
		f->flag_file_lock = 0;
	}
	else {
		rcsz = (unsigned int)f->record_max;
		if (rcsz <= 0)
			rcsz = 2;
		if (f->record_slot <= 0)
			f->record_slot = rcsz + 1;
		pos = (unsigned long)(f->file_header + ((recnum - 1) * f->record_slot));
	}
	memset(&lck, 0, sizeof(struct flock));
	lck.l_type = lock_type;
	lck.l_whence = SEEK_SET;
	lck.l_start = pos;
	lck.l_len = rcsz;
	if (fcntl(f->fd, F_SETLK, &lck) != -1) {
		*errsts = 0;
		if (recnum == 0
			&& forwrite) 			/* File locked for Exclusive use */
			f->flag_file_lock = 1;
		return 1;			/* Got the lock so all is good */
	}
	*errsts = errno;
	if (retry == 0) {			/* No RETRY, so return with no lock */
		if (errno == EAGAIN) {
			lck.l_type = lock_type;
			lck.l_whence = SEEK_SET;
			lck.l_start = pos;
			lck.l_len = rcsz;
			if (fcntl(f->fd, F_GETLK, &lck) == -1) {
				if (lck.l_pid == cob_sys_getpid()) {	/* Is locked by me */
					return 1;
				}
			}
		}
		return 0;
	}
	if (interval <= 0)
		interval = COB_RETRY_PER_SECOND;

#if defined(HAVE_SIGACTION) && defined(SIGALRM)
	if (retry > 0) {				/* Negative means wait forever */
		memset(&prvact, 0, sizeof(sigact));
		prvact.sa_handler = SIG_DFL;
		memset(&sigact, 0, sizeof(sigact));
		sigact.sa_handler = catch_alarm;
		sigaction(SIGALRM, &sigact, &prvact);
		wait_time = retry * interval;
		alarm(wait_time);
	}
	if (fcntl(f->fd, F_SETLKW, &lck) != -1) {
		if (retry > 0) {
			sigaction(SIGALRM, &prvact, NULL);
			alarm(0);
		}
		*errsts = 0;
		if (recnum == 0
			&& forwrite) 			/* File locked for Exclusive use */
			f->flag_file_lock = 1;
		return 1;			/* Got the lock so all is good */
	}
	*errsts = errno;
	if (retry > 0) {
		sigaction(SIGALRM, &prvact, NULL);
		alarm(0);
		if (*errsts == EINTR)		/* Timed out, so return EAGAIN */
			* errsts = EAGAIN;
	}
	return 0;				/* Record is not locked! */
#else
	if (retry > 0) {
		retry = retry * 4;
		interval = (interval * 1000) / 4;
		while (retry-- > 0) {
			lck.l_type = lock_type;
			lck.l_whence = SEEK_SET;
			lck.l_start = pos;
			lck.l_len = rcsz;
			*errsts = 0;
			if (fcntl(f->fd, F_SETLK, &lck) != -1) {
				if (recnum == 0
					&& forwrite) 	/* File locked for Exclusive use */
					f->flag_file_lock = 1;
				return 1;	/* Got the lock so all is good */
			}
			*errsts = errno;
			cob_sys_sleep_msec(interval);
		}
	}
	else {
		while (1) {
			lck.l_type = lock_type;
			lck.l_whence = SEEK_SET;
			lck.l_start = pos;
			lck.l_len = rcsz;
			*errsts = 0;
			if (fcntl(f->fd, F_SETLK, &lck) != -1) {
				if (recnum == 0
					&& forwrite) 	/* File locked for Exclusive use */
					f->flag_file_lock = 1;
				return 1;	/* Got the lock so all is good */
			}
			*errsts = errno;
			cob_sys_sleep_msec(250);
		}
	}
	return 0;				/* Record is not locked! */
#endif
}

/*
 * Un-Lock 'recnum' with system
 */
static int
unlock_record(cob_file * f, unsigned int recnum)
{
	unsigned long pos;
	unsigned int rcsz;
	struct flock lck;

	if (recnum == 0) {			/* Un-Lock entire file */
		pos = 0;
		rcsz = 0;
		f->flag_file_lock = 0;
	}
	else {
		rcsz = (unsigned int)f->record_max;
		if (rcsz <= 0)
			rcsz = 2;
		if (f->record_slot <= 0)
			f->record_slot = rcsz + 1;
		pos = (unsigned long)(f->file_header + ((recnum - 1) * f->record_slot));
	}
	lck.l_type = F_UNLCK;
	lck.l_whence = SEEK_SET;
	lck.l_start = pos;
	lck.l_len = rcsz;
	errno = 0;
	if (fcntl(f->fd, F_SETLK, &lck) != -1) {
		return 1;					/* Released the lock so all is good */
	}
	return 0;						/* Record is not locked! */
}

#else
/* System does not even have 'fcntl' so no Record/File lock is used */
static int
lock_record(
	cob_file * f,
	unsigned int recnum,
	int 	forwrite,
	int* errsts)
{
	COB_UNUSED(f);
	COB_UNUSED(recnum);
	COB_UNUSED(forwrite);
	*errsts = 0;
	return 1;
}

static int
unlock_record(cob_file * f, unsigned int recnum)
{
	COB_UNUSED(f);
	COB_UNUSED(recnum);
	return 1;
}

#endif


/*
 * Determine if file should be locked
 */
static int
set_file_lock(cob_file* f, const char* filename, int open_mode)
{
	int	lock_mode, ret;

	f->flag_record_lock = 0;
	f->flag_file_lock = 0;
	if (memcmp(filename, "/dev/", (size_t)5) == 0) 	/* Do not lock Devices */
		return 0;

	if ((f->share_mode & COB_SHARE_ALL_OTHER)
		&& ((open_mode == COB_OPEN_INPUT) || (open_mode == COB_OPEN_I_O))) {/* File is SHARE ALL */
		f->flag_record_lock = 1;
		return 0;
	}

	/* Lock the file */
	if ((f->share_mode & COB_SHARE_ALL_OTHER)) {
		if (open_mode == COB_OPEN_OUTPUT)
			lock_mode = 1;
		else
			lock_mode = 0;
	}
	else if ((open_mode != COB_OPEN_INPUT)
		|| (f->share_mode & COB_SHARE_NO_OTHER)
		|| (f->lock_mode & COB_FILE_EXCLUSIVE)) {
		lock_mode = 1;
	}
	else {
		lock_mode = 0;
	}

	lock_record(f, 0, lock_mode, &ret);
	if (ret != 0) {
		if (f->file)
			fclose(f->file);
		else
			close(f->fd);
		f->fd = -1;
		f->file = NULL;
		f->open_mode = COB_OPEN_CLOSED;
		switch (ret) {
		case EACCES:
		case EAGAIN:
		case EDEADLK:
			return COB_STATUS_61_FILE_SHARING;
		default:
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}
	if (!f->flag_file_lock) {
		if ((open_mode != COB_OPEN_INPUT)
			&& (f->share_mode & COB_SHARE_ALL_OTHER)) {
			f->flag_record_lock = 1;
		}
	}
	return 0;
}

/*
 * Determine if current record should be locked and if previous lock to be released
 */
static void
set_lock_opts(cob_file* f, unsigned int read_opts)
{
	f->flag_lock_mode = 0;		/* READ lock */
	if (f->retry_mode == 0
		&& f->dflt_retry != 0) {	/* Use IO_filename RETRY values */
		f->retry_mode = f->dflt_retry;
		f->retry_times = f->dflt_times;
		f->retry_seconds = f->dflt_seconds;
	}
	if (f->flag_file_lock) {	/* File is EXCLUSIVE */
		f->flag_lock_rec = 0;
		f->flag_lock_rls = 0;
		return;
	}
	if (!f->lock_mode) {
		if (f->open_mode != COB_OPEN_INPUT) {
			f->flag_lock_rec = 0;
			f->flag_lock_rls = 0;
		}
		else {
			f->flag_lock_rec = 1;
			f->flag_lock_rls = 1;
		}
	}
	else if (f->flag_file_lock) {
		f->flag_lock_rec = 0;
		f->flag_lock_rls = 0;
	}
	else if ((f->lock_mode & COB_LOCK_AUTOMATIC)
		&& (f->open_mode != COB_OPEN_INPUT)) {
		f->flag_lock_rec = 1;
		if ((f->lock_mode & COB_LOCK_MULTIPLE)) {
			f->flag_lock_rls = 0;
		}
		else {
			f->flag_lock_rls = 1;
		}
	}
	else {
		f->flag_lock_rec = 1;
		if ((f->lock_mode & COB_LOCK_MULTIPLE)) {
			f->flag_lock_rls = 0;
		}
		else {
			f->flag_lock_rls = 1;
		}
	}

	if ((read_opts & COB_READ_IGNORE_LOCK)
		|| (f->retry_mode & COB_IGNORE_LOCK)) {
		f->flag_lock_rec = 0;
		f->flag_lock_rls = 0;
		f->flag_lock_mode = 0;
	}
	else
		if ((read_opts & COB_READ_LOCK)) {
			f->flag_lock_rec = 1;
			f->flag_lock_mode = 1;
		}
		else
			if ((read_opts & COB_READ_WAIT_LOCK)) {
				f->flag_lock_rec = 1;
				f->flag_lock_mode = 1;
			}
			else
				if ((read_opts & COB_READ_NO_LOCK)) {
					f->flag_lock_rec = 0;
					f->flag_lock_rls = 0;
				}
				else
					if ((read_opts & COB_READ_KEPT_LOCK)) {
						f->flag_lock_rec = 1;
						f->flag_lock_rls = 0;
					}

	if (f->flag_lock_rls && f->prev_lock) {
		unlock_record(f, f->prev_lock);
		f->prev_lock = 0;
	}
}

static void
save_status (cob_file *f, cob_field *fnstatus, const int status)
{
	int	k, indent = 15;
	struct stat	st;
	FILE* fo;
	char	prcoma[6];
	const char* iotype[11];
	struct cob_time tod;

	cobglobptr->cob_error_file = f;
	if (likely(status == 0)) {
		memset (f->file_status, '0', (size_t)2);
		if (fnstatus) {
			memset (fnstatus->data, '0', (size_t)2);
		}
		/* EOP is non-fatal therefore 00 status but needs exception */
		if (unlikely (eop_status)) {
			eop_status = 0;
			cob_set_exception (COB_EC_I_O_EOP);
		} else {
			cobglobptr->cob_exception_code = 0;
		}
		if (unlikely (cobsetptr->cob_do_sync)) {
			cob_sync (f);
		}
		return;
	}
	cob_set_exception (status_exception[status / 10]);
	f->file_status[0] = (unsigned char)COB_I2D (status / 10);
	f->file_status[1] = (unsigned char)COB_I2D (status % 10);
	if (fnstatus) {
		memcpy (fnstatus->data, f->file_status, (size_t)2);
	}
	if (cobsetptr->cob_line_trace
		&& f->trace_io
		&& f->last_operation > 0) {
		if (cobsetptr->cob_trace_file == NULL
			&& cobsetptr->cob_trace_filename != NULL) {
			/* Open so that I/O can be traced by itself */
			cobsetptr->cob_trace_file = fopen(cobsetptr->cob_trace_filename, "w");
			if (!cobsetptr->cob_trace_file) {
				cobsetptr->cob_trace_file = stderr;
			}
		}
		if (cobsetptr->cob_trace_file) {
			fprintf(cobsetptr->cob_trace_file, "%*s", indent - 3, "");
			switch (f->last_operation) {
			default:
				fprintf(cobsetptr->cob_trace_file, "Unknown I/O on %s Status: %.2s\n",
					f->select_name, f->file_status);
				break;
			case COB_LAST_CLOSE:
				fprintf(cobsetptr->cob_trace_file, "CLOSE %s Status: %.2s\n",
					f->select_name, f->file_status);
				break;
			case COB_LAST_OPEN:
				fprintf(cobsetptr->cob_trace_file, "OPEN %s %s -> '%s' Status: %.2s\n",
					f->open_mode == COB_OPEN_INPUT ? "INPUT" :
					f->open_mode == COB_OPEN_OUTPUT ? "OUTPUT" :
					f->open_mode == COB_OPEN_I_O ? "I_O" :
					f->open_mode == COB_OPEN_EXTEND ? "EXTEND" : "",
					f->select_name,
					file_open_name ? file_open_name : "",
					f->file_status);
				break;
			case COB_LAST_DELETE_FILE:
				fprintf(cobsetptr->cob_trace_file, "DELETE FILE %s Status: %.2s\n",
					f->select_name, f->file_status);
				break;
			case COB_LAST_READ:
				fprintf(cobsetptr->cob_trace_file, "READ %s Status: %.2s\n",
					f->select_name, f->file_status);
				if (status == 0) {
					fprintf(cobsetptr->cob_trace_file, "%*s : ", indent, "Record");
					cob_print_field(cobsetptr->cob_trace_file, f->record,
						indent + 3, cobsetptr->cob_dump_width);
				}
				if (f->last_key) {
					fprintf(cobsetptr->cob_trace_file, "%*s : ", indent,
						f->organization == COB_ORG_RELATIVE ? "Record#" : "Key");
					cob_print_field(cobsetptr->cob_trace_file, f->last_key,
						indent + 3, cobsetptr->cob_dump_width);
				}
				break;
			case COB_LAST_START:
				fprintf(cobsetptr->cob_trace_file, "START %s Status: %.2s\n",
					f->select_name, f->file_status);
				if (f->last_key) {
					fprintf(cobsetptr->cob_trace_file, "%*s : ", indent,
						f->organization == COB_ORG_RELATIVE ? "Record#" : "Key");
					cob_print_field(cobsetptr->cob_trace_file, f->last_key,
						indent + 3, cobsetptr->cob_dump_width);
				}
				break;
			case COB_LAST_READ_SEQ:
				fprintf(cobsetptr->cob_trace_file, "READ Sequential %s Status: %.2s\n",
					f->select_name, f->file_status);
				if (status == 0) {
					fprintf(cobsetptr->cob_trace_file, "%*s : ", indent, "Record");
					cob_print_field(cobsetptr->cob_trace_file, f->record,
						indent + 3, cobsetptr->cob_dump_width);
				}
				if (f->last_key
					&& f->organization == COB_ORG_RELATIVE) {
					fprintf(cobsetptr->cob_trace_file, "%*s : ", indent, "Record#");
					cob_print_field(cobsetptr->cob_trace_file, f->last_key,
						indent + 3, cobsetptr->cob_dump_width);
				}
				break;
			case COB_LAST_WRITE:
				fprintf(cobsetptr->cob_trace_file, "WRITE %s Status: %.2s\n",
					f->select_name, f->file_status);
				fprintf(cobsetptr->cob_trace_file, "%*s : ", indent, "Record");
				cob_print_field(cobsetptr->cob_trace_file, f->record,
					indent + 3, cobsetptr->cob_dump_width);
				if (f->last_key
					&& f->organization == COB_ORG_RELATIVE) {
					fprintf(cobsetptr->cob_trace_file, "%*s : ", indent, "Record#");
					cob_print_field(cobsetptr->cob_trace_file, f->last_key,
						indent + 3, cobsetptr->cob_dump_width);
				}
				break;
			case COB_LAST_REWRITE:
				fprintf(cobsetptr->cob_trace_file, "REWRITE %s Status: %.2s\n",
					f->select_name, f->file_status);
				fprintf(cobsetptr->cob_trace_file, "%*s : ", indent, "Record");
				cob_print_field(cobsetptr->cob_trace_file, f->record,
					indent + 3, cobsetptr->cob_dump_width);
				if (f->last_key
					&& f->organization == COB_ORG_RELATIVE) {
					fprintf(cobsetptr->cob_trace_file, "%*s : ", indent, "Record#");
					cob_print_field(cobsetptr->cob_trace_file, f->last_key,
						indent + 3, cobsetptr->cob_dump_width);
				}
				break;
			case COB_LAST_DELETE:
				fprintf(cobsetptr->cob_trace_file, "DELETE %s Status: %.2s\n",
					f->select_name, f->file_status);
				fprintf(cobsetptr->cob_trace_file, "%*s : ", indent, "Record");
				cob_print_field(cobsetptr->cob_trace_file, f->record,
					indent + 3, cobsetptr->cob_dump_width);
				if (f->last_key
					&& f->organization == COB_ORG_RELATIVE) {
					fprintf(cobsetptr->cob_trace_file, "%*s : ", indent, "Record#");
					cob_print_field(cobsetptr->cob_trace_file, f->last_key,
						indent + 3, cobsetptr->cob_dump_width);
				}
				break;
			}
		}
	}

	if (f->io_stats
		&& cobsetptr->cob_stats_filename
		&& f->last_operation > 0) {
		if (f->last_operation <= 6) {
			f->stats[f->last_operation - 1].rqst_io++;
			if (status != 0
				&& status != 2) {
				f->stats[f->last_operation - 1].fail_io++;
			}
		}
		if (f->last_operation == COB_LAST_CLOSE) {	/* Write stats out on FILE Close */
			fo = NULL;
			if (stat(cobsetptr->cob_stats_filename, &st) == -1) {
				iotype[COB_LAST_READ] = "READ";
				iotype[COB_LAST_WRITE] = "WRITE";
				iotype[COB_LAST_REWRITE] = "REWRITE";
				iotype[COB_LAST_DELETE] = "DELETE";
				iotype[COB_LAST_START] = "START";
				iotype[COB_LAST_READ_SEQ] = "READ_SEQ";
				fo = fopen(cobsetptr->cob_stats_filename, "w");
				if (fo) {
					fprintf(fo, "%19s,", "Time");
					fprintf(fo, "%s", " Source, FDSelect, ");
					strcpy(prcoma, "");
					for (k = 1; k <= 6; k++) {
						fprintf(fo, "%s%s", prcoma, iotype[k]);
						strcpy(prcoma, ",");
					}
					strcpy(prcoma, "");
					fprintf(fo, ", ");
					for (k = 1; k <= 6; k++) {
						fprintf(fo, "%sX%s", prcoma, iotype[k]);
						strcpy(prcoma, ",");
					}
					fprintf(fo, "\n");
					fclose(fo);
					fo = NULL;
				}
			}
			if (fo == NULL) {
				fo = fopen(cobsetptr->cob_stats_filename, "a");
			}
			if (fo) {
				tod = cob_get_current_date_and_time();
				fprintf(fo, "%04d/%02d/%02d %02d:%02d:%02d,",
					tod.year, tod.month, tod.day_of_month,
					tod.hour, tod.minute, tod.second);
				if (COB_MODULE_PTR
					&& COB_MODULE_PTR->module_source)
					fprintf(fo, "%s", COB_MODULE_PTR->module_source);
				else
					fprintf(fo, "%s", "Unknown");
				fprintf(fo, ",%s, ", f->select_name);
				strcpy(prcoma, "");
				for (k = 0; k <= 5; k++) {
					fprintf(fo, "%s%d", prcoma, f->stats[k].rqst_io);
					strcpy(prcoma, ",");
				}
				fprintf(fo, ", ");
				strcpy(prcoma, "");
				for (k = 0; k <= 5; k++) {
					fprintf(fo, "%s%d", prcoma, f->stats[k].fail_io);
					strcpy(prcoma, ",");
				}
				fprintf(fo, "\n");
				fclose(fo);
			}
			for (k = 0; k <= 5; k++) {		/* Reset counts on CLOSE */
				f->stats[k].rqst_io = 0;
				f->stats[k].fail_io = 0;
			}
		}
	}
	f->last_operation = 0;				/* Avoid double count/trace */
}

/* Regular file */

/* Translate errno status to COBOL status,
   Note: always sets either an error or the given default value */
static int
errno_cob_sts (const int default_status)
{
	switch (errno) {
#ifdef EDQUOT
	case EDQUOT:
#endif
	case ENOSPC:
		return COB_STATUS_34_BOUNDARY_VIOLATION;
	case EPERM:
	case EACCES:
	case EISDIR:
		return COB_STATUS_37_PERMISSION_DENIED;
	case ENOENT:
		return COB_STATUS_35_NOT_EXISTS;
	default:
		return default_status;
	}
}

#define COB_CHECKED_PUTC(character,fstream)	do { \
		ret = putc ((int)character, fstream); \
		if (unlikely (ret != (int)character)) { \
			return errno_cob_sts (COB_STATUS_30_PERMANENT_ERROR); \
		} \
	} ONCE_COB /* LCOV_EXCL_LINE */

#define COB_CHECKED_WRITE(fd,string,length)	do { \
		ret = write (fd, string, (size_t)length); \
		if (unlikely (ret != (size_t)length)) { \
			return errno_cob_sts (COB_STATUS_30_PERMANENT_ERROR); \
		} \
	} ONCE_COB /* LCOV_EXCL_LINE */

static size_t
file_linage_check (cob_file *f)
{
	cob_linage	*lingptr;

	lingptr = f->linorkeyptr;
	lingptr->lin_lines = cob_get_int (lingptr->linage);
	if (lingptr->lin_lines < 1) {
		goto linerr;
	}
	if (lingptr->latfoot) {
		lingptr->lin_foot = cob_get_int (lingptr->latfoot);
		if (lingptr->lin_foot < 1 ||
		    lingptr->lin_foot > lingptr->lin_lines) {
			goto linerr;
		}
	} else {
		lingptr->lin_foot = 0;
	}
	if (lingptr->lattop) {
		lingptr->lin_top = cob_get_int (lingptr->lattop);
		if (lingptr->lin_top < 0) {
			goto linerr;
		}
	} else {
		lingptr->lin_top = 0;
	}
	if (lingptr->latbot) {
		lingptr->lin_bot = cob_get_int (lingptr->latbot);
		if (lingptr->lin_bot < 0) {
			goto linerr;
		}
	} else {
		lingptr->lin_bot = 0;
	}
	return 0;
linerr:
	cob_set_int (lingptr->linage_ctr, 0);
	return 1;
}

static int
cob_linage_write_opt (cob_file *f, const int opt)
{
	cob_linage		*lingptr;
	FILE			*fp;
	int			i;
	int			n;
	int			ret;

	fp = (FILE *)f->file;
	lingptr = f->linorkeyptr;
	if (unlikely (opt & COB_WRITE_PAGE)) {
		i = cob_get_int (lingptr->linage_ctr);
		if (i == 0) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		n = lingptr->lin_lines;
		for (; i < n; ++i) {
			COB_CHECKED_PUTC ('\n', fp);
		}
		for (i = 0; i < lingptr->lin_bot; ++i) {
			COB_CHECKED_PUTC ('\n', fp);
		}
		if (file_linage_check (f)) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		for (i = 0; i < lingptr->lin_top; ++i) {
			COB_CHECKED_PUTC ('\n', fp);
		}
		cob_set_int (lingptr->linage_ctr, 1);
	} else if (opt & COB_WRITE_LINES) {
		n = cob_get_int (lingptr->linage_ctr);
		if (n == 0) {
			return COB_STATUS_57_I_O_LINAGE;
		}
		cob_add_int (lingptr->linage_ctr, opt & COB_WRITE_MASK, 0);
		i = cob_get_int (lingptr->linage_ctr);
		/* Set EOP status if requested */
		if (check_eop_status && lingptr->lin_foot) {
			if (i >= lingptr->lin_foot) {
				eop_status = 1;
			}
		}
		if (i > lingptr->lin_lines) {
			/* Set EOP status if requested */
			if (check_eop_status) {
				eop_status = 1;
			}
			for (; n < lingptr->lin_lines; ++n) {
				COB_CHECKED_PUTC ('\n', fp);
			}
			for (i = 0; i < lingptr->lin_bot; ++i) {
				COB_CHECKED_PUTC ('\n', fp);
			}
			if (file_linage_check (f)) {
				return COB_STATUS_57_I_O_LINAGE;
			}
			cob_set_int (lingptr->linage_ctr, 1);
			for (i = 0; i < lingptr->lin_top; ++i) {
				COB_CHECKED_PUTC ('\n', fp);
			}
		} else {
			for (i = (opt & COB_WRITE_MASK) - 1; i > 0; --i) {
				COB_CHECKED_PUTC ('\n', fp);
			}
		}
	}
	return 0;
}

static unsigned int
cob_seq_write_opt (cob_file *f, const int opt)
{
	int	i;
	size_t ret;

	if (opt & COB_WRITE_LINES) {
		i = opt & COB_WRITE_MASK;
		if (!i) {
			/* AFTER/BEFORE 0 */
			COB_CHECKED_WRITE (f->fd, "\r", 1);
		} else {
			for (i = opt & COB_WRITE_MASK; i > 0; --i) {
				COB_CHECKED_WRITE (f->fd, "\n", 1);
			}
		}
	} else if (opt & COB_WRITE_PAGE) {
		COB_CHECKED_WRITE (f->fd, "\f", 1);
	}
	return 0;
}

static int
cob_file_write_opt (cob_file *f, const int opt)
{
	int	i, ret;

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		return cob_linage_write_opt (f, opt);
	}
	if (opt & COB_WRITE_LINES) {
		i = opt & COB_WRITE_MASK;
		if (!i) {
			/* AFTER/BEFORE 0 */
			COB_CHECKED_PUTC ('\r', (FILE *)f->file);
		} else {
			for (; i > 0; --i) {
				COB_CHECKED_PUTC ('\n', (FILE *)f->file);
			}
		}
	} else if (opt & COB_WRITE_PAGE) {
		COB_CHECKED_PUTC ('\f', (FILE *)f->file);
	}
	return 0;
}

//kamal079 - mf merge file changes (start)
/*
 * Check if input file is Micro Focus variable length format
 *         (Refer to Micro Focus file format documentation for details)
 */
static unsigned char mfhdrmark2[4] = { 0x30,0x7E,0x00,0x00 };
static unsigned char mfhdrmark4[4] = { 0x30,0x00,0x00,0x7C };
static int
check_mf_format(cob_file* f, char* filename)
{
	FILE* fd;
	int	ln, minrcsz, maxrcsz;
	unsigned char mfhdr[128];

	fd = fopen(filename, "r");
	if (fd == NULL) {
		return 0;
	}

	memset(mfhdr, 0, sizeof(mfhdr));
	ln = fread(mfhdr, 1, sizeof(mfhdr), fd);
	minrcsz = LDCOMPX4(((unsigned char*)& mfhdr[58]));
	maxrcsz = LDCOMPX4(((unsigned char*)& mfhdr[54]));

	/* Check for file header markers and sanity checks on record size info */
	if (ln == sizeof(mfhdr)
		&& (memcmp(mfhdr, mfhdrmark2, 4) == 0 || memcmp(mfhdr, mfhdrmark4, 4) == 0)
		&& mfhdr[36] == 0x00
		&& mfhdr[37] == 0x3E
		&& (mfhdr[39] == 0x01 || mfhdr[39] == 0x03)
		&& (mfhdr[48] == 0x01 || mfhdr[48] == 0x00)
		&& minrcsz > 0
		&& maxrcsz < (60 * 1024 * 1024)
		&& minrcsz <= maxrcsz) {
		if (f->organization == COB_ORG_RELATIVE
			&& mfhdr[39] != 0x03) {
			DEBUG_LOG("io", ("File %s is not RELATIVE on disk\n", f->select_name));
		}
		else
			if (f->organization == COB_ORG_SEQUENTIAL
				&& mfhdr[39] != 0x01) {
				DEBUG_LOG("io", ("File %s is not SEQUENTIAL on disk\n", f->select_name));
			}

		if (memcmp(mfhdr, mfhdrmark4, 4) == 0) {
			f->record_prefix = 4;
		}
		else {
			f->record_prefix = 2;
		}
		if (maxrcsz > f->record_max) {
			cob_runtime_error(_("ERROR FILE %s has record size %d exceeds %d in program"),
				f->select_name, maxrcsz, (int)f->record_max);
		}
		else {
			f->record_min = minrcsz;
			f->record_max = maxrcsz;
		}
		f->file_header = 128;
		f->record_off = -1;		/* At start of file */
		f->file_format = COB_FILE_IS_MF;
		if (mfhdr[39] == 0x03) {		/* Relative format */
			f->record_slot = f->record_max + 1 + f->record_prefix;
		}
		else {
			f->record_slot = 0;	/* Unused for sequential format */
		}
	}
	else {
		fclose(fd);
		return 0;
	}
	fclose(fd);
	return 1;
}

/*
 * Write the MF style file header for variable sequential & relative files
 */
static int			/* Return -1 on error, else 0 */
write_mf_header(cob_file* f, char* filename)
{
	FILE* fd;
	int	k;
	char	wrk[16];
	unsigned char mfhdr[128];
	time_t	nowis;
	struct tm* lclNow;

	fd = fopen(filename, "w");
	if (fd == NULL) {
		return -1;
	}
	memset(mfhdr, 0, sizeof(mfhdr));
	if (f->record_max < 4096) {
		memcpy(mfhdr, mfhdrmark2, 4);
		f->record_prefix = 2;
	}
	else {
		memcpy(mfhdr, mfhdrmark4, 4);
		f->record_prefix = 4;
	}
	time(&nowis);
	lclNow = localtime(&nowis);
	strftime(wrk, sizeof(wrk), "%y%m%d%H%M%S00", lclNow);
	memcpy(&mfhdr[8], wrk, 14);
	memcpy(&mfhdr[22], wrk, 14);
	mfhdr[37] = 0x3E;
	if (f->organization == COB_ORG_RELATIVE)
		mfhdr[39] = 3;
	else
		if (f->organization == COB_ORG_SEQUENTIAL)
			mfhdr[39] = 1;
	if (f->record_min != f->record_max)
		mfhdr[48] = 1;
	else
		mfhdr[48] = 0;
	STCOMPX4(f->record_max, LSUCHAR(&mfhdr[54]));
	STCOMPX4(f->record_min, LSUCHAR(&mfhdr[58]));
	k = fwrite(mfhdr, sizeof(mfhdr), 1, fd);
	fclose(fd);
	if (k != 1)
		return -1;
	f->file_header = 128;
	f->record_off = -1;		/* At start of file */
	return 0;
}
//kamal079 - mf merge file changes (end)
static int
cob_fd_file_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	int		fd;
	int		fdmode;
	int		fperms;
	unsigned int	nonexistent;
	int		ret;

	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

	f->share_mode = sharing;

	cob_chk_file_mapping (f);

	nonexistent = 0;
	errno = 0;
	if (access (filename, F_OK) && errno == ENOENT) {
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
		nonexistent = 1;
	}

	//kamal079 - mf file merge changes (start)
	set_file_format(f);		/* Set file format */

	if ((f->organization == COB_ORG_RELATIVE || f->organization == COB_ORG_SEQUENTIAL)
		&& nonexistent == 0
		&& (mode == COB_OPEN_INPUT || mode == COB_OPEN_I_O || mode == COB_OPEN_EXTEND)) {
		if (f->file_format == COB_FILE_IS_MF
			&& f->record_min == f->record_max) {
			/* Fixed size records so No file header to check */
		}
		else
			if (f->file_format == COB_FILE_IS_MF
				&& !check_mf_format(f, filename)) {
				f->file_format = COB_FILE_IS_GCVS0;	/* Try GNU Cobol format */
				f->record_prefix = 4;
				f->file_header = 0;
				set_file_format(f);			/* Reset file format options */
			}
			else
				if (f->file_format != COB_FILE_IS_MF
					&& check_mf_format(f, filename)) {
					f->file_format = COB_FILE_IS_MF;	/* Use Micro Focus format */
				}
	}
	//kamal079 - mf file merge changes (end)

	fdmode = O_BINARY;
	fperms = 0;
	f->fd = -1;
	f->flag_file_lock = 0;

	switch (mode) {
	case COB_OPEN_INPUT:
		//kamal079 - mf file merge changes (start)
		if ((f->share_mode & COB_SHARE_NO_OTHER)
			|| (f->lock_mode & COB_FILE_EXCLUSIVE)) {
			/* fcntl with WRLCK requires file to be opened RDWR */
			fdmode |= O_RDWR;
		} else {
			fdmode |= O_RDONLY;
		}
		//kamal079 - mf file merge changes (end)
//		fdmode |= O_RDONLY;
		break;
	case COB_OPEN_OUTPUT:
		nonexistent = 0;
		fdmode |= O_CREAT | O_TRUNC;
		if (f->organization == COB_ORG_RELATIVE) {
			fdmode |= O_RDWR;
		} else {
			fdmode |= O_WRONLY;
		}
#ifdef	_WIN32
		fperms = _S_IREAD | _S_IWRITE ;
#else
		fperms = COB_FILE_MODE;
#endif
		break;
	case COB_OPEN_I_O:
		if (nonexistent) {
			fdmode |= O_CREAT | O_RDWR;
#ifdef	_WIN32
			fperms = _S_IREAD | _S_IWRITE ;
#else
			fperms = COB_FILE_MODE;
#endif
		} else {
			fdmode |= O_RDWR;
		}
		break;
	case COB_OPEN_EXTEND:
		fdmode |= O_CREAT | O_RDWR | O_APPEND;
#ifdef	_WIN32
		fperms = _S_IREAD | _S_IWRITE ;
#else
		fperms = COB_FILE_MODE;
#endif
		break;
	}

	errno = 0;
	fd = open (filename, fdmode, fperms);
	ret = errno;

	switch (ret) {
	case 0:
		if (mode == COB_OPEN_EXTEND && fd >= 0) {
			lseek (fd, (off_t) 0, SEEK_END);
		}
		f->open_mode = mode;
		break;
	case ENOENT:
		if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {

			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		return COB_STATUS_35_NOT_EXISTS;
	case EACCES:
	case EISDIR:
	case EROFS:
		return COB_STATUS_37_PERMISSION_DENIED;
	case EAGAIN:
		return COB_STATUS_61_FILE_SHARING;
	default:
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	f->fd = fd;
	//kamal079 - mf file merge changes (start)
	if ((mode == COB_OPEN_OUTPUT || (mode == COB_OPEN_I_O && nonexistent))
		&& f->file_format == COB_FILE_IS_MF) {	/* Write MF file header */
		if (f->record_min != f->record_max) {
			write_mf_header(f, filename);
			f->record_off = lseek(f->fd, (off_t)f->file_header, SEEK_SET);
		}
		else {
			f->record_prefix = 0;
		}
	}
	else
		if ((f->organization == COB_ORG_RELATIVE || f->organization == COB_ORG_SEQUENTIAL)
			&& f->file_format == COB_FILE_IS_MF
			&& (mode == COB_OPEN_INPUT || mode == COB_OPEN_I_O || mode == COB_OPEN_EXTEND)) {
			f->record_off = lseek(f->fd, (off_t)f->file_header, SEEK_SET);
		}
	if (f->access_mode == COB_ACCESS_SEQUENTIAL
		&& f->organization == COB_ORG_RELATIVE
		&& f->keys[0].field) {
		cob_set_int(f->keys[0].field, 0);
	}
	f->record_off = -1;

	if ((ret=set_file_lock(f, filename, mode)) != 0)
		return ret;

	//kamal079 - mf file merge changes (end)
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;
}

static int
cob_file_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

#ifdef	WITH_SEQRA_EXTFH
	int		ret;
	f->share_mode = sharing;
	ret = extfh_seqra_locate (f, filename);
	switch (ret) {
	case COB_NOT_CONFIGURED:
		cob_chk_file_mapping (f);
		if (access (filename, F_OK) && errno == ENOENT) {
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
				return COB_STATUS_35_NOT_EXISTS;
			}
		}
		break;
	case COB_STATUS_00_SUCCESS:
		break;
	default:
		return ret;
	}
	ret = extfh_cob_file_open (f, filename, mode, sharing);
	switch (ret) {
	case COB_STATUS_00_SUCCESS:
		f->open_mode = mode;
		break;
	case COB_STATUS_35_NOT_EXISTS:
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		break;
	}
	return ret;

#else

	FILE			*fp;
	const char		*fmode;
	cob_linage		*lingptr;
	int			ret;
	unsigned int		nonexistent;

	f->share_mode = sharing;
	if (f->organization != COB_ORG_LINE_SEQUENTIAL) {
		return cob_fd_file_open (f, filename, mode, sharing);
	}

	cob_chk_file_mapping (f);

	nonexistent = 0;
	errno = 0;
	if (access (filename, F_OK) && errno == ENOENT) {
		nonexistent = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
	}

	fp = NULL;
	fmode = NULL;
	/* Open the file */
	switch (mode) {
	case COB_OPEN_INPUT:
		if ((f->share_mode & COB_SHARE_NO_OTHER)
		 || (f->lock_mode & COB_FILE_EXCLUSIVE) ) {
			fmode = "r+";
		} else 
		if (!cobsetptr->cob_unix_lf) {
			fmode = "r";
		} else {
			fmode = "rb";
		}
		break;
	case COB_OPEN_OUTPUT:
		if (!cobsetptr->cob_unix_lf) {
			fmode = "w";
		} else {
			fmode = "wb";
		}
		break;
	case COB_OPEN_I_O:
		fmode = "r+";
		break;
		//kamal079 - mf file merge changes
		//return COB_STATUS_37_PERMISSION_DENIED;
	case COB_OPEN_EXTEND:
		/* Problem on WIN32 (tested _MSC_VER 1500 and GCC build) if file isn't there: */
		/* Both modes create the file and return a bad pointer */
		/* Mode "a+"  sets EINVAL, further actions on the file do work */
		/* Mode "ab+" doesn't set errno, but we don't want a binary file */
		/* Possible Solutions: */
		/* a) Create the file and reopen it with a+ */
		/* b) Check this stuff in EINVAL and just go on */
		if (!cobsetptr->cob_unix_lf) {
			fmode = "a+";
		} else {
			fmode = "ab+";
		}
		break;
	/* LCOV_EXCL_START */
	default:
		cob_fatal_error(COB_FERROR_CODEGEN);
	/* LCOV_EXCL_STOP */
	}

	errno = 0;
	fp = fopen (filename, fmode);
	switch (errno) {
	case 0:
		f->open_mode = mode;
		break;
	case EINVAL:
		if (f->flag_optional && nonexistent) {
			f->open_mode = mode;
		} else {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		break;
	case ENOENT:
		if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		return COB_STATUS_35_NOT_EXISTS;
	case EACCES:
	case EISDIR:
	case EROFS:
		return COB_STATUS_37_PERMISSION_DENIED;
	case EAGAIN:
		return COB_STATUS_61_FILE_SHARING;
	default:
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		if (file_linage_check (f)) {
				fclose (fp);
			return COB_STATUS_57_I_O_LINAGE;
		}
		f->flag_needs_top = 1;
		lingptr = f->linorkeyptr;
		cob_set_int (lingptr->linage_ctr, 1);
	}
	f->file = fp;
	f->fd = fileno (fp);
	set_file_format(f);		/* Set file format */

	if ((ret=set_file_lock(f, filename, mode)) != 0)
		return ret;
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;

#endif
}

static int
cob_file_close (cob_file *f, const int opt)
{
#ifdef	WITH_SEQRA_EXTFH
	return extfh_cob_file_close (f, opt);
#else

	switch (opt) {
	case COB_CLOSE_LOCK:
		/* meaning (not file-sharing related):
		   file may not be opened in *this runtime unit* again */
		/* TODO: set flag here */
		/* fall-thru */
	case COB_CLOSE_NORMAL:
	case COB_CLOSE_NO_REWIND:
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->flag_needs_nl &&
			    !(f->flag_select_features & COB_SELECT_LINAGE)) {
				f->flag_needs_nl = 0;
				putc ('\n', (FILE *)f->file);
			}
		} else if (f->flag_needs_nl) {
			f->flag_needs_nl = 0;
			if (f->fd >= 0) {
				if (write (f->fd, "\n", (size_t)1) != 1) {
			}
		}
		}
		/* Unlock the file */
		unlock_record (f, 0);

		/* Close the file */
		if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
			if (f->file) {
				fclose ((FILE *)f->file);
			}
		} else {
			if (f->fd >= 0) {
				close (f->fd);
			}
		}
		if (opt == COB_CLOSE_NO_REWIND) {
			f->open_mode = COB_OPEN_CLOSED;
			return COB_STATUS_07_SUCCESS_NO_UNIT;
		}
		return COB_STATUS_00_SUCCESS;
	default:
		if (f->fd >= 0 && f->open_mode != COB_OPEN_INPUT) {
			fdcobsync (f->fd);
		}
		return COB_STATUS_07_SUCCESS_NO_UNIT;
	}
#endif
}

/* SEQUENTIAL */

static int
sequential_read (cob_file *f, const int read_opts)
{
	int	bytesread,padlen;
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;

#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_read (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (read_opts);
#endif

	if (unlikely (f->flag_operation != 0)) {
		f->flag_operation = 0;
	}
	if(f->record_off == -1) {
		f->record_off = lseek (f->fd, (off_t)f->file_header, SEEK_SET);/* Set current file position */
	} else {
		f->record_off = lseek (f->fd, (off_t)0, SEEK_CUR);	/* Get current file position */
	}

	if (unlikely (f->record_min != f->record_max)) {
		/* Read record size */

		bytesread = read (f->fd, recsize.sbuff, f->record_prefix);
		if (unlikely (bytesread != (int)f->record_prefix)) {
			if (bytesread == 0) {
				return COB_STATUS_10_END_OF_FILE;
			} else {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
		switch (f->file_format) {
		case COB_FILE_IS_GC:
		case 0:
		case 3:
			f->record->size = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		case 1:
			f->record->size = COB_MAYSWAP_32 (recsize.sint);
			break;
		case 2:
			f->record->size = recsize.sint;
			break;
		case COB_FILE_IS_B32:		/* Was varseq 2 on Big Endian system */
			f->record->size = LDCOMPX4(recsize.sbuff);
			break;
		case COB_FILE_IS_L32:		/* Was varseq 2 on Little Endian system */
			f->record->size = LDBINLE4(recsize.sbuff);
			break;
		case COB_FILE_IS_MF:
			if(f->record_prefix == 2) {
				f->record->size = ((recsize.sbuff[0] & 0x0F) << 8) + recsize.sbuff[1];
			} else {
				f->record->size = ((recsize.sbuff[0] & 0x0F) << 24) + (recsize.sbuff[1] << 16) 
						+ (recsize.sbuff[2] << 8) + recsize.sbuff[3];
			}
			break;
		default:
			f->record->size = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		}
	}

	/* Read record */
	bytesread = read (f->fd, f->record->data, f->record->size);
	if (f->record_min != f->record_max
	&&  f->file_format == COB_FILE_IS_MF) {
		padlen = ((f->record->size + f->record_prefix + 3) / 4 * 4) - (f->record->size + f->record_prefix);
		if(padlen > 0)
			read(f->fd, recsize.sbuff, padlen);	/* Read past padding chars */
	}
	if (unlikely (bytesread != (int)f->record->size)) {
		if (bytesread == 0) {
			return COB_STATUS_10_END_OF_FILE;
		/* LCOV_EXCL_START */
		} else if (bytesread < 0) {
			return COB_STATUS_30_PERMANENT_ERROR;
		/* LCOV_EXCL_STOP */
		} else {
			return COB_STATUS_04_SUCCESS_INCOMPLETE;
		}
	}
	return COB_STATUS_00_SUCCESS;
}

static int
sequential_write (cob_file *f, const int opt)
{
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;
	int	padlen;

#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	if (unlikely (f->flag_operation == 0)) {
		f->flag_operation = 1;
	}

	/* WRITE AFTER */
	if (unlikely (opt & COB_WRITE_AFTER)) {
		if (cob_seq_write_opt (f, opt)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		f->flag_needs_nl = 1;
	}

	if(f->record_off == -1) {
		f->record_off = lseek (f->fd, (off_t)f->file_header, SEEK_SET);/* Set current file position */
	} else {
		f->record_off = lseek (f->fd, (off_t)0, SEEK_CUR);	/* Get current file position */
	}
	if (unlikely (f->record_min != f->record_max)) {
		/* Write record size */

		recsize.sint = 0;
		switch (f->file_format) {
		case 1:
			recsize.sint = COB_MAYSWAP_32 (f->record->size);
			break;
		case 2:
			recsize.sint = f->record->size;
			break;
		case COB_FILE_IS_B32:		/* Was varseq 2 on Big Endian system */
			STCOMPX4(f->record->size, recsize.sbuff);
			break;
		case COB_FILE_IS_L32:		/* Was varseq 2 on Little Endian system */
			STBINLE4(f->record->size, recsize.sbuff);
			break;
		case COB_FILE_IS_MF:
			if(f->record_prefix == 2) {
				STCOMPX2(f->record->size, recsize.sbuff);
			} else {
				STCOMPX4(f->record->size, recsize.sbuff);
			}
			recsize.sbuff[0] |= 0x40;
			break;
		default:
			recsize.sshort[0] = COB_MAYSWAP_16 (f->record->size);
			break;
		}

		if (unlikely(write (f->fd, recsize.sbuff, f->record_prefix) !=
			     (int)f->record_prefix)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	}

	/* Write record */
	if (unlikely(write (f->fd, f->record->data, f->record->size) !=
		     (int)f->record->size)) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	if (f->record_min != f->record_max
	&&  f->file_format == COB_FILE_IS_MF) {
		padlen = ((f->record->size + f->record_prefix + 3) / 4 * 4) - (f->record->size + f->record_prefix);
		while(padlen-- > 0)
			write(f->fd, " ",1);
	}

	/* WRITE BEFORE */
	if (unlikely (opt & COB_WRITE_BEFORE)) {
		if (cob_seq_write_opt (f, opt)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		f->flag_needs_nl = 0;
	}

	return COB_STATUS_00_SUCCESS;
}

static int
sequential_rewrite (cob_file *f, const int opt)
{
	union {
		unsigned char	sbuff[4];
		unsigned short	sshort[2];
		unsigned int	sint;
	} recsize;
	int	bytesread, rcsz, padlen;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_sequential_rewrite (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (opt);
#endif
	f->flag_operation = 1;
	if (f->record_off != -1) {
		if (lseek (f->fd, f->record_off, SEEK_SET) == (off_t)-1) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
	} else
	if (lseek (f->fd, -(off_t) f->record->size, SEEK_CUR) == (off_t)-1) {	/* Not used! */
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	rcsz = f->record->size;
	padlen = 0;
	if (f->record_min != f->record_max
	&& f->record_prefix > 0) {
		bytesread = read (f->fd, recsize.sbuff, f->record_prefix);
		if (unlikely (bytesread != (int)f->record_prefix)) {
			if (bytesread == 0) {
				return COB_STATUS_10_END_OF_FILE;
			} else {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
		switch (f->file_format) {
		case COB_FILE_IS_GC:
		case 0:
		case 3:
			rcsz = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		case 1:
			rcsz = COB_MAYSWAP_32 (recsize.sint);
			break;
		case 2:
			rcsz = recsize.sint;
			break;
		case COB_FILE_IS_B32:		/* Was varseq 2 on Big Endian system */
			rcsz = LDCOMPX4(recsize.sbuff);
			break;
		case COB_FILE_IS_L32:		/* Was varseq 2 on Little Endian system */
			rcsz = LDBINLE4(recsize.sbuff);
			break;
		case COB_FILE_IS_MF:
			if(f->record_prefix == 2) {
				rcsz = ((recsize.sbuff[0] & 0x0F) << 8) + recsize.sbuff[1];
			} else {
				rcsz = ((recsize.sbuff[0] & 0x0F) << 24) + (recsize.sbuff[1] << 16) 
					+ (recsize.sbuff[2] << 8) + recsize.sbuff[3];
			}
			padlen = ((rcsz + f->record_prefix + 3) / 4 * 4) - (rcsz + f->record_prefix);
			break;
		default:
			rcsz = COB_MAYSWAP_16 (recsize.sshort[0]);
			break;
		}
		if((rcsz + padlen) < f->record->size)
			return COB_STATUS_30_PERMANENT_ERROR;
	}
	if(rcsz > f->record_max)
		return COB_STATUS_30_PERMANENT_ERROR;
	if (write (f->fd, f->record->data, (size_t)rcsz) != (int)rcsz) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (f->record_min != f->record_max
	&&  f->file_format == COB_FILE_IS_MF) {
		while(padlen-- > 0)
			write(f->fd, " ",1);
	}
	return COB_STATUS_00_SUCCESS;
}

/* LINE SEQUENTIAL */

static int
lineseq_read (cob_file *f, const int read_opts)
{
	unsigned char	*dataptr;
	size_t		i = 0;
	int		n, k;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_sequential_read (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED (read_opts);
#endif

	dataptr = f->record->data;
	f->record_off = ftell ((FILE *)f->file);	/* Save file position at start of line */
	for (; ;) {
		n = getc ((FILE *)f->file);
		if (unlikely (n == EOF)) {
			if (!i) {
				return COB_STATUS_10_END_OF_FILE;
			} else {
				break;
			}
		}
		if (unlikely(n == 0)
		&& (f->file_features & COB_FILE_LS_NULLS)) {
			n = getc ((FILE *)f->file);
			/* LCOV_EXCL_START */
			if (n == EOF) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			if ((f->file_features & COB_FILE_LS_VALIDATE)
			&& (unsigned char)n >= ' ') {		/* Should be less than a space */
				return COB_STATUS_34_BOUNDARY_VIOLATION;
			}
		} else {
			if (n == '\r') {
				continue;
			}
			if (n == '\n') {
				break;
			}
		}
		if (likely(i < f->record_max)) {
			*dataptr++ = (unsigned char)n;
			i++;
			if (i >= f->record_max
			 && (f->file_features & COB_FILE_LS_SPLIT)) {
				/* If record is too long, then simulate end
				 * so balance becomes the next record read */
				n = getc ((FILE *)f->file);
				k = 1;
				if (n == '\r') {
					n = getc ((FILE *)f->file);
					k++;
				}
				if (n != '\n') {
					fseek((FILE*)f->file, -k, SEEK_CUR);
				}
				break;
			}
		}
	}
	if (i < f->record_max) {
		/* Fill the record with spaces */
		memset ((unsigned char *)f->record->data + i, ' ',
			f->record_max - i);
	}
	f->record->size = i;
	return COB_STATUS_00_SUCCESS;
}

static int
lineseq_write (cob_file *f, const int opt)
{
	unsigned char		*p;
	cob_linage		*lingptr;
	size_t			size;
	int			i,j;
	int			ret;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_sequential_write (f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	/* Determine the size to be written */
	if ((f->file_features & COB_FILE_LS_FIXED)) {
		size = f->record->size;
	} else {
		for (i = (int)f->record->size - 1; i >= 0; --i) {
			if (f->record->data[i] != ' ') {
				break;
			}
		}
		size = i + 1;
	}

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		if (f->flag_needs_top) {
			f->flag_needs_top = 0;
			lingptr = f->linorkeyptr;
			for (i = 0; i < lingptr->lin_top; ++i) {
				COB_CHECKED_PUTC ('\n', (FILE *)f->file);
			}
		}
	}
	/* WRITE AFTER */
	if (opt & COB_WRITE_AFTER) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 1;
	}

	f->record_off = ftell ((FILE *)f->file);	/* Save file position at start of line */
	/* Write to the file */
	if (size) {
		if ((f->file_features & COB_FILE_LS_NULLS)) {
			p = f->record->data;
			for(i=j=0; j < (int)size; j++) {
				if(p[j] < ' ') {
					if(j-i > 0) {
						if(fwrite(&p[i],j-i,1,(FILE*)f->file) <= 0)
							return COB_STATUS_30_PERMANENT_ERROR;
					}
					i = j + 1;
					fputc(0x00, (FILE*)f->file);
					fputc(p[j], (FILE*)f->file);
				}
			}
			if(i < (int)size) {
				if(fwrite(&p[i],(int)size-i,1,(FILE*)f->file) <= 0)
					return COB_STATUS_30_PERMANENT_ERROR;
			}
		} else {
			if ((f->file_features & COB_FILE_LS_VALIDATE)) {
				p = f->record->data;
				for (i = 0; i < (int)size; ++i, ++p) {
					if (*p < ' '
					&&  *p != COB_CHAR_BS 
					&&  *p != COB_CHAR_ESC 
					&&  *p != COB_CHAR_FF 
					&&  *p != COB_CHAR_SI 
					&&  *p != COB_CHAR_TAB) 
						return COB_STATUS_34_BOUNDARY_VIOLATION;
		}
	}
			if (unlikely(fwrite (f->record->data, size, (size_t)1,
				     (FILE *)f->file) != 1)) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	if (unlikely (f->flag_select_features & COB_SELECT_LINAGE)) {
		putc ('\n', (FILE *)f->file);
	} else if (cobsetptr->cob_ls_uses_cr) {
		if ((opt & COB_WRITE_PAGE)
		 || (opt & COB_WRITE_BEFORE && f->flag_needs_nl)) {
			COB_CHECKED_PUTC ('\r', (FILE *)f->file);
		} else if ((opt == 0) ) {
			putc ('\r', (FILE *)f->file);
		}
	}

	if ((opt == 0) 
	&& !(f->flag_select_features & COB_SELECT_LINAGE)
	&& ((f->file_features & COB_FILE_LS_LF)
	 || (f->file_features & COB_FILE_LS_CRLF))) {	/* At least add 1 LF */
		putc ('\n', (FILE *)f->file);
		f->flag_needs_nl = 0;
	}

	/* WRITE BEFORE */
	if (opt & COB_WRITE_BEFORE) {
		ret = cob_file_write_opt (f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 0;
	}

	return COB_STATUS_00_SUCCESS;
}
static int
lineseq_rewrite(cob_file* f, const int opt)
{
	unsigned char* p;
	size_t			size, psize;
	int			i, j, slotlen;
	int			ret;
	off_t			curroff;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_sequential_rewrite(f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	curroff = ftell((FILE*)f->file);	/* Current file position */
	/* Determine the size to be written */
	if ((f->file_features & COB_FILE_LS_FIXED)) {
		size = f->record->size;
	}
	else {
		for (i = (int)f->record->size - 1; i >= 0; --i) {
			if (f->record->data[i] != ' ') {
				break;
			}
		}
		size = i + 1;
	}

	p = f->record->data;
	psize = size;
	if ((f->file_features & COB_FILE_LS_NULLS)) {
		for (j = 0; j < (int)size; j++) {
			if (p[j] < ' ') {
				psize++;
			}
		}
	}
	slotlen = curroff - f->record_off - 1;

	if (psize > slotlen)
		return COB_STATUS_44_RECORD_OVERFLOW;

	if (fseek((FILE*)f->file, (off_t)f->record_off, SEEK_SET) != 0)
		return COB_STATUS_30_PERMANENT_ERROR;

	/* Write to the file */
	if (size) {
		if ((f->file_features & COB_FILE_LS_NULLS)) {
			p = f->record->data;
			for (i = j = 0; j < (int)size; j++) {
				if (p[j] < ' ') {
					if (j - i > 0) {
						if (fwrite(&p[i], j - i, 1, (FILE*)f->file) <= 0)
							return COB_STATUS_30_PERMANENT_ERROR;
					}
					i = j + 1;
					fputc(0x00, (FILE*)f->file);
					fputc(p[j], (FILE*)f->file);
				}
			}
			if (i < (int)size) {
				if (fwrite(&p[i], (int)size - i, 1, (FILE*)f->file) <= 0)
					return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
		else {
			if ((f->file_features & COB_FILE_LS_VALIDATE)) {
				p = f->record->data;
				for (i = 0; i < (int)size; ++i, ++p) {
					if (*p < ' ')
						return COB_STATUS_34_BOUNDARY_VIOLATION;
				}
			}
			if (unlikely(fwrite(f->record->data, size, (size_t)1,
				(FILE*)f->file) != 1)) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
		for (i = psize; i < slotlen; i++)		/* In case new record was shorter, pad with spaces */
			fputc(' ', (FILE*)f->file);
	}

	if (unlikely(f->flag_select_features & COB_SELECT_LINAGE)) {
		putc('\n', (FILE*)f->file);
	}
	else
		if ((f->file_features & COB_FILE_LS_CRLF)) {
			if (opt & COB_WRITE_PAGE) {
				putc('\r', (FILE*)f->file);
			}
			else if ((opt & COB_WRITE_BEFORE) && f->flag_needs_nl) {
				putc('\r', (FILE*)f->file);
			}
		}
		else {
			putc('\n', (FILE*)f->file);
		}

	/* WRITE BEFORE */
	if (opt & COB_WRITE_BEFORE) {
		ret = cob_file_write_opt(f, opt);
		if (ret) {
			return ret;
		}
		f->flag_needs_nl = 0;
	}

	return COB_STATUS_00_SUCCESS;
}

/* RELATIVE */
/*
 * Return size of relative record at given offset
 */
static int
relative_read_size(cob_file* f, off_t off, int* isdeleted)
{
	int	relsize = 0;
	unsigned char rechdr[8];

	*isdeleted = 0;
	if (lseek(f->fd, off, SEEK_SET) == (off_t)-1) {
		return -1;
	}
	if (f->record_prefix > 0) {
		memset(rechdr, 0, sizeof(rechdr));
		if (read(f->fd, rechdr, f->record_prefix) != f->record_prefix) {
			return -1;
		}
		switch (f->file_format) {
		case COB_FILE_IS_B32:		/* Was 32bit Big Endian system */
			relsize = LDCOMPX4(rechdr);
			break;
		case COB_FILE_IS_B64:		/* Was 64bit Big Endian system */
			relsize = LDCOMPX4(((unsigned char*)& rechdr[4]));
			break;
		case COB_FILE_IS_L32:		/* Was 32bit Little Endian system */
			relsize = LDBINLE4(rechdr);
			break;
		case COB_FILE_IS_L64:		/* Was 64bit Little Endian system */
			relsize = LDBINLE4(rechdr);
			break;
		case COB_FILE_IS_MF:
			if (f->record_prefix == 2) {
				relsize = ((rechdr[0] & 0x0F) << 8) + rechdr[1];
			}
			else {
				relsize = ((rechdr[0] & 0x0F) << 24) + (rechdr[1] << 16)
					+ (rechdr[2] << 8) + rechdr[3];
			}
			if ((rechdr[0] & 0x20)) {
				relsize = 0;	/* Deleted record */
			}
			break;
		default:
			memcpy(&relsize, rechdr, sizeof(relsize));	/* Local native 'size_t' */
			break;
		}
		if (relsize <= 0)
			* isdeleted = 1;
		return (int)relsize;
	}
	else
		if (f->file_format == COB_FILE_IS_MF) {
			if (lseek(f->fd, off + f->record_slot - 1, SEEK_SET) == (off_t)-1) {
				return -1;
			}
			rechdr[0] = 0;
			read(f->fd, rechdr, 1);	/* 0x00 means deleted record */
			lseek(f->fd, off, SEEK_SET);
			if (rechdr[0] == 0) {
				*isdeleted = 1;
				return 0;
			}
			else {
				return (int)f->record_max;
			}
		}
	return 0;
}



/* RELATIVE */

static int
relative_start (cob_file *f, const int cond, cob_field *k)
{
	off_t		off;
	size_t		relsize;
	int		kindex;
	int		ksindex;
	int		kcond, isdeleted;
	struct stat	st;

#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_start (f, cond, k);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	if (fstat (f->fd, &st) != 0 || st.st_size == 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	/* Get the index */
	f->flag_first_read = 0;
	switch (cond) {
	case COB_FI:
		kcond = COB_GE;
		kindex = 0;
		f->flag_first_read = 1;
		break;
	case COB_LA:
		kcond = COB_LE;
		kindex = (st.st_size - f->file_header) / f->record_slot - 1;
		break;
	case COB_LT:
	case COB_LE:
		kcond = cond;
		kindex = cob_get_int (k) - 1;
		/* Check against current file size */
		ksindex = (st.st_size - f->file_header) / f->record_slot - 1;
		if (kindex > ksindex) {
			kindex = ksindex + 1;
		}
		break;
	default:
		kcond = cond;
		kindex = cob_get_int (k) - 1;
		break;
	}

	if (kindex < 0) {
		/* Only valid ops are GE and GT in this case */
		switch (kcond) {
		case COB_GE:
			kindex = 0;
			break;
		case COB_GT:
			/* Set to cater for increment below */
			kindex = -1;
			break;
		default:
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	}

	if (kcond == COB_LT) {
		kindex--;
		if (kindex < 0) {
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	} else if (kcond == COB_GT) {
		kindex++;
	}

	f->flag_operation = 0;

	/* Seek index */
	for (;;) {
		if (kindex < 0) {
			break;
		}
		off = kindex * f->record_slot + f->file_header;
		if (off >= st.st_size) {
			break;
		}
		relsize = relative_read_size(f, off, &isdeleted);

		/* Check if a valid record */
		if (relsize > 0 && !isdeleted) {
			f->record_off = off;
			lseek (f->fd, off, SEEK_SET);	/* Set file position to start of record */
			if (f->access_mode == COB_ACCESS_SEQUENTIAL
			&&  f->keys[0].field) {
				kindex = (int)(((off - f->file_header) / f->record_slot) + 1);
				cob_set_int (f->keys[0].field, kindex);
			}
			return COB_STATUS_00_SUCCESS;
		}

		switch (kcond) {
		case COB_EQ:
			return COB_STATUS_23_KEY_NOT_EXISTS;
		case COB_LT:
		case COB_LE:
			kindex--;
			break;
		case COB_GT:
		case COB_GE:
			kindex++;
			break;
		}
	}
	return COB_STATUS_23_KEY_NOT_EXISTS;
}

/*
 * Read relative record at given offset
 */
static int
relative_read_off (cob_file *f, off_t off)
{
	unsigned char recmark[2];
	int	relsize = 0;
	int	relnum,isdeleted=0;

	relsize = relative_read_size(f, off, &isdeleted);
	if(relsize < 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	if (relsize == 0 || isdeleted) {
		f->record->size = 0;
		lseek (f->fd, off, SEEK_SET);
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	if (relsize > f->record_max) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}

	if (read (f->fd, f->record->data, relsize) != (int)relsize) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	f->record->size = relsize;
	f->record_off = off;

	if (f->keys[0].field) {
		relnum = (int)(((off - f->file_header) / f->record_slot) + 1);
		cob_set_int (f->keys[0].field, 0);
		if (cob_add_int (f->keys[0].field, relnum, COB_STORE_KEEP_ON_OVERFLOW) != 0) {
			lseek (f->fd, off, SEEK_SET);
			return COB_STATUS_14_OUT_OF_KEY_RANGE;
		}
	}
	if (f->file_format == COB_FILE_IS_MF) {
		if(f->record_min != f->record_max) {
			lseek (f->fd, off + f->record_slot - 1, SEEK_SET);
		}
		read (f->fd, recmark, 1);	/* Active Record marker */
		if (recmark[0] == 0x00) {	/* Flagged Deleted */
			f->record->size = 0;
			lseek (f->fd, off, SEEK_SET);
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	}
	return COB_STATUS_00_SUCCESS;
}
static int
relative_read (cob_file *f, cob_field *k, const int read_opts)
{
	off_t	off;
	int	relnum,errsts;
	struct stat	st;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_read (f, k, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	if (unlikely (f->flag_operation != 0)) {
		f->flag_operation = 0;
	}

	relnum = cob_get_int (k) - 1;
	if (relnum < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	off = relnum * f->record_slot + f->file_header;

	if (fstat (f->fd, &st) != 0 || st.st_size == 0) {
		return COB_STATUS_10_END_OF_FILE;
	}
	if(off >= st.st_size)
		return COB_STATUS_23_KEY_NOT_EXISTS;
	set_lock_opts (f, read_opts);
	if(f->flag_lock_rec) {
		lock_record (f, relnum+1, f->flag_lock_mode, &errsts);
		if (errsts != 0) {
			switch (errsts) {
			case EACCES:
			case EAGAIN:
				return COB_STATUS_51_RECORD_LOCKED;
			case EDEADLK:
				return COB_STATUS_52_DEAD_LOCK;
			case ENOLCK:
				return COB_STATUS_53_MAX_LOCKS;
			default:
		return COB_STATUS_30_PERMANENT_ERROR;
	}
		}
	}
	return relative_read_off(f, off);
}

static int
relative_read_next (cob_file *f, const int read_opts)
{
	off_t		curroff;
	int		sts;
	cob_u32_t	moveback;
	struct stat	st;
	int		relnum,errsts;

#ifdef	WITH_SEQRA_EXTFH
	int		extfh_ret;

	extfh_ret = extfh_relative_read_next (f, read_opts);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	if (fstat (f->fd, &st) != 0 || st.st_size == 0) {
		return COB_STATUS_10_END_OF_FILE;
	}
	/* LCOV_EXCL_START */
	if (st.st_size < f->record_slot) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	/* LCOV_EXCL_STOP */

	if(f->record_off == -1) {
		curroff = lseek (f->fd, (off_t)f->file_header, SEEK_SET);	/* Set current file position */
	} else {
		curroff = lseek (f->fd, (off_t)0, SEEK_CUR);	/* Get current file position */
	}
	if (unlikely(f->flag_operation != 0)) {
		f->flag_operation = 0;
	}
	moveback = 0;

	switch (read_opts & COB_READ_MASK) {
	case COB_READ_FIRST:
		curroff = f->file_header;
		break;
	case COB_READ_LAST:
		curroff = st.st_size - f->record_slot;
		moveback = 1;
		break;
	case COB_READ_PREVIOUS:
		if (f->flag_first_read) {
			break;
		} else if (curroff > (f->record_slot + f->file_header)) {
			curroff -= (f->record_slot * 2);
		} else {
			return COB_STATUS_10_END_OF_FILE;
		}
		moveback = 1;
		break;
	case COB_READ_NEXT:
	default:
		break;
	}

	for (;;) {
		if(st.st_size <= curroff)
				break;
		set_lock_opts (f, read_opts);
		if(f->flag_lock_rec) {
			relnum = ((curroff - f->file_header) / f->record_slot) + 1;
			lock_record (f, relnum, f->flag_lock_mode, &errsts);
			if (errsts != 0) {
				switch (errsts) {
				case EACCES:
				case EAGAIN:
					if ((f->retry_mode & COB_ADVANCING_LOCK)
					 || (read_opts & COB_READ_ADVANCING_LOCK))
						goto next_record;
					return COB_STATUS_51_RECORD_LOCKED;
				case EDEADLK:
					if ((f->retry_mode & COB_ADVANCING_LOCK)
					 || (read_opts & COB_READ_ADVANCING_LOCK))
						goto next_record;
					return COB_STATUS_52_DEAD_LOCK;
				case ENOLCK:
					return COB_STATUS_53_MAX_LOCKS;
				default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			}
			}
		sts = relative_read_off (f, curroff);

		if (sts == COB_STATUS_00_SUCCESS) {
			lseek (f->fd, (off_t)curroff + f->record_slot, SEEK_SET);
			return COB_STATUS_00_SUCCESS;
		}
		if (sts == COB_STATUS_30_PERMANENT_ERROR
		||  sts == COB_STATUS_10_END_OF_FILE
		||  sts == COB_STATUS_14_OUT_OF_KEY_RANGE) {
			return sts;
		}
next_record:
		if (moveback) {
			if (curroff > (f->record_slot + f->file_header)) {
				curroff -= (f->record_slot * 2);
			} else {
				break;
			}
		} else {
			curroff += f->record_slot;
		}
	}
	return COB_STATUS_10_END_OF_FILE;
}

/*
 * Write Relative record prefix
 */
static int
relative_write_size(cob_file* f, off_t off, int recsize)
{
	size_t	relsize = 0;
	unsigned char rechdr[8];

	if (lseek(f->fd, off, SEEK_SET) == (off_t)-1) {
		return -1;
	}
	f->record_off = off;
	if (f->record_prefix > 0) {
		memset(rechdr, 0, sizeof(rechdr));
		switch (f->file_format) {
		case COB_FILE_IS_B32:		/* Was 32bit Big Endian system */
			STCOMPX4(recsize, rechdr);
			break;
		case COB_FILE_IS_B64:		/* Was 64bit Big Endian system */
			STCOMPX4(recsize, ((unsigned char*)& rechdr[4]));
			break;
		case COB_FILE_IS_L32:		/* Was 32bit Little Endian system */
			STBINLE4(recsize, rechdr);
			break;
		case COB_FILE_IS_L64:		/* Was 64bit Little Endian system */
			STBINLE4(recsize, rechdr);
			break;
		case COB_FILE_IS_MF:
			if (f->record_prefix == 2) {
				STCOMPX2(recsize, rechdr);
			} else {
				STCOMPX4(recsize, rechdr);
			}
			rechdr[0] |= 0x40;
			break;
		default:
			relsize = recsize;
			memcpy(rechdr, &relsize, sizeof(relsize));	/* Local native 'size_t' */
			break;
		}
		if (write(f->fd, rechdr, f->record_prefix) != f->record_prefix) {
			return -1;
		}
	}
	return recsize;
}


static void
relative_padout(cob_file* f, char pad, int len)
{
	unsigned char wrk[32];
	memset(wrk, pad, sizeof(wrk));
	while (len > sizeof(wrk)) {
		write(f->fd, wrk, sizeof(wrk));	/* Pad out record on disk */
		len -= sizeof(wrk);
	}
	if (len > 0)
		write(f->fd, wrk, len);
}


static int
relative_write (cob_file *f, const int opt)
{
	off_t	off;
	int	relsize;
	int	i, isdeleted = 0;
	int	kindex, rcsz;
	struct stat	st;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_write(f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#else
	COB_UNUSED(opt);
#endif

	rcsz = (int)f->record->size;
	if (unlikely(f->flag_operation == 0)) {
		f->flag_operation = 1;
	}

	f->last_key = f->keys[0].field;
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		kindex = cob_get_int(f->keys[0].field) - 1;
		if (kindex < 0) {
			return COB_STATUS_24_KEY_BOUNDARY;
		}
		off = (off_t)(f->file_header + f->record_slot * kindex);
		if (fstat(f->fd, &st) != 0) {
			return COB_STATUS_10_END_OF_FILE;
		}
		if (off < st.st_size) {
			relsize = relative_read_size(f, off, &isdeleted);
			if ((long)relsize < 0)
				return COB_STATUS_30_PERMANENT_ERROR;
			if ((long)relsize > 0) {
				return COB_STATUS_22_KEY_EXISTS;
			}
		} else {
			off = lseek(f->fd, off, SEEK_SET);	/* Set current file position */
		}
	} else {
		if (f->record_off == -1) {
			off = lseek(f->fd, (off_t)f->file_header, SEEK_SET);	/* Set current file position */
		} else {
			off = lseek(f->fd, (off_t)0, SEEK_CUR);	/* Get current file position */
		}
	}

	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int(f->variable_record);
		if (unlikely(f->record->size > rcsz)) {
			f->record->size = rcsz;
		}
	} else {
		f->record->size = rcsz;
	}

	relsize = relative_write_size(f, off, f->record->size);
	if (relsize < 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (write(f->fd, f->record->data, f->record->size) != (int)f->record->size) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	relative_padout(f, ' ', f->record_max - f->record->size); /* Pad out with SPACES */

	if (f->file_format == COB_FILE_IS_MF) {
		if ((f->file_features & COB_FILE_LS_CRLF)) {	/* Windows format */
			write(f->fd, "\r", 1);
		}
		write(f->fd, "\n", 1);
	}

	/* Update RELATIVE KEY */
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (f->keys[0].field) {
			i = (int)((off + f->record_slot - f->file_header) / f->record_slot);
			cob_set_int(f->keys[0].field, i);
		}
	}

	return COB_STATUS_00_SUCCESS;
}

static int
relative_rewrite (cob_file *f, const int opt)
{
	off_t	off;
	int	relsize;
	int	relnum, isdeleted = 0, errsts;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_rewrite(f, opt);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	f->flag_operation = 1;
	f->last_key = f->keys[0].field;
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		off = (off_t)f->record_off;
		relnum = (off - f->file_header) / f->record_slot;
	} else {
		relnum = cob_get_int(f->keys[0].field) - 1;
		if (relnum < 0) {
			return COB_STATUS_24_KEY_BOUNDARY;
		}
		off = f->file_header + relnum * f->record_slot;
	}
	relsize = relative_read_size(f, off, &isdeleted);
	if (relsize < 0)
		return COB_STATUS_30_PERMANENT_ERROR;
	if (relsize == 0 || isdeleted) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	set_lock_opts(f, opt);
	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int(f->variable_record);
		if (unlikely(f->record->size > f->record_max)) {
			f->record->size = f->record_max;
		}
	}
	relsize = relative_write_size(f, off, f->record->size);
	if (relsize < 0) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	if (f->flag_record_lock) {
		lock_record(f, relnum + 1, 1, &errsts);
		if (errsts != 0) {
			switch (errsts) {
			case EACCES:
			case EAGAIN:
				return COB_STATUS_51_RECORD_LOCKED;
			case EDEADLK:
				return COB_STATUS_52_DEAD_LOCK;
			case ENOLCK:
				return COB_STATUS_53_MAX_LOCKS;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}
	if (write(f->fd, f->record->data, f->record->size) != (int)f->record->size) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	relative_padout(f, ' ', f->record_max - f->record->size); /* Pad out with SPACES */

	if (f->file_format == COB_FILE_IS_MF) {
		if (f->record_min == f->record_max) {	/* Fixed size */
			write(f->fd, "\n", 1);
		} else {
			lseek(f->fd, (off_t)off + f->record_slot, SEEK_SET);
		}
	}
	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		f->record_off = lseek(f->fd, (off_t)0, SEEK_CUR);	/* Save current file position */
	}
	if (f->flag_record_lock) {
		if ((f->lock_mode & COB_LOCK_MULTIPLE)) {
			if ((opt & COB_WRITE_NO_LOCK)) {
				unlock_record(f, relnum + 1);
			}
		} else {
			if (!(opt & COB_WRITE_LOCK)) {
				unlock_record(f, relnum + 1);
			}
		}
	}
	return COB_STATUS_00_SUCCESS;
}

static int
relative_delete (cob_file *f)
{
	off_t	off;
	int	relsize;
	unsigned char rechdr[8];
	int	relnum, isdeleted, errsts;
#ifdef	WITH_SEQRA_EXTFH
	int	extfh_ret;

	extfh_ret = extfh_relative_delete(f);
	if (extfh_ret != COB_NOT_CONFIGURED) {
		return extfh_ret;
	}
#endif

	f->flag_operation = 1;
	relnum = cob_get_int(f->keys[0].field) - 1;
	if (relnum < 0) {
		return COB_STATUS_24_KEY_BOUNDARY;
	}
	off = f->file_header + relnum * f->record_slot;
	relsize = relative_read_size(f, off, &isdeleted);
	if (relsize < 0)
		return COB_STATUS_30_PERMANENT_ERROR;
	if (relsize == 0 || isdeleted) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	set_lock_opts(f, 0);
	if (lseek(f->fd, off, SEEK_SET) == (off_t)-1) {
		return -1;
	}
	f->record_off = off;
	if (f->flag_record_lock) {
		lock_record(f, relnum + 1, 1, &errsts);
		if (errsts != 0) {
			switch (errsts) {
			case EACCES:
			case EAGAIN:
				return COB_STATUS_51_RECORD_LOCKED;
			case EDEADLK:
				return COB_STATUS_52_DEAD_LOCK;
			case ENOLCK:
				return COB_STATUS_53_MAX_LOCKS;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}
	if (f->record_prefix > 0) {
		memset(rechdr, 0, sizeof(rechdr));
		switch (f->file_format) {
		case COB_FILE_IS_B32:		/* Was 32bit Big Endian system */
			STCOMPX4(0, rechdr);
			break;
		case COB_FILE_IS_B64:		/* Was 64bit Big Endian system */
			STCOMPX4(0, ((unsigned char*)& rechdr[4]));
			break;
		case COB_FILE_IS_L32:		/* Was 32bit Little Endian system */
			STBINLE4(0, rechdr);
			break;
		case COB_FILE_IS_L64:		/* Was 64bit Little Endian system */
			STBINLE4(0, rechdr);
			break;
		case COB_FILE_IS_MF:
			if (f->record_prefix == 2) {
				STCOMPX2(relsize, rechdr);
			} else {
				STCOMPX4(relsize, rechdr);
			}
			rechdr[0] |= 0x20;
			break;
		default:
			relsize = 0;
			memcpy(rechdr, &relsize, sizeof(relsize));	/* Local native 'size_t' */
			break;
		}
		if (write(f->fd, rechdr, f->record_prefix) != f->record_prefix) {
			return -1;
		}
		if (f->file_format == COB_FILE_IS_MF) {
			if (lseek(f->fd, off + f->record_slot - 1, SEEK_SET) == (off_t)-1) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			rechdr[0] = 0;
			write(f->fd, rechdr, 1);	/* 0x00 means deleted record */
		}
	} else
		if (f->file_format == COB_FILE_IS_MF) {
			if (lseek(f->fd, off + f->record_max, SEEK_SET) == (off_t)-1) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			rechdr[0] = 0;
			write(f->fd, rechdr, 1);	/* 0x00 means deleted record */
		}
	lseek(f->fd, (off_t)f->record_off, SEEK_SET);
	if (f->flag_record_lock) {
		unlock_record(f, relnum + 1);
	}
	return COB_STATUS_00_SUCCESS;
}

/* INDEXED */

#ifdef	WITH_ANY_ISAM

/* Translate ISAM status to COBOL status */
static int
fisretsts (const int default_status)
{
	switch (ISERRNO) {
	case 0:
		return COB_STATUS_00_SUCCESS;
	case ENOREC:
		return COB_STATUS_23_KEY_NOT_EXISTS;
	case EENDFILE:
		if (default_status != COB_STATUS_23_KEY_NOT_EXISTS) {
			return COB_STATUS_10_END_OF_FILE;
		}
		break;
	case EDUPL:
	case EKEXISTS:
		return COB_STATUS_22_KEY_EXISTS;
	case EPERM:
	case EACCES:
	case EISDIR:
		return COB_STATUS_37_PERMISSION_DENIED;
	case ENOENT:
		return COB_STATUS_35_NOT_EXISTS;
	case EBADFILE:
		return COB_STATUS_30_PERMANENT_ERROR;
	case ELOCKED:
		return COB_STATUS_51_RECORD_LOCKED;
	case EDEADLK:
		return COB_STATUS_52_DEAD_LOCK;
	case ENOLCK:
		return COB_STATUS_53_MAX_LOCKS;
	case EFLOCKED:
		return COB_STATUS_61_FILE_SHARING;
	case ENOCURR:
		if (default_status != COB_STATUS_10_END_OF_FILE) {
			return COB_STATUS_21_KEY_INVALID;
		}
		break;
	default:
		break;
	}
	return default_status;
}

/* Free memory for indexfile packet */

static void
freefh (struct indexfile *fh)
{
	if (fh == NULL) {
		return;
	}
	if (fh->filename) {
		cob_free ((void *)fh->filename);
	}
	if (fh->savekey) {
		cob_free ((void *)fh->savekey);
	}
	if (fh->recwrk) {
		cob_free ((void *)fh->recwrk);
	}
	cob_free ((void *)fh);
}

/* Restore ISAM file positioning */
static void
restorefileposition (cob_file *f)
{
	struct indexfile	*fh;
	struct keydesc		k0;

	fh = f->file;
	memset ((void *)&k0, 0, sizeof (k0));
	if (fh->saverecnum >= 0) {
		/* Switch back to index */
		ISRECNUM = fh->saverecnum;
		/* Switch to recnum mode */
		isstart (fh->isfd, &k0, 0, (void *)fh->recwrk, ISEQUAL);
		/* Read by record number */
		isread (fh->isfd, (void *)fh->recwrk, ISEQUAL);
		isstart (fh->isfd, &fh->key[fh->curkey], 0,
			 (void *)fh->recwrk, ISEQUAL);
		isread (fh->isfd, (void *)fh->recwrk, ISEQUAL);
		while (ISRECNUM != fh->saverecnum) {
			/* Read back into position */
			if (isread (fh->isfd, (void *)fh->recwrk, fh->readdir)) {
				break;
			}
		}
		if (ISRECNUM == fh->saverecnum) {
			if (fh->readdir == ISNEXT) {
				/* Back off by one so next read gets this */
				isread (fh->isfd, (void *)fh->recwrk, ISPREV);
			} else {
				isread (fh->isfd, (void *)fh->recwrk, ISNEXT);
			}
		}
	} else if (fh->readdone && fh->curkey == 0) {
		indexed_restorekey(fh, NULL, 0);
		isstart (fh->isfd, &fh->key[fh->curkey], 0,
			 (void *)fh->recwrk, ISGTEQ);
	}
}

/* Save ISAM file positioning information for later 'restorefileposition' */

static void
savefileposition (cob_file *f)
{
	struct indexfile	*fh;

	fh = f->file;
	if (fh->curkey >= 0 && fh->readdir != -1) {
		/* Switch back to index */
		if (fh->wrkhasrec != fh->readdir) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			/* Read next record in file */
			if (isread (fh->isfd, (void *)fh->recwrk, fh->readdir)) {
				fh->saverecnum = -1;
				fh->saveerrno = ISERRNO;
				if (fh->saveerrno == EENDFILE ||
				    fh->saveerrno == ENOREC)  {
					fh->eofpending = fh->readdir;
				}
			} else {
				fh->saverecnum = ISRECNUM;
				fh->saveerrno = 0;
			}
			/* Restore saved record data */
			memcpy (fh->recwrk, f->record->data, f->record_max);
		}
	} else {
		fh->saverecnum = -1;
	}
}

/*
 * Open ISAM File, if locked retry as requested
 */
static int
isopen_retry(cob_file *f, char *filename, int mode)
{
	int	isfd, retry, interval;

	retry = interval = 0;
	if ((f->retry_mode & COB_RETRY_FOREVER)) {
		retry = -1;
	} else
	if ((f->retry_mode & COB_RETRY_SECONDS)) {
		retry = 1;
		interval = f->retry_seconds>0?f->retry_seconds:
			(cobsetptr->cob_retry_seconds>0?cobsetptr->cob_retry_seconds:1);
	} else
	if ((f->retry_mode & COB_RETRY_TIMES)) {
		retry = f->retry_times>0?f->retry_times:
			(cobsetptr->cob_retry_times>0?cobsetptr->cob_retry_times:1);
		interval = cobsetptr->cob_retry_seconds>0?cobsetptr->cob_retry_seconds:1;
	}
	if(retry > 0) {
		retry = retry * interval * COB_RETRY_PER_SECOND ;
		interval = 1000 / COB_RETRY_PER_SECOND ;
	}
	isfd = isopen ((void *)filename, mode);
	while(isfd < 0 && retry != 0) {
		if (ISERRNO != EFLOCKED) 
			break;
		if(retry > 0) {
			retry--;
			cob_sys_sleep_msec(interval);
		}
		isfd = isopen ((void *)filename, mode);
	}
	if (isfd >= 0
	 && (mode & ISEXCLLOCK))
		f->flag_file_lock = 1;	
	else
		f->flag_file_lock = 0;	
	return isfd;
}

/*
 * Read ISAM record, if locked retry as requested
 */
static int
isread_retry(cob_file *f, void *data, int mode)
{
	int	isfd, sts, retry, interval;
	struct indexfile	*fh;

	fh = f->file;
	isfd = fh->isfd;

	retry = interval = 0;
	if ((f->retry_mode & COB_RETRY_FOREVER)) {
		retry = -1;
	} else
	if ((f->retry_mode & COB_RETRY_SECONDS)) {
		retry = 1;
		interval = f->retry_seconds>0?f->retry_seconds:
			(cobsetptr->cob_retry_seconds>0?cobsetptr->cob_retry_seconds:1);
	} else
	if ((f->retry_mode & COB_RETRY_TIMES)) {
		retry = f->retry_times>0?f->retry_times:
			(cobsetptr->cob_retry_times>0?cobsetptr->cob_retry_times:1);
		interval = cobsetptr->cob_retry_seconds>0?cobsetptr->cob_retry_seconds:1;
	}
	if(retry > 0) {
		retry = retry * interval * COB_RETRY_PER_SECOND ;
		interval = 1000 / COB_RETRY_PER_SECOND ;
	}
	do {
		ISERRNO = 0;
		sts = isread (isfd, data, mode);
#ifdef	ISSKIPLOCK
		if ((mode & ISSKIPLOCK))
			break;
#endif
		if (!(mode & ISLOCK))
			break;
		if (ISERRNO != ELOCKED
		 || retry == 0
		 || sts == 0) 
			break;
		if(retry > 0) {
			retry--;
			cob_sys_sleep_msec(interval);
		}
	} while(sts != 0 && retry != 0);
	return sts;
}
#endif	/* WITH_ANY_ISAM */

#ifdef	WITH_DB

#if	0	/* RXWRXW - BDB msg */
static void
bdb_msgcall_set (DB_ENV *dbe, const char *err)
{
	COB_UNUSED (dbe);

	cob_runtime_error (_("BDB error: %s"), err);
	cob_stop_run (1);
}

static void
bdb_errcall_set (DB_ENV *dbe, const char *prefix, const char *err)
{
	COB_UNUSED (dbe);

	cob_runtime_error (_("BDB error: %s %s"), prefix, err);
	cob_stop_run (1);
}
#endif

static void
join_environment (void)
{
	cob_u32_t	flags;
	int		ret;

	if (cobsetptr->bdb_home == NULL) {
		return;
	}
	ret = db_env_create (&bdb_env, 0);
	if (ret) {
		cob_runtime_error (_("cannot join BDB environment (%s), error: %d %s"),
				   "env_create", ret, db_strerror (ret));
		cob_stop_run (1);
	}
#if	0	/* RXWRXW - BDB msg */
	bdb_env->set_errcall (bdb_env, bdb_errcall_set);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	bdb_env->set_msgcall (bdb_env, bdb_msgcall_set);
#endif
#else
	bdb_env->set_errfile (bdb_env, stderr);
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 2))
	bdb_env->set_msgfile (bdb_env, stderr);
#endif
#endif
	bdb_env->set_cachesize (bdb_env, 0, 2*1024*1024, 0);
	bdb_env->set_alloc (bdb_env, cob_malloc, realloc, cob_free);
	flags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_CDB;
	ret = bdb_env->open (bdb_env, cobsetptr->bdb_home, flags, 0);
	if (ret) {
		cob_runtime_error (_("cannot join BDB environment (%s), error: %d %s"),
				   "env->open", ret, db_strerror (ret));
		bdb_env->close (bdb_env, 0);
		bdb_env = NULL;
		cob_stop_run (1);
	}
#if (DB_VERSION_MAJOR > 4) || ((DB_VERSION_MAJOR == 4) && (DB_VERSION_MINOR > 1))
	bdb_env->get_data_dirs (bdb_env, &bdb_data_dir);
#endif
	bdb_env->lock_id (bdb_env, &bdb_lock_id);
	bdb_env->set_lk_detect (bdb_env, DB_LOCK_DEFAULT);
}

/* Impose lock on 'file' using BDB locking */
static int
bdb_lock_file (cob_file *f, char *filename, int lock_mode)
{
	struct indexed_file	*p;
	int			ret, retry, interval;
	DBT			dbt;

	if (bdb_env == NULL) 
		return 0;
	p = f->file;
	ret = 0;
	p->file_lock_set = 0;
	retry = interval = 0;
	if ((f->retry_mode & COB_RETRY_FOREVER)) {
		retry = -1;
	} else
	if ((f->retry_mode & COB_RETRY_SECONDS)) {
		retry = 1;
		interval = f->retry_seconds>0?f->retry_seconds:
			(cobsetptr->cob_retry_seconds>0?cobsetptr->cob_retry_seconds:1);
	} else
	if ((f->retry_mode & COB_RETRY_TIMES)) {
		retry = f->retry_times>0?f->retry_times:
			(cobsetptr->cob_retry_times>0?cobsetptr->cob_retry_times:1);
		interval = cobsetptr->cob_retry_seconds>0?cobsetptr->cob_retry_seconds:1;
	}
	if(retry > 0) {
		retry = retry * interval * COB_RETRY_PER_SECOND ;
		interval = 1000 / COB_RETRY_PER_SECOND ;
	}
	do {
		memset(&dbt,0,sizeof(dbt));
		dbt.size = (cob_dbtsize_t) strlen (filename);
		dbt.data = filename;
		ret = bdb_env->lock_get (bdb_env, bdb_lock_id, DB_LOCK_NOWAIT,
					&dbt, lock_mode, &p->bdb_file_lock);
		if (ret == 0) {
			p->file_lock_set = 1;
			break;
		}
		if (ret == DB_LOCK_DEADLOCK)
			return COB_STATUS_52_DEAD_LOCK;
		if(ret != DB_LOCK_NOTGRANTED) {
			break;
		}
		if (retry > 0) {
			retry--;
			cob_sys_sleep_msec(interval);
}
	} while (ret != 0 && retry != 0);

	if(ret == DB_LOCK_NOTGRANTED) 
		return COB_STATUS_61_FILE_SHARING;
	if (ret) {
		cob_runtime_error (_("BDB (%s), error: %d %s"),
				   "file lock_get", ret, db_strerror (ret));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return ret;
}

/* Impose lock on record and table it */
static int
lock_record (cob_file *f, const char *key, const unsigned int keylen)
{
	struct indexed_file	*p;
	size_t			len;
	int			j, k, ret, retry, interval;
	DBT			dbt;

	if (bdb_env == NULL) 
		return 0;
	p = f->file;
	ret = 0;
	retry = interval = 0;
	if ((f->retry_mode & COB_RETRY_FOREVER)) {
		retry = -1;
	} else
	if ((f->retry_mode & COB_RETRY_SECONDS)) {
		retry = 1;
		interval = f->retry_seconds>0?f->retry_seconds:
			(cobsetptr->cob_retry_seconds>0?cobsetptr->cob_retry_seconds:1);
	} else
	if ((f->retry_mode & COB_RETRY_TIMES)) {
		retry = f->retry_times>0?f->retry_times:
			(cobsetptr->cob_retry_times>0?cobsetptr->cob_retry_times:1);
		interval = cobsetptr->cob_retry_seconds>0?cobsetptr->cob_retry_seconds:1;
	}

	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		cob_free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename, (size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key, (size_t)keylen);

	if(retry > 0) {
		retry = retry * interval * COB_RETRY_PER_SECOND;
		interval = 1000 / COB_RETRY_PER_SECOND;
	}

	do {
	memset (&dbt, 0, sizeof (dbt));
	dbt.size = (cob_dbtsize_t) len;
	dbt.data = record_lock_object;
		ret = bdb_env->lock_get (bdb_env, bdb_lock_id, retry==-1?0:DB_LOCK_NOWAIT,
				&dbt, DB_LOCK_WRITE, &p->bdb_record_lock);
		if (ret == 0)
			break;
		if (ret == DB_LOCK_DEADLOCK)
			return COB_STATUS_52_DEAD_LOCK;
		if(ret != DB_LOCK_NOTGRANTED) {
			break;
		}
		if (retry > 0) {
			retry--;
			cob_sys_sleep_msec(interval);
		}
	} while (ret != 0 && retry != 0);

	if (!ret) {
		if (p->bdb_lock_max == 0) {
			p->bdb_lock_max = COB_MAX_BDB_LOCKS;
			p->bdb_locks = cob_malloc(p->bdb_lock_max * sizeof(DB_LOCK));
			p->bdb_lock_num = 0;
		}
		if (p->bdb_lock_num >= p->bdb_lock_max) {
			p->bdb_lock_max += COB_MAX_BDB_LOCKS;
			p->bdb_locks = realloc(p->bdb_locks, p->bdb_lock_max * sizeof(DB_LOCK));
		}
		for(k = 0; k < p->bdb_lock_num; k++) {
			if (memcmp(&p->bdb_record_lock, &p->bdb_locks[k], sizeof(DB_LOCK)) == 0) {
				/* Move to end of lock table for later: bdb_unlock_last */
				for (j=k;  j < p->bdb_lock_num; j++) {
					memcpy (&p->bdb_locks[j], &p->bdb_locks[j+1], sizeof(DB_LOCK));
				}
				memcpy (&p->bdb_locks[p->bdb_lock_num-1], &p->bdb_record_lock, sizeof(DB_LOCK));
				/* Release lock just acquired as it is a duplicate */
				ret = bdb_env->lock_put (bdb_env, &p->bdb_record_lock);
				return ret;
			}
		}
		if (p->bdb_lock_num < p->bdb_lock_max) {
			p->bdb_locks [ p->bdb_lock_num++ ] = p->bdb_record_lock;
		}
	}
	if(ret == DB_LOCK_NOTGRANTED) 
		return COB_STATUS_51_RECORD_LOCKED;
	if (ret) {
		cob_runtime_error (_("BDB (%s), error: %d %s"),
				   "lock_get", ret, db_strerror (ret));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return ret;
}

static int
test_record_lock (cob_file *f, const char *key, const unsigned int keylen)
{
	struct indexed_file	*p;
	size_t			len;
	int			j, k, ret, retry, interval;
	DBT			dbt;
	DB_LOCK			test_lock;

	if (bdb_env == NULL) 
		return 0;
	p = f->file;
	ret = 0;
	retry = interval = 0;
	if ((f->retry_mode & COB_RETRY_FOREVER)) {
		retry = -1;
	} else
	if ((f->retry_mode & COB_RETRY_SECONDS)) {
		retry = 1;
		interval = f->retry_seconds>0?f->retry_seconds:
			(cobsetptr->cob_retry_seconds>0?cobsetptr->cob_retry_seconds:1);
	} else
	if ((f->retry_mode & COB_RETRY_TIMES)) {
		retry = f->retry_times>0?f->retry_times:
			(cobsetptr->cob_retry_times>0?cobsetptr->cob_retry_times:1);
		interval = cobsetptr->cob_retry_seconds>0?cobsetptr->cob_retry_seconds:1;
	}
	len = keylen + p->filenamelen + 1;
	if (len > rlo_size) {
		cob_free (record_lock_object);
		record_lock_object = cob_malloc (len);
		rlo_size = len;
	}
	memcpy ((char *)record_lock_object, p->filename, (size_t)(p->filenamelen + 1));
	memcpy ((char *)record_lock_object + p->filenamelen + 1, key, (size_t)keylen);
	memset(&test_lock,0,sizeof(test_lock));
	if(retry > 0) {
		retry = retry * interval * COB_RETRY_PER_SECOND ;
		interval = 1000 / COB_RETRY_PER_SECOND ;
	}
	do {
	memset (&dbt, 0, sizeof (dbt));
	dbt.size = (cob_dbtsize_t) len;
	dbt.data = record_lock_object;
		ret = bdb_env->lock_get (bdb_env, bdb_lock_id, DB_LOCK_NOWAIT,
				&dbt, DB_LOCK_WRITE, &test_lock);
		if (ret == 0)
			break;
		if (ret == DB_LOCK_DEADLOCK)
			return COB_STATUS_52_DEAD_LOCK;
		if(ret != DB_LOCK_NOTGRANTED) {
			break;
		}
		if (retry > 0) {
			retry--;
			cob_sys_sleep_msec(interval);
		}
	} while (ret != 0 && retry != 0);

	if (!ret) {
		if (p->bdb_lock_num > 0) {
			for(k = 0; k < p->bdb_lock_num; k++) {
				if (memcmp(&test_lock, &p->bdb_locks[k], sizeof(DB_LOCK)) == 0) {
					/* Move to end of lock table for later: bdb_unlock_last */
					for (j=k;  j < p->bdb_lock_num; j++) {
						memcpy (&p->bdb_locks[j], &p->bdb_locks[j+1], sizeof(DB_LOCK));
					}
					memcpy (&p->bdb_locks[p->bdb_lock_num-1], &test_lock, sizeof(DB_LOCK));
					break;
				}
			}
		}
		ret = bdb_env->lock_put (bdb_env, &test_lock);/* Release lock just acquired */
	}
	if(ret == DB_LOCK_NOTGRANTED) 
		return COB_STATUS_51_RECORD_LOCKED;
	if (ret) {
		cob_runtime_error (_("BDB (%s), error: %d %s"),
				   "lock_get", ret, db_strerror (ret));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return ret;
}

static int
unlock_record (cob_file *f)
{
	struct indexed_file	*p;
	int			ret = 0, k;

	p = f->file;
	if (p->bdb_lock_num == 0
	 || bdb_env == NULL) {
		return 0;
	}
	if (p->bdb_lock_num > 0) {
		for (k=p->bdb_lock_num-1; k >= 0; k--) {
			ret = bdb_env->lock_put (bdb_env, &p->bdb_locks[k]);
		}
		p->bdb_lock_num = 0;
	} else {
	ret = bdb_env->lock_put (bdb_env, &p->bdb_record_lock);
	}
	if (ret) {
		cob_runtime_error (_("BDB (%s), error: %d %s"),
				   "lock_put", ret, db_strerror (ret));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return ret;
}

static int
bdb_unlock_last (cob_file *f)
{
	struct indexed_file	*p;
	int			ret = 0;

	p = f->file;
	if (p->bdb_lock_num == 0
	 || bdb_env == NULL) {
		return 0;
	}
	if (p->bdb_lock_num > 0) {
		p->bdb_lock_num--;
		ret = bdb_env->lock_put (bdb_env, &p->bdb_locks[p->bdb_lock_num]);
	}
	if (ret) {
		cob_runtime_error (_("BDB (%s), error: %d %s"),
				   "lock_put", ret, db_strerror (ret));
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return ret;
}

static int
bdb_test_lock_advance(cob_file *f, int nextprev, int skip_lock)
{
	struct indexed_file	*p;
	int			ret;

	p = f->file;
	ret = bdb_test_record_lock (f, p->key.data, p->key.size);
	while (ret == COB_STATUS_51_RECORD_LOCKED
	   &&  skip_lock) {
		ret = DB_SEQ (p->cursor[p->key_index], nextprev);
		if (ret == DB_NOTFOUND)
			return COB_STATUS_10_END_OF_FILE;
		if (!ret) {
			ret = bdb_test_record_lock (f, p->key.data, p->key.size);
		}
	}
	return ret;
}

static int
bdb_lock_advance(cob_file *f, int nextprev, int skip_lock)
{
	struct indexed_file	*p;
	int			ret;

	p = f->file;
	ret = bdb_lock_record (f, p->key.data, p->key.size);
	while (ret == COB_STATUS_51_RECORD_LOCKED
	   &&  skip_lock) {
		ret = DB_SEQ (p->cursor[p->key_index], nextprev);
		if (ret == DB_NOTFOUND)
			return COB_STATUS_10_END_OF_FILE;
		if (!ret) {
			ret = bdb_test_record_lock (f, p->key.data, p->key.size);
		}
	}
	return ret;
}

/* Get the next number in a set of duplicates */
static unsigned int
get_dupno (cob_file *f, const cob_u32_t i)
{
	struct indexed_file	*p;
	int			ret;
	unsigned int		dupno;

	p = f->file;
	dupno = 0;
	bdb_setkey(f, i);
	memcpy (p->temp_key, p->key.data, (size_t)p->maxkeylen);
	p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], 0);
	ret = DB_SEQ (p->cursor[i], DB_SET_RANGE);
	while (ret == 0 && memcmp (p->key.data, p->temp_key, (size_t)p->key.size) == 0) {
		memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
		ret = DB_SEQ (p->cursor[i], DB_NEXT);
	}
	p->cursor[i]->c_close (p->cursor[i]);
	p->cursor[i] = NULL;
	dupno = COB_DUPSWAP(dupno);
	return ++dupno;
}

/* read file with all alternate keys that don't allow duplicates
   to check if records exist already, returns 1 if true */
static int
check_alt_keys (cob_file *f, const int rewrite)
{
	struct indexed_file	*p;
	size_t			i;
	int			ret;

	p = f->file;
	for (i = 1; i < f->nkeys; ++i) {
		if (!f->keys[i].tf_duplicates) {
			bdb_setkey (f, i);
			ret = DB_GET (p->db[i], 0);
			if (ret == 0) {
				if (rewrite) {
					if (bdb_cmpkey (f, p->data.data, f->record->data, 0, 0)) {
						return 1;
					}
				} else {
					return 1;
				}
			}
		}
	}
	return 0;
}

static int
indexed_write_internal (cob_file *f, const int rewrite, const int opt)
{
	struct indexed_file	*p;
	cob_u32_t		i, len;
	unsigned int		dupno;
	cob_u32_t		flags;
	int			close_cursor, ret;

	p = f->file;
	close_cursor = bdb_open_cursor (f, 1);

	/* Check duplicate alternate keys */
	if (f->nkeys > 1 && !rewrite) {
		if (check_alt_keys (f, 0)) {
			bdb_close_cursor (f);
			return COB_STATUS_22_KEY_EXISTS;
		}
		bdb_setkey (f, 0);
	} else if (!rewrite) {
		bdb_setkey(f, 0);
	}

	/* Write data */
	if (DB_SEQ (p->cursor[0], DB_SET) == 0) {
		bdb_close_cursor (f);
		return COB_STATUS_22_KEY_EXISTS;
	}
	p->data.data = f->record->data;
	p->data.size = (cob_dbtsize_t) f->record->size;
	DB_CPUT(p->cursor[0], DB_KEYFIRST);

	/* Write secondary keys */
	p->data = p->key;
	for (i = 1; i < f->nkeys; ++i) {
		if (rewrite && ! p->rewrite_sec_key[i]) {
			continue;
		}
		if (bdb_suppresskey (f, i)) {
			continue;
		bdb_setkey (f, i);
		memset(&p->data,0,sizeof(p->data));
		if (f->keys[i].tf_duplicates) {
			flags =  0;
			dupno = get_dupno(f, i);
			dupno = COB_DUPSWAP (dupno);
			len = bdb_savekey(f, p->temp_key, f->record->data, 0);
			p->data.data = p->temp_key;
			p->data.size = (cob_dbtsize_t)len;
			memcpy (((char*)(p->data.data)) + p->data.size, &dupno, sizeof (unsigned int));
			p->data.size += sizeof (unsigned int);
		} else {
			len = bdb_savekey(f, p->temp_key, f->record->data, 0);
			p->data.data = p->temp_key;
			p->data.size = (cob_dbtsize_t)len;
			flags = DB_NOOVERWRITE;
			dupno = 0;
		}
		bdb_setkey (f, i);

		ret = DB_PUT (p->db[i], flags);
#if (DB_VERSION_MAJOR < 6)
		if (ret == ENOENT) {	/* This is strange, but BDB 5.3 was returning ENOENT sometimes */
			ret = DB_PUT (p->db[i], 0);
			}
#endif
		if (ret != 0) {
			bdb_close_cursor (f);
			return COB_STATUS_22_KEY_EXISTS;
		}
	}

				if (close_cursor) {
		bdb_close_cursor (f);
		}
	if ((opt & COB_WRITE_LOCK) 
	 && (bdb_env != NULL)) {
		bdb_setkey(f, 0);
		if ((ret = bdb_lock_record (f, p->key.data, p->key.size))) {
			bdb_close_cursor (f);
			return ret;
	}
	}
	return COB_STATUS_00_SUCCESS;
}

static int
indexed_start_internal (cob_file *f, const int cond, cob_field *key,
			const int read_opts, const int test_lock)
{
	struct indexed_file	*p;
	int			ret, len, fullkeylen, partlen;
	unsigned int		dupno;

	dupno = 0;
	ret = 0;
	p = f->file;
	/* Look up for the key */
	p->key_index = bdb_findkey(f, key, &fullkeylen, &partlen);
	if(p->key_index < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}

	/* Search */
	bdb_setkey (f, p->key_index);
	p->key.size = (cob_dbtsize_t)partlen;	/* may be partial key */
	/* The open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);
	if (cond == COB_FI) {
		ret = DB_SEQ (p->cursor[p->key_index], DB_FIRST);
	} else if (cond == COB_LA) {
		ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
	} else {
		ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
	}
	switch (cond) {
	case COB_EQ:
		if (ret == 0) {
			ret = bdb_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen);
		}
		break;
	case COB_LT:
		if (ret != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
		} else {
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
		}
		break;
	case COB_LE:
		if (ret != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
		} else if (bdb_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen) != 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
		} else if (f->keys[p->key_index].tf_duplicates) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT_NODUP);
			if (ret != 0) {
				ret = DB_SEQ (p->cursor[p->key_index], DB_LAST);
			} else {
				ret = DB_SEQ (p->cursor[p->key_index], DB_PREV);
			}
		}
		break;
	case COB_GT:
		while (ret == 0 && bdb_cmpkey (f, p->key.data, f->record->data, p->key_index, partlen) == 0) {
			ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
		}
		break;
	case COB_GE:
		/* nothing */
		break;
	case COB_FI:
		/* nothing */
		break;
	case COB_LA:
		/* nothing */
		break;
	}

	if (ret == 0 && p->key_index > 0) {
		/* Temporarily save alternate key */
		len = p->key.size;
		memcpy (p->temp_key, p->key.data, len);
		if (f->keys[p->key_index].tf_duplicates) {
			memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
			dupno = COB_DUPSWAP (dupno);
		}
		p->key.data = p->data.data;
		p->key.size = p->primekeylen;
		ret = DB_GET (p->db[0], 0);
	}

	if (p->key_index > 0)
		bdb_close_index (f, p->key_index);
	bdb_close_cursor (f);

	if (ret == 0 && test_lock) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)
		 && !(read_opts & COB_READ_NO_LOCK)
		 && !(read_opts & COB_READ_LOCK)) {
			ret = bdb_test_record_lock (f, p->key.data, p->key.size);
			if (ret) {
				bdb_close_index (f, p->key_index);
				bdb_close_cursor (f);
				return ret;
			}
		}
		if (read_opts & COB_READ_LOCK) {
			ret = bdb_lock_record (f, p->key.data, p->key.size);
			if (ret) {
				bdb_close_index (f, p->key_index);
				bdb_close_cursor (f);
				return ret;
			}
		}
	}

	if (ret == 0) {
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, p->primekeylen);
		} else {
			memcpy (p->last_readkey[p->key_index],
				    p->temp_key, bdb_keylen(f,p->key_index));
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, p->primekeylen);
			if (f->keys[p->key_index].tf_duplicates) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

	bdb_close_index (f, p->key_index);
	if (p->key_index != 0) {
		bdb_close_cursor (f);
	}

	return (ret == 0) ? COB_STATUS_00_SUCCESS : COB_STATUS_23_KEY_NOT_EXISTS;
}

static int
indexed_delete_internal (cob_file *f, const int rewrite, int bdb_opts)
{
	struct indexed_file	*p;
	int			i,len;
	DBT			prim_key;
	int			ret;
	cob_u32_t		flags;
	int			close_cursor = 0;

	p = f->file;
	if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
		bdb_unlock_all (f);
	}
	/* Find the primary key */
	if (f->access_mode != COB_ACCESS_SEQUENTIAL) {
		bdb_setkey(f, 0);
	}
	if (bdb_env != NULL) {
		ret = bdb_test_record_lock (f, p->key.data, p->key.size);
		if (ret) {
			bdb_close_cursor (f);
			return ret;
			}
		}
	if (bdb_env) {
		flags = DB_WRITECURSOR;
	} else {
		flags = 0;
	}
	close_cursor = bdb_open_cursor (f, 1);
	ret = DB_SEQ (p->cursor[0], DB_SET);
	if (ret != 0 && f->access_mode != COB_ACCESS_SEQUENTIAL) {
		bdb_close_cursor (f);
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	prim_key = p->key;
	memcpy(p->saverec, p->data.data, p->data.size);		/* Save old record image */
	memcpy(p->temp_key,prim_key.data,prim_key.size);	/* Save primary key value */
	prim_key.data = p->temp_key;

	/* Delete the secondary keys */
	for (i = 1; i < f->nkeys; ++i) {
		len = bdb_savekey(f, p->suppkey, p->data.data, i);
		memset(p->savekey, 0, p->maxkeylen);
		len = bdb_savekey(f, p->savekey, p->saverec, i);
		p->key.data = p->savekey;
		p->key.size = (cob_dbtsize_t) len;
		/* rewrite: no delete if secondary key is unchanged */
		if (rewrite) {
			p->rewrite_sec_key[i] = bdb_cmpkey(f, p->suppkey, f->record->data, i, 0);
			if (!p->rewrite_sec_key[i]) {
				continue;
			}
		}
		if (!f->keys[i].tf_duplicates) {
			DB_DEL (p->db[i], &p->key, 0);
		} else {
			DBT	sec_key = p->key;

			p->db[i]->cursor (p->db[i], NULL, &p->cursor[i], flags);
			if (DB_SEQ (p->cursor[i], DB_SET_RANGE) == 0) {
				while (sec_key.size == p->key.size
				&& memcmp (p->key.data, sec_key.data, (size_t)sec_key.size) == 0) {
					if (memcmp (p->data.data, prim_key.data, (size_t)prim_key.size) == 0) {
						ret = DB_CDEL(p->cursor[i], 0);
					}
					if (DB_SEQ (p->cursor[i], DB_NEXT) != 0) {
						break;
					}
				}
			}
			bdb_close_index (f, i);
		}
	}

	/* Delete the record */
	ret = DB_CDEL(p->cursor[0], 0);

	if (close_cursor && !rewrite) {
		bdb_close_cursor (f);
	}
	return COB_STATUS_00_SUCCESS;
}

/* Check if a file exists in bdb data dirs */

static int
is_absolute (const char *filename)
{
#ifdef	_WIN32
	if (filename[0] == '/' || filename[0] == '\\') {
		return 1;
	} else {
		if (isalpha (filename[0]) && filename[1] == ':' &&
		  (filename[2] == '/' || filename[2] == '\\')) {
			return 1;
		} else {
			return 0;
		}
	}
#else
	if (filename[0] == '/') {
		return 1;
	} else {
		return 0;
	}
#endif
}

static int
bdb_nofile (const char *filename)
{
	cob_u32_t	i;

	if (!bdb_env || is_absolute (filename)) {
		errno = 0;
		if (bdb_buff)
			strcpy(bdb_buff, filename);
		if (access (filename, F_OK) && errno == ENOENT) {
			return 1;
		}
		return 0;
	}

	for (i = 0; bdb_data_dir && bdb_data_dir[i]; ++i) {
		bdb_buff[COB_SMALL_MAX] = 0;
		if (is_absolute (bdb_data_dir[i])) {
			snprintf (bdb_buff, (size_t)COB_SMALL_MAX, "%s%c%s",
				  bdb_data_dir[i], SLASH_CHAR, filename);
		} else {
			snprintf (bdb_buff, (size_t)COB_SMALL_MAX, "%s%c%s%c%s",
				  cobsetptr->bdb_home, SLASH_CHAR, bdb_data_dir[i], SLASH_CHAR, filename);
		}
		errno = 0;
		if (access (bdb_buff, F_OK) == 0 || errno != ENOENT) {
			return 0;
		}
	}
	bdb_buff[COB_SMALL_MAX] = 0;
		snprintf (bdb_buff, (size_t)COB_SMALL_MAX, "%s%c%s",
			  cobsetptr->bdb_home, SLASH_CHAR, filename);
		errno = 0;
		if (access (bdb_buff, F_OK) == 0 || errno != ENOENT) {
			return 0;
		}
	return 1;
}

#endif	/* WITH_DB */

/* Delete file */

static void
indexed_file_delete (cob_file *f, const char *filename)
{
#ifdef	WITH_ANY_ISAM
#if defined(WITH_DISAM)
	struct stat	st;
#endif
	COB_UNUSED (f);

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.idx", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	unlink (file_open_buff);
	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.dat", filename);
	file_open_buff[COB_FILE_MAX] = 0;
#if defined(WITH_DISAM)
	if (stat(file_open_buff, &st) != 0) {	/* Micro Focus naming style has no .dat */
		snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s", filename);
	}
#endif
	unlink (file_open_buff);
#elif	defined(WITH_DB)
	size_t	i;

	for (i = 0; i < f->nkeys; ++i) {
		if (i == 0) {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s",
				  filename);
		} else {
			snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.%d",
				  filename, (int)i);
		}
		file_open_buff[COB_FILE_MAX] = 0;
		unlink (file_open_buff);
	}
#endif
}

/* OPEN INDEXED file */

static int
indexed_open (cob_file *f, char *filename, const int mode, const int sharing)
{
	/* Note filename points to file_open_name */
	/* cob_chk_file_mapping manipulates file_open_name directly */

#ifdef	WITH_INDEX_EXTFH
	int		ret;

	f->share_mode = sharing;
	set_file_format(f);		/* Set file options */
	ret = extfh_indexed_locate (f, filename);
	switch (ret) {
	case COB_NOT_CONFIGURED:
		cob_chk_file_mapping (f);
		if (access (filename, F_OK) && errno == ENOENT) {
			if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
				return COB_STATUS_35_NOT_EXISTS;
			}
		}
		break;
	case COB_STATUS_00_SUCCESS:
		break;
	default:
		return ret;
	}
	ret = extfh_indexed_open (f, filename, mode, sharing);
	switch (ret) {
	case COB_STATUS_00_SUCCESS:
		f->open_mode = mode;
		break;
	case COB_STATUS_35_NOT_EXISTS:
		if (f->flag_optional) {
			f->open_mode = mode;
			f->flag_nonexistent = 1;
			f->flag_end_of_file = 1;
			f->flag_begin_of_file = 1;
			return COB_STATUS_05_SUCCESS_OPTIONAL;
		}
		break;
	}
	return ret;

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh, *fh2;
	size_t			k;
	int			ret,len,j;
	int			omode;
	int			lmode;
	int			vmode;
	int			dobld;
	int			isfd;
	int			checkvalue;
	struct keydesc		kd;
	struct dictinfo		di;		/* Defined in (c|d|vb)isam.h */

	f->share_mode = sharing;

	cob_chk_file_mapping (f);
	set_file_format(f);		/* Set file options */

	if (mode == COB_OPEN_INPUT) {
		checkvalue = R_OK;
	} else {
		checkvalue = R_OK | W_OK;
	}

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.idx", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	errno = 0;
	if (access (file_open_buff, checkvalue)) {
		if (!(errno == ENOENT && (mode == COB_OPEN_OUTPUT || f->flag_optional == 1))) {
			switch (errno) {
			case ENOENT:
				return COB_STATUS_35_NOT_EXISTS;
			case EACCES:
				return COB_STATUS_37_PERMISSION_DENIED;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s.dat", filename);
	file_open_buff[COB_FILE_MAX] = 0;
	errno = 0;
#if defined(WITH_DISAM)
	if (access (file_open_buff, checkvalue)
	&& (errno == ENOENT) ) {	/* D-ISAM will handle files with Micro Focus naming style */
		errno = 0;
		snprintf (file_open_buff, (size_t)COB_FILE_MAX, "%s", filename);
	}
#endif
	if (access (file_open_buff, checkvalue)) {
		if (!(errno == ENOENT && (mode == COB_OPEN_OUTPUT || f->flag_optional == 1))) {
			switch (errno) {
			case ENOENT:
				return COB_STATUS_35_NOT_EXISTS;
			case EACCES:
				return COB_STATUS_37_PERMISSION_DENIED;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}
	}

	ret = COB_STATUS_00_SUCCESS;
	omode = 0;
	lmode = 0;
	vmode = 0;
	dobld = 0;
	isfd = -1;
#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		vmode = ISVARLEN;
		ISRECLEN = f->record_min;
	}
#endif
	if ((f->share_mode & COB_SHARE_NO_OTHER)
	 || (f->lock_mode & COB_FILE_EXCLUSIVE) ) {
		lmode = ISEXCLLOCK;
	} else if (!f->lock_mode) {
		if (mode != COB_OPEN_INPUT) {
			lmode = ISEXCLLOCK;
		} else {
			lmode = ISMANULOCK;
		}
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) && mode != COB_OPEN_INPUT) {
		lmode = ISAUTOLOCK;
	} else {
		lmode = ISMANULOCK;
	}
	switch (mode) {
	case COB_OPEN_INPUT:
		omode = ISINPUT;
		break;
	case COB_OPEN_OUTPUT:
		lmode = ISEXCLLOCK;
		omode = ISOUTPUT;
		ISERRNO = 0;
		isfd = isopen ((void *)filename, ISINPUT | ISEXCLLOCK | vmode);
		if (ISERRNO == EFLOCKED) {
			return COB_STATUS_61_FILE_SHARING;
		} else {
			if (isfd >= 0) {
				isfullclose (isfd);
			}
			iserase ((void *)filename);
			ISERRNO = 0;
		}
		dobld = 1;
		break;
	case COB_OPEN_I_O:
		omode = ISINOUT;
		break;
	case COB_OPEN_EXTEND:
		lmode = ISEXCLLOCK;
		omode = ISINOUT;
		break;
	}
	fh = cob_malloc (sizeof (struct indexfile) +
			 ((sizeof (struct keydesc)) * (f->nkeys + 1)));
	/* Copy index information */
	for (k = 0; k < f->nkeys; ++k) {
		len = indexed_keydesc(f, &fh->key[k], &f->keys[k]);
		if (fh->lenkey < len) {
			fh->lenkey = len;
		}
	}
	ISERRNO = 0;
	fh->lmode = 0;
#ifdef WITH_VBISAM
	remove_ext(filename); //kamal079 - remove ".dat" if vbisam
#endif	
	if (dobld) {
dobuild:
		isfd = isbuild ((void *)filename, (int)f->record_max, &fh->key[0],
				vmode | ISINOUT | ISEXCLLOCK);
		f->flag_file_lock = 1;
		if (ISERRNO == EEXIST
		 && isfd < 0) {
			/* Erase file and redo the 'isbuild' */
			iserase ((void *)filename);
			isfd = isbuild ((void *)filename, (int)f->record_max, &fh->key[0],
					vmode | ISINOUT | ISEXCLLOCK);
			f->flag_file_lock = 1;
		}
	} else {
		if (lmode == ISAUTOLOCK
		&& (f->lock_mode & COB_LOCK_MULTIPLE)) {
			lmode = ISMANULOCK;
		}
		if (lmode == ISMANULOCK) {
			fh->lmode = ISLOCK; 	/* fileio will handle Record locking */
		}
		isfd = isopen_retry (f, (char *)filename, omode | lmode | vmode);
		if (isfd < 0) {
			if (ISERRNO == EFLOCKED)
				return COB_STATUS_61_FILE_SHARING;
			if (f->flag_optional) {
				if (mode == COB_OPEN_EXTEND 
				 || mode == COB_OPEN_I_O) {
					dobld = 1;
					ret = COB_STATUS_05_SUCCESS_OPTIONAL;
					goto dobuild;
				}
				freefh (fh);
				f->open_mode = mode;
				f->flag_end_of_file = 1;
				f->flag_begin_of_file = 1;
				if (f->flag_nonexistent) {
					return COB_STATUS_00_SUCCESS;
				}
				f->flag_nonexistent = 1;
				return COB_STATUS_05_SUCCESS_OPTIONAL;
			}
		} else {
			memset(&di, 0, sizeof (di));
			isindexinfo (isfd, (void *)&di, 0);
			/* Mask off ISVARLEN */
			fh->nkeys = di.di_nkeys & 0x7F;
			if (fh->nkeys != f->nkeys
			 && f->flag_keycheck) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			} else if (fh->nkeys > f->nkeys) {
				/* More keys in file than COBOL has defined */
				fh2 = cob_malloc (sizeof(struct indexfile) +
						 ((sizeof (struct keydesc)) * (fh->nkeys + 1)));
				memcpy (fh2, fh, sizeof(struct indexfile) +
						((sizeof (struct keydesc)) * (f->nkeys + 1)));
				cob_free (fh);
				fh = fh2;
			}
			if (f->record_max != di.di_recsize) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
			if (!f->flag_keycheck) {
				/* Copy real ISAM file key information */
			for (k = 0; k < fh->nkeys && !ret; ++k) {
				memset (&fh->key[k], 0, sizeof (struct keydesc));
				isindexinfo (isfd, &fh->key[k], (int)(k+1));
				if (fh->lenkey < indexed_keylen(fh, k)) {
					fh->lenkey = indexed_keylen(fh, k);
				}
				}
				/* Verify that COBOL keys defined match some real ISAM key */
				for (j = 0; j < f->nkeys && !ret; ++j) {
					indexed_keydesc(f, &kd, &f->keys[j]);
					for (k = 0; k < fh->nkeys; ++k) {
						if (indexed_keycmp(&kd, &fh->key[k]) == 0) {
							break;
						}
					}
					if (k >= fh->nkeys) {
							ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
					}
				}
			} else {
				for (k = 0; k < fh->nkeys && !ret; ++k) {
					memset (&fh->key[k], 0, sizeof(struct keydesc));
					isindexinfo (isfd, &fh->key[k], (int)(k+1));
					if (fh->lenkey < indexed_keylen(fh, k)) {
						fh->lenkey = indexed_keylen(fh, k);
					}
					/* Verify that COBOL keys match exactly to real ISAM keys */
				len = indexed_keydesc(f, &kd, &f->keys[k]);
				if (fh->lenkey < len) {
					fh->lenkey = len;
				}
				if(indexed_keycmp(&kd, &fh->key[k]) != 0) {
					ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
					break;
				}
			}
		}
	}
	}
	if (isfd < 0) {
		ret = fisretsts (COB_STATUS_30_PERMANENT_ERROR);
		freefh (fh);
		return ret;
	}
	if (ret > 9) {
		isfullclose (isfd);
		freefh (fh);
		return ret;
	}
	if (dobld) {
		for (k = 1; k < f->nkeys; ++k) {
			ISERRNO = 0;
			if (isaddindex (isfd, &fh->key[k])) {
				ret = COB_STATUS_39_CONFLICT_ATTRIBUTE;
			}
		}
		if (ret > 9) {
			isfullclose (isfd);
			iserase ((void *)filename);
			freefh (fh);
			return ret;
		}
	}
	f->file = fh;
	f->open_mode = mode;
	fh->isfd = isfd;
	fh->filename = cob_strdup (filename);
	fh->savekey = cob_malloc ((size_t)(fh->lenkey + 1));
	fh->recwrk = cob_malloc ((size_t)(f->record_max + 1));
	/* Active index is unknown at this time */
	fh->curkey = -1;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	return ret;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	size_t			i;
	size_t			j;
	size_t			maxsize;
	db_lockmode_t		lock_mode;
	int			handle_created;
	cob_u32_t		flags = 0;
	int			ret = 0;
	int			nonexistent;

	if (cobsetptr->bdb_home != NULL
	 && bdb_env == NULL) {		/* Join BDB, on first OPEN of INDEXED file */
		join_environment ();
	}
	f->share_mode = sharing;

	cob_chk_file_mapping (f);
	set_file_format(f);		/* Set file options */

	nonexistent = 0;
	if (bdb_nofile (filename)) {
		nonexistent = 1;
		if (mode != COB_OPEN_OUTPUT && f->flag_optional == 0) {
			return COB_STATUS_35_NOT_EXISTS;
		}
	}

	p = cob_malloc (sizeof (struct indexed_file));
	f->flag_file_lock = 0;	
	if (bdb_env != NULL) {
		if ((f->share_mode & COB_SHARE_ALL_OTHER)) {
			lock_mode = DB_LOCK_READ;
		} else
		if (mode == COB_OPEN_OUTPUT 
		 || mode == COB_OPEN_EXTEND 
		 || (f->share_mode & COB_SHARE_NO_OTHER)
		 || (f->lock_mode & COB_FILE_EXCLUSIVE) 
		 || (mode == COB_OPEN_I_O && !f->lock_mode)) {
			lock_mode = DB_LOCK_WRITE;
			f->flag_file_lock = 1;	
		} else {
			lock_mode = DB_LOCK_READ;
		}
		f->file = p;
		ret = bdb_lock_file (f, filename, lock_mode);
		if (ret) {
			cob_free (p);
			f->file = NULL;
			return ret;
		}
	}

	switch (mode) {
	case COB_OPEN_INPUT:
		flags |= DB_RDONLY;
		break;
	case COB_OPEN_OUTPUT:
		flags |= DB_CREATE;
		break;
	case COB_OPEN_I_O:
	case COB_OPEN_EXTEND:
		flags |= DB_CREATE;
		break;
	}

	p->db = cob_malloc (sizeof (DB *) * f->nkeys);
	p->cursor = cob_malloc (sizeof (DBC *) * f->nkeys);
	p->filenamelen = (int) strlen (filename);
	p->last_readkey = cob_malloc (sizeof (unsigned char *) * 2 * f->nkeys);
	p->last_dupno = cob_malloc (sizeof (unsigned int) * f->nkeys);
	p->rewrite_sec_key = cob_malloc (sizeof (int) * f->nkeys);
	maxsize = p->primekeylen = bdb_keylen(f, 0);
	for (i = 1; i < f->nkeys; ++i) {
		j = bdb_keylen(f, i);
		if( j > maxsize)
			maxsize = j;
	}
	p->maxkeylen = maxsize;

	for (i = 0; i < f->nkeys; ++i) {
		/* File name */
		runtime_buffer[COB_FILE_MAX] = 0;
		if (i == 0) {
			snprintf (runtime_buffer, (size_t)COB_FILE_MAX, "%s", filename);
			} else {
			snprintf (runtime_buffer, (size_t)COB_FILE_MAX, "%s.%d", filename, (int)i);
			}

		/* btree info */
		ret = db_create (&p->db[i], bdb_env, 0);
		if (!ret) {
			handle_created = 1;
			if (mode == COB_OPEN_OUTPUT) {
				if (bdb_env) {
					if (!bdb_nofile(runtime_buffer)) {
						ret = bdb_env->dbremove (bdb_env, NULL, runtime_buffer, NULL, 0);
						if (ret == ENOENT)
							ret = 0;
					}
				} else {
					/* FIXME: test "First READ on empty SEQUENTIAL INDEXED file ..."
					   on OPEN-OUTPUT results with MinGW & BDB 6 in
					   BDB1565 DB->pget: method not permitted before handle's open method
					*/
					p->db[i]->remove (p->db[i], runtime_buffer, NULL, 0);
					ret = db_create (&p->db[i], bdb_env, 0);
				}
			}
			if (!ret) {
				if (f->keys[i].tf_duplicates) {
					p->db[i]->set_flags (p->db[i], DB_DUP);
				}
			}
		} else {
			handle_created = 0;
		}
		/* Open db */
		if (!ret) {
			/* FIXME: test "First READ on empty SEQUENTIAL INDEXED file ..."
			   on OPEN-OUTPUT results with MinGW & BDB 6 in
			   BDB0588 At least one secondary cursor must be specified to DB->join
			*/
			ret = p->db[i]->open (p->db[i], NULL, runtime_buffer, NULL,
						DB_BTREE, flags, COB_FILE_MODE);
		}
		if (ret) {
			for (j = 0; j < i; ++j) {
				DB_CLOSE (p->db[j]);
			}
			if (handle_created) {
				DB_CLOSE (p->db[i]);
			}
			cob_free (p->db);
			cob_free (p->last_readkey);
			cob_free (p->last_dupno);
			cob_free (p->cursor);
			if (bdb_env != NULL) {
				if(p->file_lock_set) {
				bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
					p->file_lock_set = 0;
				}
			}
			cob_free (p);
			switch (ret) {
			case DB_LOCK_NOTGRANTED:
				return COB_STATUS_61_FILE_SHARING;
			case ENOENT:
				if (mode == COB_OPEN_EXTEND ||
				    mode == COB_OPEN_OUTPUT) {
					return COB_STATUS_30_PERMANENT_ERROR;
				}
				if (f->flag_optional) {
					if (mode == COB_OPEN_I_O) {
						return COB_STATUS_30_PERMANENT_ERROR;
					}
					f->open_mode = mode;
					f->flag_nonexistent = 1;
					f->flag_end_of_file = 1;
					f->flag_begin_of_file = 1;
					/* RXWRXW - Check directory exists? */
					return COB_STATUS_05_SUCCESS_OPTIONAL;
				}
				return COB_STATUS_35_NOT_EXISTS;
			default:
				return COB_STATUS_30_PERMANENT_ERROR;
			}

		}

		p->last_readkey[i] = cob_malloc (maxsize);
		p->last_readkey[f->nkeys + i] = cob_malloc (maxsize);
	}

	p->temp_key = cob_malloc (maxsize + sizeof (unsigned long));
	p->savekey  = cob_malloc (maxsize + sizeof (unsigned long));
	p->suppkey  = cob_malloc (maxsize + sizeof (unsigned long));
	p->saverec  = cob_malloc (f->record_max + sizeof (unsigned long));
	f->file = p;
	p->key_index = 0;
	p->last_key = NULL;

	memset ((void *)&p->key, 0, sizeof (DBT));
	memset ((void *)&p->data, 0, sizeof (DBT));
	p->filename = cob_malloc (strlen (filename) + 1);
	strcpy (p->filename, filename);
	p->write_cursor_open = 0;

	bdb_setkey(f, 0);
	p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	ret = DB_SEQ (p->cursor[0], DB_FIRST);
	bdb_close_cursor (f);
	if (!ret) {
		memcpy (p->last_readkey[0], p->key.data, (size_t)p->key.size);
		if (p->data.data != NULL
		 && p->data.size > 0
		 && p->data.size > f->record_max) {
			return COB_STATUS_39_CONFLICT_ATTRIBUTE;
		}
	} else {
		p->data.data = NULL;
	}

	f->open_mode = mode;
	if (f->flag_optional && nonexistent) {
		return COB_STATUS_05_SUCCESS_OPTIONAL;
	}
	return 0;

#else

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* Close the INDEXED file */

static int
indexed_close (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_close (f, opt);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;

	COB_UNUSED (opt);

	fh = f->file;
	if (fh == NULL) {
		return COB_STATUS_00_SUCCESS;
	}
	if (fh->isfd >= 0) {
		isfullclose (fh->isfd);
	}
	freefh (fh);
	f->file = NULL;
	return COB_STATUS_00_SUCCESS;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			i;

	COB_UNUSED (opt);

	p = f->file;
	if (bdb_env != NULL) {
		bdb_unlock_all (f);
		if(p->file_lock_set) {
			bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
			p->file_lock_set = 0;
		}
	}
	/* Close DB's */
	for (i = 0; i < (int)f->nkeys; ++i) {
		if (p->cursor[i]) {
			bdb_close_index (f, i);
		}
	}
	for (i = (int)f->nkeys - 1; i >= 0; --i) {
		if (p->db[i]) {
			DB_CLOSE (p->db[i]);
		}
		cob_free (p->last_readkey[i]);
		cob_free (p->last_readkey[f->nkeys + i]);
	}

	if (p->last_key) {
		cob_free (p->last_key);
	}
	cob_free (p->temp_key);
	cob_free (p->savekey);
	cob_free (p->suppkey);
	cob_free (p->saverec);
	cob_free (p->db);
	cob_free (p->last_readkey);
	cob_free (p->last_dupno);
	cob_free (p->rewrite_sec_key);
	cob_free (p->filename);
	cob_free (p->cursor);
	cob_free (p);

	return COB_STATUS_00_SUCCESS;

#else

	return COB_STATUS_91_NOT_AVAILABLE;

#endif
}


static void remove_ext(char* filename) {
	char* fname = strrchr(filename, '.');
	if (fname == NULL) {
		return;
	}
	else {
		int offset = fname - filename;
		filename[offset] = '\0';
	}
}


/* START INDEXED file with positioning */

static int
indexed_start (cob_file *f, const int cond, cob_field *key)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_start (f, cond, key);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			k;
	int			mode;
	int			klen,fullkeylen,partlen;
	int			savecond;

	fh = f->file;
	f->flag_read_done = 0;
	f->flag_first_read = 0;
	fh->readdone = 0;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	k = indexed_findkey(f, key, &fullkeylen, &partlen);
	if(k < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	/* Use size of data field; This may indicate a partial key */
	klen = partlen;
	if (klen < 1 || klen > fullkeylen) {
		/* Max key length for this index */
		klen = fullkeylen;
	}
	mode = ISGTEQ;
	fh->startiscur = 1;
	savecond = cond;
	switch (cond) {
	case COB_EQ:
		mode = ISEQUAL;
		fh->readdir = ISNEXT;
		break;
	case COB_GE:
		mode = ISGTEQ;
		fh->readdir = ISNEXT;
		break;
	case COB_GT:
		mode = ISGREAT;
		fh->readdir = ISNEXT;
		break;
	case COB_LE:
		mode = ISGTEQ;
		fh->readdir = ISPREV;
		break;
	case COB_LT:
		mode = ISGTEQ;
		fh->readdir = ISPREV;
		break;
	case COB_FI:
		mode = ISFIRST;
		fh->readdir = ISNEXT;
		break;
	case COB_LA:
		mode = ISLAST;
		fh->readdir = ISPREV;
		break;
	default:
		return COB_STATUS_21_KEY_INVALID;
	}
	if (isstart (fh->isfd, &fh->key[k], klen, (void *)f->record->data, mode)) {
		if (cond == COB_LE || cond == COB_LT) {
			if (isstart (fh->isfd, &fh->key[k], klen, (void *)f->record->data, ISLAST)) {
				fh->curkey = -1;
				fh->startcond = -1;
				fh->readdir = -1;
				fh->startiscur = 0;
				return fisretsts (COB_STATUS_23_KEY_NOT_EXISTS);
			} else {
				savecond = COB_LA;
			}
		} else {
			fh->curkey = -1;
			fh->startcond = -1;
			fh->readdir = -1;
			fh->startiscur = 0;
			return fisretsts (COB_STATUS_23_KEY_NOT_EXISTS);
		}
	}
	fh->startcond = savecond;
	indexed_savekey(fh, f->record->data, k);
	fh->curkey = k;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 1;
	return COB_STATUS_00_SUCCESS;

#elif	defined(WITH_DB)

	return indexed_start_internal (f, cond, key, 0, 0);

#else

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* Random READ of the INDEXED file  */

static int
indexed_read (cob_file *f, cob_field *key, const int read_opts)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_read (f, key, read_opts);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			k,fullkeylen,partlen;
	int			ret;
	int			lmode;

	fh = f->file;
	fh->eofpending = 0;
	fh->startiscur = 0;
	fh->wrkhasrec = 0;
	if (f->flag_nonexistent) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	k = indexed_findkey(f, key, &fullkeylen, &partlen);
	if(k < 0) {
		return COB_STATUS_23_KEY_NOT_EXISTS;
	}
	if (fh->curkey != (int)k) {
		/* Switch to this index */
		isstart (fh->isfd, &fh->key[k], 0,
			 (void *)f->record->data, ISEQUAL);
		fh->curkey = k;
		fh->wrkhasrec = 0;
	}
	fh->startcond = -1;
	lmode = 0;
	if (read_opts & COB_READ_LOCK) {
		lmode = ISLOCK;
	} else if (read_opts & COB_READ_WAIT_LOCK) {
		if (f->retry_mode == 0
		|| (f->retry_mode & COB_RETRY_FOREVER)) {
			lmode = ISLCKW;		/* ISAM library will wait FOREVER! */
		} else {
			lmode = ISLOCK;		/* isread_retry will handle the retries */
		}
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)
		&& (f->open_mode != COB_OPEN_INPUT) ) {
		lmode = ISLOCK;
	}
	if ((read_opts & COB_READ_IGNORE_LOCK)
	 || (read_opts & COB_READ_NO_LOCK) ) {
		lmode &= ~ISLOCK;
	}
	if ((fh->lmode & ISLOCK) && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	ISERRNO = 0;
	fh->readdir = -1;
	ret = COB_STATUS_00_SUCCESS;
	if (isread_retry (f, (void *)f->record->data, ISEQUAL | lmode)) {
		ret = fisretsts (COB_STATUS_21_KEY_INVALID);
	}
	if (unlikely (ret != 0)) {
		memset (fh->savekey, 0, fh->lenkey);
		fh->recnum = 0;
		fh->readdone = 0;
		return ret;
	}
	f->flag_first_read = 0;
	f->flag_read_done = 1;
	fh->readdone = 1;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	indexed_savekey(fh, f->record->data, 0);
	fh->recnum = ISRECNUM;
#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		f->record->size = ISRECLEN;
	}
#endif
	return 0;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;
	int			bdb_opts;
	int			test_lock;

	p = f->file;
	test_lock = 0;
	bdb_opts = read_opts;
	if (bdb_env != NULL) {
		if (read_opts & COB_READ_LOCK) {
			bdb_opts |= COB_READ_LOCK;
		} else if (read_opts & COB_READ_WAIT_LOCK) {
			if (f->retry_mode == 0
			|| (f->retry_mode & COB_RETRY_FOREVER)) {
				bdb_opts |= COB_READ_LOCK;
			} else {
				bdb_opts |= COB_READ_LOCK;
			}
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)
			&& (f->open_mode != COB_OPEN_INPUT) ) {
			bdb_opts |= COB_READ_LOCK;
		}
		if ((bdb_opts & COB_READ_IGNORE_LOCK)
		 || (bdb_opts & COB_READ_NO_LOCK) ) {
			bdb_opts &= ~COB_READ_LOCK;
		}
		if (f->open_mode != COB_OPEN_I_O
		 || f->flag_file_lock) {
			bdb_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) 
			&& !(bdb_opts & COB_READ_NO_LOCK)) {
			bdb_opts |= COB_READ_LOCK;
		}
		if ((bdb_opts & COB_READ_LOCK)
		 && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
			bdb_unlock_all (f);
		}
		test_lock = 1;
	} else {
		bdb_opts &= ~COB_READ_LOCK;
	}

	ret = indexed_start_internal (f, COB_EQ, key, bdb_opts, test_lock);
	if (ret != COB_STATUS_00_SUCCESS) {
		return ret;
	}

	f->record->size = p->data.size;
	if (f->record->size > f->record_max) {
		f->record->size = f->record_max;
		ret = COB_STATUS_43_READ_NOT_DONE;
	} else {
		ret = COB_STATUS_00_SUCCESS;
	}
	memcpy (f->record->data, p->data.data, f->record->size);

	return ret;

#else

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* Sequential READ of the INDEXED file */

static int
indexed_read_next (cob_file *f, const int read_opts)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_read_next (f, read_opts);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			ret;
	int			lmode, skip_read;
	int			domoveback;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	lmode = 0;

	if (fh->curkey == -1) {
		/* Switch to primary index */
		isstart (fh->isfd, &fh->key[0], 0, NULL, ISFIRST);
		fh->curkey = 0;
		fh->readdir = ISNEXT;
		fh->startcond = -1;
		fh->startiscur = 0;
		fh->wrkhasrec = 0;
	}
	if (read_opts & COB_READ_LOCK) {
		lmode = ISLOCK;
	} else if (read_opts & COB_READ_WAIT_LOCK) {
		lmode = ISLCKW;
	} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) &&
		   f->open_mode != COB_OPEN_INPUT) {
		if (!(read_opts & COB_READ_IGNORE_LOCK)) {
			lmode = ISLOCK;
		}
	}
#ifdef	ISSKIPLOCK
	if ((f->retry_mode & COB_ADVANCING_LOCK)
	 || (read_opts & COB_READ_ADVANCING_LOCK)) {
		lmode |= ISSKIPLOCK;
	}
#endif
	if ((read_opts & COB_READ_IGNORE_LOCK)) {
		lmode &= ~ISLOCK;
	}

	if ((fh->lmode & ISLOCK) && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	skip_read = ISNEXT;

	ISERRNO = 0;
	ret = COB_STATUS_00_SUCCESS;
	switch (read_opts & COB_READ_MASK) {
	case COB_READ_NEXT:
		fh->readdir = ISNEXT;
		if (fh->eofpending == ISNEXT) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (fh->startcond == COB_LA) {
				skip_read = ISPREV;
				if (isread_retry (f, (void *)f->record->data, ISLAST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (fh->startcond == COB_FI) {
				if (isread_retry (f, (void *)f->record->data, ISFIRST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (isread (fh->isfd, (void *)f->record->data, ISCURR)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			} else {
				switch (fh->startcond) {
				case COB_GE:
					domoveback = 0;
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, ISERRNO == 0 ? ISNEXT : ISFIRST);
					}
					break;
				case COB_LE:
					domoveback = 0;
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, ISERRNO == 0 ? ISPREV : ISLAST);
					}
					break;
				case COB_LT:
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
					}
					break;
				case COB_GT:
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) <= 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				}
				if (isread_retry (f, (void *)f->record->data, ISCURR | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISNEXT) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			if (fh->lmode & ISLOCK) {
				/* Now lock 'peek ahead' record */
				if (isread_retry (f, (void *)f->record->data, ISCURR | fh->lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISPREV) {
				isread (fh->isfd, (void *)f->record->data, ISNEXT);
				fh->wrkhasrec = 0;
			}
			if (isread_retry (f, (void *)f->record->data, ISNEXT | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_PREVIOUS:
		skip_read = ISPREV;
		fh->readdir = ISPREV;
		if (fh->eofpending == ISPREV) {
			fh->eofpending = 0;
			fh->wrkhasrec = 0;
			return COB_STATUS_10_END_OF_FILE;
		}
		if (fh->startiscur) {
			if (fh->startcond == COB_FI) {
				if (isread_retry (f, (void *)f->record->data, ISFIRST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (fh->startcond == COB_LA) {
				skip_read = ISPREV;
				if (isread_retry (f, (void *)f->record->data, ISLAST | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			} else if (isread_retry (f, (void *)f->record->data, ISCURR | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			} else {
				switch (fh->startcond) {
				case COB_LE:
					domoveback = 0;
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) == 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
						domoveback = 1;
					}
					if (domoveback) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
						skip_read = ISPREV;
					}
					break;
				case COB_LT:
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) >= 0) {
						isread (fh->isfd, (void *)f->record->data, ISPREV);
						skip_read = ISPREV;
					}
					break;
				case COB_GT:
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) <= 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				case COB_GE:
					while (ISERRNO == 0
					&& indexed_cmpkey(fh, f->record->data, fh->curkey, 0) < 0) {
						isread (fh->isfd, (void *)f->record->data, ISNEXT);
					}
					break;
				}
				if (isread_retry (f, (void *)f->record->data, ISCURR | lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
			fh->startcond = -1;
			fh->startiscur = 0;
		} else if (fh->wrkhasrec == ISPREV) {
			memcpy (f->record->data, fh->recwrk, f->record_max);
			if (fh->lmode & ISLOCK) {
				/* Now lock 'peek ahead' record */
				if (isread_retry (f, (void *)f->record->data, ISCURR | fh->lmode)) {
					ret = fisretsts (COB_STATUS_10_END_OF_FILE);
				}
			}
		} else {
			if (fh->wrkhasrec == ISNEXT) {
				isread (fh->isfd, (void *)f->record->data, ISPREV);
				fh->wrkhasrec = 0;
			}
			skip_read = ISPREV;
			if (isread_retry (f, (void *)f->record->data, ISPREV | lmode)) {
				ret = fisretsts (COB_STATUS_10_END_OF_FILE);
			}
		}
		break;
	case COB_READ_FIRST:
		fh->readdir = ISNEXT;
		if (isread_retry (f, (void *)f->record->data, ISFIRST | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	case COB_READ_LAST:
		skip_read = ISPREV;
		fh->readdir = ISPREV;
		if (isread_retry (f, (void *)f->record->data, ISLAST | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	default:
		fh->readdir = ISNEXT;
		if (isread_retry (f, (void *)f->record->data, ISNEXT | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
		break;
	}
	while (ret == COB_STATUS_51_RECORD_LOCKED
	&& ((f->retry_mode & COB_ADVANCING_LOCK)
	 || (read_opts & COB_READ_ADVANCING_LOCK))) {
		ret = COB_STATUS_00_SUCCESS;
		if (isread_retry (f, (void *)f->record->data, skip_read | lmode)) {
			ret = fisretsts (COB_STATUS_10_END_OF_FILE);
		}
	}
	if (unlikely (ret != 0)) {
		memset (fh->savekey, 0, fh->lenkey);
		fh->recnum = 0;
		fh->readdone = 0;
		fh->wrkhasrec = 0;
		return ret;
	}
	fh->eofpending = 0;
	f->flag_first_read = 0;
	f->flag_read_done = 1;
	fh->readdone = 1;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	indexed_savekey(fh, f->record->data, 0);
	fh->recnum = ISRECNUM;
#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		f->record->size = ISRECLEN;
	}
#endif

#ifdef	COB_WITH_STATUS_02
	if((isstat1 == '0') && (isstat2 == '2')) {
		return COB_STATUS_02_SUCCESS_DUPLICATE;
	}
#endif
	return 0;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;
	int			read_nextprev,skip_lock;
	cob_u32_t		nextprev;
	int			file_changed;
	int			bdb_opts;
	unsigned int		dupno;

	p = f->file;
	nextprev = DB_NEXT;
	dupno = 0;
	file_changed = 0;

	bdb_opts = read_opts;
	skip_lock = 0;
	if (bdb_env != NULL) {
		if (f->open_mode != COB_OPEN_I_O 
		 || f->flag_file_lock) {
			bdb_opts &= ~COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC) 
			&& !(bdb_opts & COB_READ_NO_LOCK)) {
			bdb_opts |= COB_READ_LOCK;
		}
		if ((f->retry_mode & COB_ADVANCING_LOCK)
		|| (read_opts & COB_READ_ADVANCING_LOCK)) {
			bdb_opts |= COB_READ_LOCK;
			skip_lock = 1;
		} else if (read_opts & COB_READ_LOCK) {
			bdb_opts |= COB_READ_LOCK;
		} else if (read_opts & COB_READ_WAIT_LOCK) {
			bdb_opts |= COB_READ_LOCK;
		} else if ((f->lock_mode & COB_LOCK_AUTOMATIC)
			&&  f->open_mode != COB_OPEN_INPUT) {
			if ((read_opts & COB_READ_IGNORE_LOCK)) {
				bdb_opts &= ~COB_READ_LOCK;
			} else {
				bdb_opts |= COB_READ_LOCK;
			}
		}
		if ((bdb_opts & COB_READ_LOCK)
		 && !(f->lock_mode & COB_LOCK_MULTIPLE)) {
			bdb_unlock_all (f);
		}
	} else {
		bdb_opts &= ~COB_READ_LOCK;
	}

	if (unlikely (bdb_opts & COB_READ_PREVIOUS)) {
		if (f->flag_end_of_file) {
			nextprev = DB_LAST;
		} else {
			nextprev = DB_PREV;
		}
	} else if (f->flag_begin_of_file) {
		nextprev = DB_FIRST;
	}
	/* The open cursor makes this function atomic */
	if (p->key_index != 0) {
		p->db[0]->cursor (p->db[0], NULL, &p->cursor[0], 0);
	}
	p->db[p->key_index]->cursor (p->db[p->key_index], NULL, &p->cursor[p->key_index], 0);

	if (f->flag_first_read) {
		/* Data is read in indexed_open or indexed_start */
		if (p->data.data == NULL
		 || (f->flag_first_read == 2 && nextprev == DB_PREV)) {
			bdb_close_index (f, p->key_index);
			if (p->key_index != 0) {
				bdb_close_cursor (f);
			}
			return COB_STATUS_10_END_OF_FILE;
		}
		/* Check if previously read data still exists */
		p->key.size = (cob_dbtsize_t) bdb_keylen(f,p->key_index);
		p->key.data = p->last_readkey[p->key_index];
		ret = DB_SEQ (p->cursor[p->key_index], DB_SET);
		if (!ret && p->key_index > 0) {
			if (f->keys[p->key_index].tf_duplicates) {
				memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
				dupno = COB_DUPSWAP (dupno);
				while (ret == 0
				   && memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0
				   && dupno < p->last_dupno[p->key_index]) {
					ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
					memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
					dupno = COB_DUPSWAP (dupno);
				}
				if (ret == 0
				 && memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0
				 && dupno == p->last_dupno[p->key_index]) {
					ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, p->primekeylen);
				} else {
					ret = 1;
				}
			} else {
				ret = memcmp (p->last_readkey[p->key_index + f->nkeys], p->data.data, p->primekeylen);
			}
			if (!ret) {
				p->key.size = (cob_dbtsize_t) p->primekeylen;
				p->key.data = p->last_readkey[p->key_index + f->nkeys];
				ret = DB_GET (p->db[0], 0);
			}
		}
		file_changed = ret;
		if (bdb_env != NULL && !file_changed) {
			if (skip_lock
			 && !(bdb_opts & COB_READ_IGNORE_LOCK)
			 && !(bdb_opts & COB_READ_LOCK)) {
				ret = bdb_test_lock_advance (f, nextprev, skip_lock);
				if (ret) {
					bdb_close_index (f, p->key_index);
					bdb_close_cursor (f);
					return ret;
				}
			}
			if (bdb_opts & COB_READ_LOCK) {
				ret = bdb_lock_advance (f, nextprev, skip_lock);
				if (ret) {
					bdb_close_index (f, p->key_index);
					bdb_close_cursor (f);
					if (ret == DB_NOTFOUND)
						return COB_STATUS_10_END_OF_FILE;
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
	}
	if (!f->flag_first_read || file_changed) {
		if (nextprev == DB_FIRST || nextprev == DB_LAST) {
			read_nextprev = 1;
		} else {
			p->key.size = (cob_dbtsize_t) bdb_keylen(f,p->key_index);
			p->key.data = p->last_readkey[p->key_index];
			ret = DB_SEQ (p->cursor[p->key_index], DB_SET_RANGE);
			/* ret != 0 possible, records may be deleted since last read */
			if (ret != 0) {
				if (nextprev == DB_PREV) {
					nextprev = DB_LAST;
					read_nextprev = 1;
				} else {
					bdb_close_index (f, p->key_index);
					if (p->key_index != 0) {
						bdb_close_cursor (f);
					}
					return COB_STATUS_10_END_OF_FILE;
				}
			} else {
				if (memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0) {
					if (p->key_index > 0 && f->keys[p->key_index].tf_duplicates) {
						memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
						dupno = COB_DUPSWAP (dupno);
						while (ret == 0
						 && memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0
						 && dupno < p->last_dupno[p->key_index]) {
							ret = DB_SEQ (p->cursor[p->key_index], DB_NEXT);
							memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
							dupno = COB_DUPSWAP (dupno);
						}
						if (ret != 0) {
							if (nextprev == DB_PREV) {
								nextprev = DB_LAST;
								read_nextprev = 1;
							} else {
								bdb_close_index (f, p->key_index);
								if (p->key_index != 0) {
									bdb_close_cursor (f);
								}
								return COB_STATUS_10_END_OF_FILE;
							}
						} else {
							if (memcmp (p->key.data, p->last_readkey[p->key_index], (size_t)p->key.size) == 0 &&
								dupno == p->last_dupno[p->key_index]) {
								read_nextprev = 1;
							} else {
								if (nextprev == DB_PREV) {
									read_nextprev = 1;
								} else {
									read_nextprev = 0;
								}
							}
						}
					} else {
						read_nextprev = 1;
					}
				} else {
					if (nextprev == DB_PREV) {
						read_nextprev = 1;
					} else {
						read_nextprev = 0;
					}
				}
			}
		}
		if (read_nextprev) {
			ret = DB_SEQ (p->cursor[p->key_index], nextprev);
			if (ret != 0) {
				bdb_close_index (f, p->key_index);
				if (p->key_index != 0) {
					bdb_close_cursor (f);
				}
				return COB_STATUS_10_END_OF_FILE;
			}
		}

		if (p->key_index > 0) {
			/* Temporarily save alternate key */
			memcpy (p->temp_key, p->key.data, (size_t)p->key.size);
			if (f->keys[p->key_index].tf_duplicates) {
				memcpy (&dupno, (cob_u8_ptr)p->data.data + p->primekeylen, sizeof (unsigned int));
				dupno = COB_DUPSWAP (dupno);
			}
			p->key.data = p->data.data;
			p->key.size = p->primekeylen;
			ret =  DB_GET (p->db[0], 0);
			if (ret != 0) {
				bdb_close_index (f, p->key_index);
				bdb_close_cursor (f);
				return COB_STATUS_23_KEY_NOT_EXISTS;
			}
		}
		if (bdb_env != NULL) {
			if (skip_lock
			&& !(bdb_opts & COB_READ_IGNORE_LOCK)) {
				ret = bdb_test_lock_advance (f, nextprev, skip_lock);
				if (ret) {
					bdb_close_index (f, p->key_index);
					bdb_close_cursor (f);
					return ret;
				}
			}
			if (bdb_opts & COB_READ_LOCK) {
				ret = bdb_lock_advance (f, nextprev, skip_lock);
				if (ret != 0) {
					bdb_close_index (f, p->key_index);
					bdb_close_cursor (f);
					if (ret == DB_NOTFOUND)
						return COB_STATUS_10_END_OF_FILE;
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
		}
		if (p->key_index == 0) {
			memcpy (p->last_readkey[0], p->key.data, (size_t)p->key.size);
		} else {
			memcpy (p->last_readkey[p->key_index], p->temp_key,
				    bdb_keylen(f,p->key_index));
			memcpy (p->last_readkey[p->key_index + f->nkeys], p->key.data, p->primekeylen);
			if (f->keys[p->key_index].tf_duplicates) {
				p->last_dupno[p->key_index] = dupno;
			}
		}
	}

	bdb_close_index (f, p->key_index);
	if (p->key_index != 0) {
		bdb_close_cursor (f);
	}

	f->record->size = p->data.size;
	if (f->record->size > f->record_max) {
		f->record->size = f->record_max;
		ret = COB_STATUS_43_READ_NOT_DONE;
	} else {
		ret = COB_STATUS_00_SUCCESS;
	}
	memcpy (f->record->data, p->data.data, f->record->size);

	return ret;

#else

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}


/* WRITE to the INDEXED file  */

static int
indexed_write (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_write (f, opt);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			ret = 0;

	fh = f->file;
	if (f->flag_nonexistent) {
		return COB_STATUS_48_OUTPUT_DENIED;
	}
	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	&&  indexed_cmpkey(fh, f->record->data, 0, 0) <= 0) {
		return COB_STATUS_21_KEY_INVALID;
	}

#ifdef	ISVARLEN
	if (f->record_min != f->record_max) {
		ISRECLEN = f->record->size;
	}
#endif
	if ((opt & COB_WRITE_LOCK)
	 && !(f->lock_mode & COB_LOCK_AUTOMATIC) 
	 && !f->flag_file_lock) {
		/* WRITE and make it 'current' */
		if (unlikely(iswrcurr (fh->isfd, (void *)f->record->data))) {
			return fisretsts (COB_STATUS_49_I_O_DENIED);
		}
#ifdef	COB_WITH_STATUS_02
		if((isstat1 == '0') && (isstat2 == '2')) {
			ret = COB_STATUS_02_SUCCESS_DUPLICATE;
		}
#endif
		/* Then read placing lock on the record */
		if (isread_retry (f, (void *)f->record->data, ISCURR | ISLOCK)) {
			return fisretsts (COB_STATUS_49_I_O_DENIED);
		}
	} else {
	if (unlikely (iswrite (fh->isfd, (void *)f->record->data))) {
		return fisretsts (COB_STATUS_49_I_O_DENIED);
	}
#ifdef	COB_WITH_STATUS_02
	if((isstat1 == '0') && (isstat2 == '2')) {
			ret = COB_STATUS_02_SUCCESS_DUPLICATE;
	}
#endif
	}
	indexed_savekey(fh, f->record->data, 0);

	return ret;

#elif	defined(WITH_DB)

	struct indexed_file	*p;
	int			ret;

	if (f->flag_nonexistent) {
		return COB_STATUS_48_OUTPUT_DENIED;
	}
	p = f->file;
	if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
		bdb_unlock_all (f);
	}

	/* Check record key */
	bdb_setkey (f, 0);
	if (!p->last_key) {
		p->last_key = cob_malloc ((size_t)p->maxkeylen);
	} else if (f->access_mode == COB_ACCESS_SEQUENTIAL &&
		   memcmp (p->last_key, p->key.data, (size_t)p->key.size) > 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	memcpy (p->last_key, p->key.data, (size_t)p->key.size);

	ret =  indexed_write_internal (f, 0, opt);
	bdb_close_cursor (f);
	return ret;

#else

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}


/* DELETE record from the INDEXED file  */

static int
indexed_delete (cob_file *f)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_delete (f);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	int			ret;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	if (fh->curkey == -1) {
		/* Switch to primary index */
		isstart (fh->isfd, &fh->key[0], 0,
			 (void *)f->record->data, ISEQUAL);
		fh->curkey = 0;
		fh->readdir = ISNEXT;
	} else {
		savefileposition (f);
		if (fh->curkey != 0) {
			/* Switch to primary index */
			isstart (fh->isfd, &fh->key[0], 0,
				 (void *)f->record->data, ISEQUAL);
		}
	}
	if (isread_retry (f, (void *)f->record->data, ISEQUAL | ISLOCK)) {
		ret = fisretsts (COB_STATUS_21_KEY_INVALID);
	} else if (isdelete (fh->isfd, (void *)f->record->data)) {
		ret = fisretsts (COB_STATUS_49_I_O_DENIED);
	}
	restorefileposition (f);
	if ( !(f->lock_mode & COB_LOCK_MULTIPLE)) {
		isrelease (fh->isfd);
	}
	return ret;

#elif	defined(WITH_DB)
	int			ret;

	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	ret = indexed_delete_internal (f, 0, 0);
	bdb_close_cursor (f);
	return ret;

#else

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}

/* REWRITE record to the INDEXED file  */

static int
indexed_rewrite (cob_file *f, const int opt)
{
#ifdef	WITH_INDEX_EXTFH

	return extfh_indexed_rewrite (f, opt);

#elif	defined(WITH_ANY_ISAM)

	struct indexfile	*fh;
	size_t			k;
	int			ret;

	fh = f->file;
	ret = COB_STATUS_00_SUCCESS;
	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL
	&&  indexed_cmpkey(fh, f->record->data, 0, 0) != 0) {
		return COB_STATUS_21_KEY_INVALID;
	}
	if (fh->curkey >= 0) {
		/* Index is active */
		/* Save record data */
		memcpy (fh->recwrk, f->record->data, f->record_max);
		fh->readdir = ISNEXT;
		savefileposition (f);
		memcpy (fh->recwrk, f->record->data, f->record_max);
		if (fh->curkey != 0) {
			/* Activate primary index */
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk, ISEQUAL);
		}
		/* Verify record exists */
		if (isread (fh->isfd, (void *)fh->recwrk, ISEQUAL)) {
			restorefileposition (f);
			return COB_STATUS_21_KEY_INVALID;
		}
		for (k = 1; k < f->nkeys && ret == COB_STATUS_00_SUCCESS; ++k) {
			if (fh->key[k].k_flags & ISDUPS) {
				continue;
			}
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[k], fh->key[k].k_leng,
				 (void *)fh->recwrk, ISEQUAL);
			if (!isread (fh->isfd, (void *)fh->recwrk, ISEQUAL)
			 && ISRECNUM != fh->recnum) {
				ret = COB_STATUS_22_KEY_EXISTS;
				break;
			}
		}
		if (ret == COB_STATUS_00_SUCCESS) {
			memcpy (fh->recwrk, f->record->data, f->record_max);
			isstart (fh->isfd, &fh->key[0], 0, (void *)fh->recwrk, ISEQUAL);
			if (isread_retry (f, (void *)fh->recwrk, ISEQUAL | ISLOCK)) {
				ret = fisretsts (COB_STATUS_49_I_O_DENIED);
			} else {
#ifdef	ISVARLEN
				if (f->record_min != f->record_max) {
					ISRECLEN = f->record->size;
				}
#endif
				if (isrewcurr (fh->isfd, (void *)f->record->data)) {
					ret = fisretsts (COB_STATUS_49_I_O_DENIED);
				}
			}
		}

#ifdef	COB_WITH_STATUS_02
		if(!ret && (isstat1 == '0') && (isstat2 == '2')) {
			ret = COB_STATUS_02_SUCCESS_DUPLICATE;
		}
#endif
		restorefileposition (f);

	} else {

	memcpy (fh->recwrk, f->record->data, f->record_max);
		if (isread_retry (f, (void *)fh->recwrk, ISEQUAL | ISLOCK)) {
		ret = fisretsts (COB_STATUS_49_I_O_DENIED);
	} else {
#ifdef	ISVARLEN
		if (f->record_min != f->record_max) {
			ISRECLEN = f->record->size;
		}
#endif
		if (isrewrite (fh->isfd, (void *)f->record->data)) {
			ret = fisretsts (COB_STATUS_49_I_O_DENIED);
		}
#ifdef	COB_WITH_STATUS_02
			if((isstat1 == '0') && (isstat2 == '2')) {
				ret = COB_STATUS_02_SUCCESS_DUPLICATE;
		}
#endif
	}
	}

	if (ret == COB_STATUS_00_SUCCESS
	 || ret == COB_STATUS_02_SUCCESS_DUPLICATE) {
		if ((f->lock_mode & COB_LOCK_AUTOMATIC)) {
			if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
			isrelease (fh->isfd);
		}
		} else {
			if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
				if (!(opt & COB_WRITE_LOCK)) {
					isrelease (fh->isfd);
				}
			} else
			if ((opt & COB_WRITE_NO_LOCK)) {
				isrelease (fh->isfd);
		}
		}
	} else if (ret) {
		isrelease (fh->isfd);
	}
	return ret;

#elif	defined(WITH_DB)

	int			ret;

	if (f->flag_nonexistent) {
		return COB_STATUS_49_I_O_DENIED;
	}
	if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
		bdb_unlock_all (f);
	}

	/* Check duplicate alternate keys */
	if (check_alt_keys (f, 1)) {
		return COB_STATUS_22_KEY_EXISTS;
	}

	/* Delete the current record */
	ret = indexed_delete_internal (f, 1, opt);

	if (ret != COB_STATUS_00_SUCCESS) {
		bdb_close_cursor (f);
		return ret;
	}

	/* Write data */
	bdb_setkey(f, 0);
	ret = indexed_write_internal (f, 1, opt);
	bdb_close_cursor (f);

	if (ret == COB_STATUS_00_SUCCESS
	 || ret == COB_STATUS_02_SUCCESS_DUPLICATE) {
		if ((f->lock_mode & COB_LOCK_AUTOMATIC)) {
			if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
				bdb_unlock_all (f);
			}
		} else {
			if (!(f->lock_mode & COB_LOCK_MULTIPLE)) {
				if (!(opt & COB_WRITE_LOCK)) {
					bdb_unlock_all (f);
				}
			} else
			if ((opt & COB_WRITE_LOCK)) {
				bdb_unlock_last (f);
			} else
			if ((opt & COB_WRITE_NO_LOCK)) {
				bdb_unlock_all (f);
			}
		}
	} else if (ret) {
		bdb_unlock_all (f);
	}
	return ret;

#else

	return COB_STATUS_91_NOT_AVAILABLE;
#endif
}


static void
cob_file_unlock (cob_file *f)
{
#ifdef	WITH_DB
	struct indexed_file	*p;
#elif	defined(WITH_ANY_ISAM)
	struct indexfile	*fh;
#endif

	if (COB_FILE_SPECIAL(f)) {
		return;
	}

	if (f->open_mode != COB_OPEN_CLOSED &&
	    f->open_mode != COB_OPEN_LOCKED) {
		if (f->organization == COB_ORG_SORT) {
			return;
		}
		if (f->organization != COB_ORG_INDEXED) {
#ifndef	WITH_SEQRA_EXTFH
			if (f->fd >= 0) {
				fdcobsync (f->fd);
			}
			if (f->flag_file_lock) {
				/* Unlock the file */
				unlock_record (f, 0);
			}

#endif
		} else {
#ifdef	WITH_INDEX_EXTFH
			extfh_indexed_unlock (f);
#elif	defined(WITH_DB)
			p = f->file;
			if (bdb_env != NULL && p) {
				bdb_unlock_all (f);
				if(p->file_lock_set) {
					bdb_env->lock_put (bdb_env, &p->bdb_file_lock);
					p->file_lock_set = 0;
				}
			}
#elif	defined(WITH_ANY_ISAM)
			fh = f->file;
			if (fh) {
				isrelease (fh->isfd);
			}
#endif
		}
	}
}

/* Global functions */

/*
 * Allocate memory for 'IS EXTERNAL' cob_file
 */
void
cob_file_external_addr (const char *exname,
		cob_file **pfl, cob_file_key **pky,
		const int nkeys, const int linage)
{
	cob_file	*fl;
	fl = cob_external_addr (exname, sizeof (cob_file));
	if (fl->file_version == 0)
		fl->file_version = COB_FILE_VERSION;

	if (nkeys > 0
	 && fl->keys == NULL) {
		fl->keys = cob_cache_malloc (sizeof (cob_file_key) * nkeys);
	}

	if (pky != NULL) {
		*pky = fl->keys;
	}

	if (linage > 0
	 && fl->linorkeyptr == NULL) {
		fl->linorkeyptr = cob_cache_malloc (sizeof (cob_linage));
	}
	*pfl = fl;
}

/*
 * Allocate memory for cob_file
 */
void
cob_file_malloc (cob_file **pfl, cob_file_key **pky,
		 const int nkeys, const int linage)
{
	cob_file	*fl;
	fl = cob_cache_malloc (sizeof (cob_file));
	fl->file_version = COB_FILE_VERSION;

	if (nkeys > 0
	 && pky != NULL) {
		*pky = fl->keys = cob_cache_malloc (sizeof (cob_file_key) * nkeys);
	}

	if (linage > 0) {
		fl->linorkeyptr = cob_cache_malloc (sizeof (cob_linage));
	}
	*pfl = fl;
}

/*
 * Free memory for cob_file
 */
void
cob_file_free (cob_file **pfl, cob_file_key **pky)
{
	cob_file	*fl;
	if (pky != NULL) {
		if (*pky != NULL) {
			cob_cache_free (*pky);
			*pky = NULL;
		}
	}
	if (pfl != NULL && *pfl != NULL) {
		fl = *pfl;
		if (fl->linorkeyptr) {
			cob_cache_free (fl->linorkeyptr);
			fl->linorkeyptr = NULL;
		}
		if (*pfl != NULL) {
			cob_cache_free (*pfl);
			*pfl = NULL;
		}
	}
}


void
cob_unlock_file (cob_file *f, cob_field *fnstatus)
{
	cob_file_unlock (f);
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

void
cob_open (cob_file *f, const int mode, const int sharing, cob_field *fnstatus)
{
	if (f->file_version != COB_FILE_VERSION) {
		cob_runtime_error (_("ERROR FILE %s does not match current version; Recompile the program"),
							f->select_name);
		cob_stop_run (1);
	}

	f->last_operation = COB_LAST_OPEN;
	f->flag_read_done = 0;

	/* File was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		save_status (f, fnstatus, COB_STATUS_38_CLOSED_WITH_LOCK);
		return;
	}

	/* File is already open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_41_ALREADY_OPEN);
		return;
	}

	f->last_open_mode = mode;
	f->flag_nonexistent = 0;
	f->flag_end_of_file = 0;
	f->flag_begin_of_file = 0;
	f->flag_first_read = 2;
	f->flag_operation = 0;
	f->lock_mode &= ~COB_LOCK_OPEN_EXCLUSIVE;
	f->share_mode = sharing;
	f->record_off = 0;

	if (unlikely (COB_FILE_STDIN (f))) {
		if (mode != COB_OPEN_INPUT) {
			save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
			return;
		}
		f->file = stdin;
		f->fd = fileno (stdin);
		f->open_mode = mode;
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}
	if (unlikely (COB_FILE_STDOUT (f))) {
		if (mode != COB_OPEN_OUTPUT) {
			save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
			return;
		}
		f->file = stdout;
		f->fd = fileno (stdout);
		f->open_mode = mode;
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}

	if (f->assign == NULL) {
		cob_runtime_error (_("ERROR FILE %s has ASSIGN field is NULL"),
							f->select_name);
		save_status (f, fnstatus, COB_STATUS_31_INCONSISTENT_FILENAME);
		return;
	}
	if (f->assign->data == NULL) {
		cob_runtime_error (_("ERROR FILE %s has ASSIGN field with NULL address"),
							f->select_name);
		save_status (f, fnstatus, COB_STATUS_31_INCONSISTENT_FILENAME);
		return;
	}

	/* Obtain the file name */
	cob_field_to_string (f->assign, file_open_name, (size_t)COB_FILE_MAX);

	cob_cache_file (f);

	/* Open the file */
	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->open (f, file_open_name,
								mode, sharing));
}

void
cob_close (cob_file *f, cob_field *fnstatus, const int opt, const int remfil)
{
	struct file_list	*l;
	struct file_list	*m;
	int			ret;

	f->last_operation = COB_LAST_CLOSE;
	f->flag_read_done = 0;
	f->flag_operation = 0;
	f->record_off = 0;

	f->lock_mode &= ~COB_LOCK_OPEN_EXCLUSIVE;

	if (COB_FILE_SPECIAL (f)) {
		f->open_mode = COB_OPEN_CLOSED;
		f->file = NULL;
		f->fd = -1;
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}

	if (unlikely (remfil)) {
		/* Remove from cache - Needed for CANCEL */
		/* Setting m silences false compiler warning */
		m = file_cache;
		for (l = file_cache; l; l = l->next) {
			if (f == l->file) {
				if (l == file_cache) {
					file_cache = l->next;
				} else {
					m->next = l->next;
				}
				cob_free (l);
				break;
			}
			m = l;
		}
	}

	if (f->open_mode == COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_42_NOT_OPEN);
		return;
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
		ret = fileio_funcs[(int)f->organization]->close (f, opt);
	}

	if (ret == COB_STATUS_00_SUCCESS) {
		switch (opt) {
		case COB_CLOSE_LOCK:
			f->open_mode = COB_OPEN_LOCKED;
			break;
		default:
			f->open_mode = COB_OPEN_CLOSED;
			break;
		}
	}

	save_status (f, fnstatus, ret);
}

#if	0	/* RXWRXW - unlock */
void
cob_unlock (cob_file *f)
{
	int	ret;

	f->flag_read_done = 0;

	if (f->open_mode == COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_42_NOT_OPEN);
		return;
	}

	if (f->flag_nonexistent) {
		ret = COB_STATUS_00_SUCCESS;
	} else {
		ret = fileio_funcs[(int)f->organization]->close (f, opt);
	}

	save_status (f, fnstatus, ret);
}
#endif

void
cob_start (cob_file *f, const int cond, cob_field *key,
	   cob_field *keysize, cob_field *fnstatus)
{
	int		ret;
	int		size;
	cob_field	tempkey;

	f->last_operation = COB_LAST_START;
	f->last_key = key;
	f->flag_read_done = 0;
	f->flag_first_read = 0;

	if (unlikely (f->open_mode != COB_OPEN_I_O
		       && f->open_mode != COB_OPEN_INPUT)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (unlikely (f->access_mode == COB_ACCESS_RANDOM)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (f->flag_nonexistent) {
		save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
		return;
	}

	size = 0;
	if (unlikely (keysize)) {
		size = cob_get_int (keysize);
		if (size < 1 || size > (int)key->size) {
			save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
			return;
		}
		tempkey = *key;
		tempkey.size = (size_t)size;
		f->last_key = &tempkey;
		ret = fileio_funcs[(int)f->organization]->start (f, cond, &tempkey);
	} else {
		ret = fileio_funcs[(int)f->organization]->start (f, cond, key);
	}
	if (ret == COB_STATUS_00_SUCCESS) {
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		f->flag_first_read = 1;
	} else {
		f->flag_end_of_file = 1;
		f->flag_begin_of_file = 0;
		f->flag_first_read = 1;
	}

	save_status (f, fnstatus, ret);
}

void
cob_read (cob_file *f, cob_field *key, cob_field *fnstatus, const int read_opts)
{
	int	ret;

	f->flag_read_done = 0;
	f->last_operation = COB_LAST_READ;
	f->last_key = key;

	if (unlikely(f->open_mode != COB_OPEN_INPUT &&
		     f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (unlikely (f->flag_nonexistent)) {
		if (f->flag_first_read == 0) {
			save_status (f, fnstatus, COB_STATUS_23_KEY_NOT_EXISTS);
			return;
		}
		f->flag_first_read = 0;
		save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}

	/* Sequential read at the end of file is an error */
	if (key == NULL) {
		f->last_operation = COB_LAST_READ_SEQ;
		if (unlikely(f->flag_end_of_file &&
			     !(read_opts & COB_READ_PREVIOUS))) {
			save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		if (unlikely(f->flag_begin_of_file &&
			     (read_opts & COB_READ_PREVIOUS))) {
			save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		ret = fileio_funcs[(int)f->organization]->read_next (f, read_opts);
	} else {
		ret = fileio_funcs[(int)f->organization]->read (f, key, read_opts);
	}

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
	case COB_STATUS_02_SUCCESS_DUPLICATE:
		f->flag_first_read = 0;
		f->flag_read_done = 1;
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		if (f->variable_record) {
			cob_set_int (f->variable_record, (int) f->record->size);
		}
		break;
	case COB_STATUS_10_END_OF_FILE:
		if (read_opts & COB_READ_PREVIOUS) {
			f->flag_begin_of_file = 1;
		} else {
			f->flag_end_of_file = 1;
		}
		break;
	}

	save_status (f, fnstatus, ret);
}

void
cob_read_next (cob_file *f, cob_field *fnstatus, const int read_opts)
{
	int	ret;

	f->last_operation = COB_LAST_READ_SEQ;
	f->flag_read_done = 0;

	if (unlikely (f->open_mode != COB_OPEN_INPUT
		       && f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_47_INPUT_DENIED);
		return;
	}

	if (unlikely (f->flag_nonexistent)) {
		if (f->flag_first_read == 0) {
			save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
			return;
		}
		f->flag_first_read = 0;
		save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}

	/* Sequential read at the end of file is an error */
	if (unlikely (f->flag_end_of_file && !(read_opts & COB_READ_PREVIOUS))) {
		save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
		return;
	}
	if (unlikely (f->flag_begin_of_file && (read_opts & COB_READ_PREVIOUS))) {
		save_status (f, fnstatus, COB_STATUS_46_READ_ERROR);
		return;
	}

	ret = fileio_funcs[(int)f->organization]->read_next (f, read_opts);

	switch (ret) {
	case COB_STATUS_00_SUCCESS:
	case COB_STATUS_02_SUCCESS_DUPLICATE:
		f->flag_first_read = 0;
		f->flag_read_done = 1;
		f->flag_end_of_file = 0;
		f->flag_begin_of_file = 0;
		if (f->variable_record) {
			cob_set_int (f->variable_record, (int) f->record->size);
		}
		break;
	case COB_STATUS_10_END_OF_FILE:
		if (read_opts & COB_READ_PREVIOUS) {
			f->flag_begin_of_file = 1;
		} else {
			f->flag_end_of_file = 1;
		}
		break;
	}

	save_status (f, fnstatus, ret);
}

void
cob_write (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus,
	   const unsigned int check_eop)
{
	f->last_operation = COB_LAST_WRITE;
	f->last_key = NULL;
	f->flag_read_done = 0;

	if (f->access_mode == COB_ACCESS_SEQUENTIAL) {
		if (unlikely (f->open_mode != COB_OPEN_OUTPUT
			       && f->open_mode != COB_OPEN_EXTEND)) {
			save_status (f, fnstatus, COB_STATUS_48_OUTPUT_DENIED);
			return;
		}
	} else {
		if (unlikely (f->open_mode != COB_OPEN_OUTPUT
			       && f->open_mode != COB_OPEN_I_O)) {
			save_status (f, fnstatus, COB_STATUS_48_OUTPUT_DENIED);
			return;
		}
	}

	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (unlikely (f->record->size > rec->size)) {
			f->record->size = rec->size;
		}
	} else {
		f->record->size = rec->size;
	}

	if (f->record->size < f->record_min || f->record_max < f->record->size) {
		save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
		return;
	}

	check_eop_status = check_eop;
	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->write (f, opt));
	f->flag_begin_of_file = 0;
}

void
cob_rewrite (cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus)
{
	int	read_done;

	read_done = f->flag_read_done;
	f->flag_read_done = 0;
	f->last_operation = COB_LAST_REWRITE;
	f->last_key = NULL;

	if (unlikely (f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_49_I_O_DENIED);
		return;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
		save_status (f, fnstatus, COB_STATUS_43_READ_NOT_DONE);
		return;
	}

	if (unlikely (f->organization == COB_ORG_SEQUENTIAL)) {
		if (f->record->size != rec->size) {
			save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
			return;
		}

		if (f->variable_record) {
			if (f->record->size != (size_t)cob_get_int (f->variable_record)) {
				save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
				return;
			}
		}
	}

	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (unlikely(f->record->size > rec->size)) {
			f->record->size = rec->size;
		}
		if (f->record->size < f->record_min || f->record_max < f->record->size) {
			save_status (f, fnstatus, COB_STATUS_44_RECORD_OVERFLOW);
			return;
		}
	}

	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->rewrite (f, opt));
}

void
cob_delete (cob_file *f, cob_field *fnstatus)
{
	int	read_done;

	read_done = f->flag_read_done;
	f->flag_read_done = 0;
	f->last_operation = COB_LAST_DELETE;

	if (unlikely (f->open_mode != COB_OPEN_I_O)) {
		save_status (f, fnstatus, COB_STATUS_49_I_O_DENIED);
		return;
	}

	if (f->access_mode == COB_ACCESS_SEQUENTIAL && !read_done) {
		save_status (f, fnstatus, COB_STATUS_43_READ_NOT_DONE);
		return;
	}

	save_status (f, fnstatus,
		     fileio_funcs[(int)f->organization]->fdelete (f));
}

void
cob_commit (void)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (l->file) {
			cob_file_unlock (l->file);
		}
	}
}

void
cob_rollback (void)
{
	struct file_list	*l;

	for (l = file_cache; l; l = l->next) {
		if (l->file) {
			cob_file_unlock (l->file);
		}
	}
}

void
cob_delete_file (cob_file *f, cob_field *fnstatus)
{
	f->last_operation = COB_LAST_DELETE_FILE;
	if (f->organization == COB_ORG_SORT) {
		save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
		return;
	}

	/* File was previously closed with lock */
	if (f->open_mode == COB_OPEN_LOCKED) {
		save_status (f, fnstatus, COB_STATUS_38_CLOSED_WITH_LOCK);
		return;
	}

	/* File is open */
	if (f->open_mode != COB_OPEN_CLOSED) {
		save_status (f, fnstatus, COB_STATUS_41_ALREADY_OPEN);
		return;
	}

	if (unlikely(COB_FILE_STDIN (f))) {
		save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
		return;
	}
	if (unlikely(COB_FILE_STDOUT (f))) {
		save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
		return;
	}

	/* Obtain the file name */
	cob_field_to_string (f->assign, file_open_name, (size_t)COB_FILE_MAX);
	cob_chk_file_mapping (f);

	if (f->organization != COB_ORG_INDEXED) {
#ifdef	WITH_SEQRA_EXTFH
		save_status (f, fnstatus, COB_STATUS_91_NOT_AVAILABLE);
		return;
#else
		unlink (file_open_name);
#endif
	} else {
#ifdef	WITH_INDEX_EXTFH
		save_status (f, fnstatus, COB_STATUS_91_NOT_AVAILABLE);
		return;
#else
		indexed_file_delete (f, file_open_name);
#endif
	}
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

/* System routines */

static void *
cob_str_from_fld (const cob_field *f)
{
	void		*mptr;
	unsigned char	*s;
	size_t		i, n, j;
#if	0	/* Quotes in file */
	int		quote_switch;

	quote_switch = 0;
#endif

	if (!f) {
		return cob_malloc ((size_t)1);
	}
	for (i = f->size - 1; i > 0; --i) {
		if (f->data[i] != ' ' && f->data[i] != 0) {
			i++;
			break;
		}
	}
	/* i is 0 or > 0 */
	mptr = cob_malloc (i + 1);
	s = mptr;
	j = 0;
	for (n = 0; n < i; ++n) {
		if (f->data[n] == '"') {
			continue;
		}
		s[j++] = f->data[n];
#if	0	/* Quotes in file */
		if (f->data[n] == '"') {
			quote_switch = !quote_switch;
			continue;
		}
		s[j] = f->data[n];
		if (quote_switch) {
			j++;
			continue;
		}
		if (s[j] == ' ' || s[j] == 0) {
			s[j] = 0;
			break;
		}
		j++;
#endif
	}
	return mptr;
}

static int
open_cbl_file (unsigned char *file_name, unsigned char *file_access,
	       unsigned char *file_handle, const int file_flags)
{
	char	*fn;
	int	flag = O_BINARY;
	int	fd;

	COB_UNUSED (file_name);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		memset (file_handle, -1, (size_t)4);
		return -1;
	}
	flag |= file_flags;
	switch (*file_access & 0x3F) {
		case 1:
			flag |= O_RDONLY;
			break;
		case 2:
			flag |= O_CREAT | O_TRUNC | O_WRONLY;
			break;
		case 3:
			flag |= O_RDWR;
			break;
		default:
			cob_runtime_warning (_("call to CBL_OPEN_FILE with wrong access mode: %d"), *file_access & 0x3F);
			memset (file_handle, -1, (size_t)4);
			return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	fd = open (fn, flag, COB_FILE_MODE);
	if (fd < 0) {
		cob_free (fn);
		memset (file_handle, -1, (size_t)4);
		return 35;
	}
	cob_free (fn);
	memcpy (file_handle, &fd, (size_t)4);
	return 0;
}

int
cob_sys_open_file (unsigned char *file_name, unsigned char *file_access,
		   unsigned char *file_lock, unsigned char *file_dev,
		   unsigned char *file_handle)
{
	COB_UNUSED (file_lock);
	COB_UNUSED (file_dev);

	COB_CHK_PARMS (CBL_OPEN_FILE, 5);

#ifdef	WORDS_BIGENDIAN
	/* if value is passed as numeric literal, it becomes an 'int' so value is in 4th byte */
	if (file_access[0] == 0
	 && file_access[1] == 0
	 && file_access[2] == 0)
		file_access += 3;
	if (file_lock[0] == 0
	 && file_lock[1] == 0
	 && file_lock[2] == 0)
		file_lock += 3;
	if (file_dev[0] == 0
	 && file_dev[1] == 0
	 && file_dev[2] == 0)
		file_dev += 3;
#endif

	return open_cbl_file (file_name, file_access, file_handle, 0);
}

int
cob_sys_create_file (unsigned char *file_name, unsigned char *file_access,
		     unsigned char *file_lock, unsigned char *file_dev,
		     unsigned char *file_handle)
{
	/*
	 * @param: file_access : 1 (read-only), 2 (write-only), 3 (both)
	 * @param: file_lock : not implemented, set 0
	 * @param: file_dev : not implemented, set 0
	 */

	COB_CHK_PARMS (CBL_CREATE_FILE, 5);

#ifdef	WORDS_BIGENDIAN
	/* if value is passed as numeric literal, it becomes an 'int' so value is in 4th byte */
	if (file_access[0] == 0
	 && file_access[1] == 0
	 && file_access[2] == 0)
		file_access += 3;
	if (file_lock[0] == 0
	 && file_lock[1] == 0
	 && file_lock[2] == 0)
		file_lock += 3;
	if (file_dev[0] == 0
	 && file_dev[1] == 0
	 && file_dev[2] == 0)
		file_dev += 3;
#endif

	if (*file_lock != 0) {
		cob_runtime_warning (_("call to CBL_CREATE_FILE with wrong file_lock: %d"), *file_lock);
	}
	if (*file_dev != 0) {
		cob_runtime_warning (_("call to CBL_CREATE_FILE with wrong file_dev: %d"), *file_dev);
	}

	return open_cbl_file (file_name, file_access, file_handle, O_CREAT | O_TRUNC);
}

int
cob_sys_read_file (unsigned char *file_handle, unsigned char *file_offset,
		   unsigned char *file_len, unsigned char *flags,
		   unsigned char *buf)
{
	cob_s64_t	off;
	int		fd;
	int		len;
	int		rc;
	struct stat	st;

	COB_CHK_PARMS (CBL_READ_FILE, 5);

	rc = 0;
	memcpy (&fd, file_handle, (size_t)4);
	memcpy (&off, file_offset, (size_t)8);
	memcpy (&len, file_len, (size_t)4);
#ifndef	WORDS_BIGENDIAN
	off = COB_BSWAP_64 (off);
	len = COB_BSWAP_32 (len);
#endif
	if (lseek (fd, (off_t)off, SEEK_SET) == (off_t)-1) {
		return -1;
	}
	if (len > 0) {
		rc = read (fd, buf, (size_t)len);
		if (rc < 0) {
			rc = -1;
		} else if (rc == 0) {
			rc = 10;
		} else {
			rc = 0;
		}
	}
	if ((*flags & 0x80) != 0) {
		if (fstat (fd, &st) < 0) {
			return -1;
		}
		off = st.st_size;
#ifndef	WORDS_BIGENDIAN
		off = COB_BSWAP_64 (off);
#endif
		memcpy (file_offset, &off, (size_t)8);
	}
	return rc;
}

int
cob_sys_write_file (unsigned char *file_handle, unsigned char *file_offset,
		    unsigned char *file_len, unsigned char *flags,
		    unsigned char *buf)
{
	cob_s64_t	off;
	int		fd;
	int		len;
	int		rc;

	COB_UNUSED (flags);

	COB_CHK_PARMS (CBL_WRITE_FILE, 5);

	memcpy (&fd, file_handle, (size_t)4);
	memcpy (&off, file_offset, (size_t)8);
	memcpy (&len, file_len, (size_t)4);
#ifndef	WORDS_BIGENDIAN
	off = COB_BSWAP_64 (off);
	len = COB_BSWAP_32 (len);
#endif
	if (lseek (fd, (off_t)off, SEEK_SET) == (off_t)-1) {
		return -1;
	}
	rc = (int) write (fd, buf, (size_t)len);
	if (rc != len) {
		return COB_STATUS_30_PERMANENT_ERROR;
	}
	return COB_STATUS_00_SUCCESS;
}

int
cob_sys_close_file (unsigned char *file_handle)
{
	int	fd;

	COB_CHK_PARMS (CBL_CLOSE_FILE, 1);

	memcpy (&fd, file_handle, (size_t)4);
	return close (fd);
}

int
cob_sys_flush_file (unsigned char *file_handle)
{
	COB_UNUSED (file_handle);

	COB_CHK_PARMS (CBL_FLUSH_FILE, 1);

	return 0;
}

int
cob_sys_delete_file (unsigned char *file_name)
{
	char	*fn;
	int	ret;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (CBL_DELETE_FILE, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	ret = unlink (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_copy_file (unsigned char *fname1, unsigned char *fname2)
{
	char	*fn1;
	char	*fn2;
	int	flag = O_BINARY;
	int	ret;
	int	i;
	int	fd1, fd2;

	COB_UNUSED (fname1);
	COB_UNUSED (fname2);

	COB_CHK_PARMS (CBL_COPY_FILE, 2);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return -1;
	}
	fn1 = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	flag |= O_RDONLY;
	fd1 = open (fn1, flag, 0);
	if (fd1 < 0) {
		cob_free (fn1);
		return -1;
	}
	cob_free (fn1);
	fn2 = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[1]);
	flag &= ~O_RDONLY;
	flag |= O_CREAT | O_TRUNC | O_WRONLY;
	fd2 = open (fn2, flag, COB_FILE_MODE);
	if (fd2 < 0) {
		close (fd1);
		cob_free (fn2);
		return -1;
	}
	cob_free (fn2);

	ret = 0;
	while ((i = read (fd1, file_open_buff, COB_FILE_BUFF)) > 0) {
		if (write (fd2, file_open_buff, (size_t)i) != (size_t)i) {
			ret = -1;
			break;
		}
	}
	close (fd1);
	close (fd2);
	return ret;
}

int
cob_sys_check_file_exist (unsigned char *file_name, unsigned char *file_info)
{
	char		*fn;
	struct tm	*tm;
	cob_s64_t	sz;
	struct stat	st;
	short		y;
	short		d, m, hh, mm, ss;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (CBL_CHECK_FILE_EXIST, 2);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return -1;
	}
	if (COB_MODULE_PTR->cob_procedure_params[1]->size < 16U) {
		cob_runtime_error (_("'%s' - File detail area is too short"), "CBL_CHECK_FILE_EXIST");
		cob_stop_run (1);
	}

	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	if (stat (fn, &st) < 0) {
		cob_free (fn);
		return 35;
	}
	cob_free (fn);
	sz = (cob_s64_t)st.st_size;
	tm = localtime (&st.st_mtime);
	d = (short)tm->tm_mday;
	m = (short)(tm->tm_mon + 1);
	y = (short)(tm->tm_year + 1900);
	hh = (short)tm->tm_hour;
	mm = (short)tm->tm_min;
	/* Leap seconds ? */
	if (tm->tm_sec >= 60) {
		ss = 59;
	} else {
		ss = (short)tm->tm_sec;
	}

#ifndef	WORDS_BIGENDIAN
	sz = COB_BSWAP_64 (sz);
	y = COB_BSWAP_16 (y);
#endif
	memcpy (file_info, &sz, (size_t)8);
	file_info[8] = (unsigned char)d;
	file_info[9] = (unsigned char)m;
	memcpy (file_info+10, &y, (size_t)2);
	file_info[12] = (unsigned char)hh;
	file_info[13] = (unsigned char)mm;
	file_info[14] = (unsigned char)ss;
	file_info[15] = 0;
	return 0;
}

int
cob_sys_rename_file (unsigned char *fname1, unsigned char *fname2)
{
	char	*fn1;
	char	*fn2;
	int	ret;

	COB_UNUSED (fname1);
	COB_UNUSED (fname2);

	COB_CHK_PARMS (CBL_RENAME_FILE, 2);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return -1;
	}
	fn1 = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	fn2 = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[1]);
	ret = rename (fn1, fn2);
	cob_free (fn1);
	cob_free (fn2);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_get_current_dir (const int flags, const int dir_length,
			 unsigned char *dir)
{
	char	*dirname;
	int	dir_size;
	int	has_space;

	COB_CHK_PARMS (CBL_GET_CURRENT_DIR, 3);

	if (dir_length < 1) {
		return 128;
	}
	if (flags) {
		return 129;
	}
	memset (dir, ' ', (size_t)dir_length);
	dirname = getcwd (NULL, (size_t)0);
	if (dirname == NULL) {
		return 128;
	}
	dir_size = (int) strlen (dirname);
	has_space = 0;
	if (strchr (dirname, ' ')) {
		has_space = 2;
	}
	if (dir_size + has_space > dir_length) {
		cob_free (dirname);
		return 128;
	}
	if (has_space) {
		*dir = '"';
		memcpy (&dir[1], dirname, (size_t)dir_size);
		dir[dir_size + 1] = '"';
	} else {
		memcpy (dir, dirname, (size_t)dir_size);
	}
	cob_free (dirname);
	return 0;
}

int
cob_sys_create_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_CREATE_DIR, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
#ifdef	_WIN32
	ret = mkdir (fn);
#else
	ret = mkdir (fn, 0770);
#endif
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_change_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_CHANGE_DIR, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	ret = chdir (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_delete_dir (unsigned char *dir)
{
	char	*fn;
	int	ret;

	COB_UNUSED (dir);

	COB_CHK_PARMS (CBL_DELETE_DIR, 1);

	if (!COB_MODULE_PTR->cob_procedure_params[0]) {
		return -1;
	}
	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	ret = rmdir (fn);
	cob_free (fn);
	if (ret) {
		return 128;
	}
	return 0;
}

int
cob_sys_mkdir (unsigned char *dir)
{
	int		ret;

	COB_CHK_PARMS (C$MAKEDIR, 1);

	ret = cob_sys_create_dir (dir);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

int
cob_sys_chdir (unsigned char *dir, unsigned char *status)
{
	int		ret;

	COB_UNUSED (status);

	COB_CHK_PARMS (C$CHDIR, 2);

	ret = cob_sys_change_dir (dir);
	if (ret < 0) {
		ret = 128;
	}
	cob_set_int (COB_MODULE_PTR->cob_procedure_params[1], ret);
	return ret;
}

int
cob_sys_copyfile (unsigned char *fname1, unsigned char *fname2,
		  unsigned char *file_type)
{
	int		ret;

	/* RXW - Type is not yet evaluated */
	COB_UNUSED (file_type);

	COB_CHK_PARMS (C$COPY, 3);

	if (cobglobptr->cob_call_params < 3) {
		return 128;
	}
	ret = cob_sys_copy_file (fname1, fname2);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

int
cob_sys_file_info (unsigned char *file_name, unsigned char *file_info)
{
	char			*fn;
	struct tm		*tm;
	cob_u64_t		sz;
	unsigned int		dt;
	short			y;
	short			d, m, hh, mm, ss;
	struct stat		st;

	COB_UNUSED (file_name);

	COB_CHK_PARMS (C$FILEINFO, 2);

	if (cobglobptr->cob_call_params < 2 ||
	    !COB_MODULE_PTR->cob_procedure_params[0]) {
		return 128;
	}
	if (!COB_MODULE_PTR->cob_procedure_params[1]) {
		return 128;
	}
	if (COB_MODULE_PTR->cob_procedure_params[1]->size < 16U) {
		cob_runtime_error (_("'%s' - File detail area is too short"), "C$FILEINFO");
		cob_stop_run (1);
	}

	fn = cob_str_from_fld (COB_MODULE_PTR->cob_procedure_params[0]);
	if (stat (fn, &st) < 0) {
		cob_free (fn);
		return 35;
	}
	cob_free (fn);
	sz = (cob_u64_t)st.st_size;
	tm = localtime (&st.st_mtime);
	d = (short)tm->tm_mday;
	m = (short)(tm->tm_mon + 1);
	y = (short)(tm->tm_year + 1900);
	hh = (short)tm->tm_hour;
	mm = (short)tm->tm_min;
	/* Leap seconds ? */
	if (tm->tm_sec >= 60) {
		ss = 59;
	} else {
		ss = (short)tm->tm_sec;
	}

#ifndef	WORDS_BIGENDIAN
	sz = COB_BSWAP_64 (sz);
#endif
	memcpy (file_info, &sz, (size_t)8);
	dt = (y * 10000) + (m * 100) + d;
#ifndef	WORDS_BIGENDIAN
	dt = COB_BSWAP_32 (dt);
#endif
	memcpy (file_info + 8, &dt, (size_t)4);
	dt = (hh * 1000000) + (mm * 10000) + (ss * 100);
#ifndef	WORDS_BIGENDIAN
	dt = COB_BSWAP_32 (dt);
#endif
	memcpy (file_info + 12, &dt, (size_t)4);
	return 0;
}

int
cob_sys_file_delete (unsigned char *file_name, unsigned char *file_type)
{
	int	ret;

	/* RXW - Type is not yet evaluated */
	COB_UNUSED (file_type);

	COB_CHK_PARMS (C$DELETE, 2);

	if (cobglobptr->cob_call_params < 2 ||
	    !COB_MODULE_PTR->cob_procedure_params[0]) {
		return 128;
	}
	ret = cob_sys_delete_file (file_name);
	if (ret < 0) {
		ret = 128;
	}
	return ret;
}

/* SORT */

static int
sort_cmps (const unsigned char *s1, const unsigned char *s2, const size_t size,
	   const unsigned char *col)
{
	size_t			i;
	int			ret;

	if (unlikely (col)) {
		for (i = 0; i < size; ++i) {
			if ((ret = col[s1[i]] - col[s2[i]]) != 0) {
				return ret;
			}
		}
	} else {
		for (i = 0; i < size; ++i) {
			if ((ret = s1[i] - s2[i]) != 0) {
				return ret;
			}
		}
	}
	return 0;
}

static COB_INLINE void
unique_copy (unsigned char *s1, const unsigned char *s2)
{
	size_t	size;

	size = sizeof (size_t);
	do {
		*s1++ = *s2++;
	} while (--size);
}

static int
cob_file_sort_compare (struct cobitem *k1, struct cobitem *k2, void *pointer)
{
	cob_file	*f;
	size_t		i;
	size_t		u1;
	size_t		u2;
	int		cmp;
	cob_field	f1;
	cob_field	f2;

	f = pointer;
	for (i = 0; i < f->nkeys; ++i) {
		f1 = f2 = *(f->keys[i].field);
		f1.data = k1->item + f->keys[i].offset;
		f2.data = k2->item + f->keys[i].offset;
		if (unlikely (COB_FIELD_IS_NUMERIC (&f1))) {
			cmp = cob_numeric_cmp (&f1, &f2);
		} else {
			cmp = sort_cmps (f1.data, f2.data, f1.size,
					 f->sort_collating);
		}
		if (cmp != 0) {
			return (f->keys[i].flag == COB_ASCENDING) ? cmp : -cmp;
		}
	}
	unique_copy ((unsigned char *)&u1, k1->unique);
	unique_copy ((unsigned char *)&u2, k2->unique);
	if (u1 < u2) {
		return -1;
	}
	return 1;
}

static void
cob_free_list (struct cobsort *hp)
{
	struct sort_mem_struct	*s1;
	struct sort_mem_struct	*s2;

	s1 = hp->mem_base;
	for (; s1;) {
		s2 = s1;
		s1 = s1->next;
		cob_free (s2->mem_ptr);
		cob_free (s2);
	}
}

static struct cobitem *
cob_new_item (struct cobsort *hp, const size_t size)
{
	struct cobitem		*q;
	struct sort_mem_struct	*s;

	COB_UNUSED (size);

	/* Creation of an empty item */
	if (unlikely (hp->empty != NULL)) {
		q = hp->empty;
		hp->empty = q->next;
		q->block_byte = 0;
		q->next = NULL;
		q->end_of_block = 0;
		return (void *)q;
	}
	if (unlikely ((hp->mem_used + hp->alloc_size) > hp->mem_size)) {
		s = cob_fast_malloc (sizeof (struct sort_mem_struct));
		s->mem_ptr = cob_fast_malloc (hp->chunk_size);
		s->next = hp->mem_base;
		hp->mem_base = s;
		hp->mem_size = hp->chunk_size;
		hp->mem_total += hp->chunk_size;
		hp->mem_used = 0;
	}
	q = (struct cobitem *)(hp->mem_base->mem_ptr + hp->mem_used);
	hp->mem_used += hp->alloc_size;
	if (unlikely (hp->mem_total >= cobsetptr->cob_sort_memory)) {
		if ((hp->mem_used + hp->alloc_size) > hp->mem_size) {
			hp->switch_to_file = 1;
		}
	}
	q->block_byte = 0;
	q->next = NULL;
	q->end_of_block = 0;
	return q;
}

static FILE *
cob_srttmpfile (void)
{
	FILE		*fp;
	char		*filename;
	int		fd;

	filename = cob_malloc ((size_t)COB_FILE_BUFF);
	cob_temp_name (filename, NULL);
	cob_incr_temp_iteration ();
#ifdef	_WIN32
	fd = open (filename,
		    _O_CREAT | _O_TRUNC | _O_RDWR | _O_BINARY | _O_TEMPORARY,
		    _S_IREAD | _S_IWRITE);
#else
	fd = open (filename, O_CREAT | O_TRUNC | O_RDWR | O_BINARY, COB_FILE_MODE);
#endif
	if (fd < 0) {
		cob_free (filename);
		return NULL;
	}
	(void)unlink (filename);
	fp = fdopen (fd, "w+b");
	if (!fp) {
		close (fd);
	}
	cob_free (filename);
	return fp;
}

static int
cob_get_sort_tempfile (struct cobsort *hp, const int n)
{
	if (hp->file[n].fp == NULL) {
		hp->file[n].fp = cob_srttmpfile ();
		if (hp->file[n].fp == NULL) {
			cob_runtime_error (_("SORT is unable to acquire temporary file"));
			cob_stop_run (1);
		}
	} else {
		rewind (hp->file[n].fp);
	}
	hp->file[n].count = 0;
	return hp->file[n].fp == NULL;
}

static int
cob_sort_queues (struct cobsort *hp)
{
	struct cobitem	*q;
	int		source;
	int		destination;
	int		move;
	int		n;
	int		end_of_block[2];

	source = 0;
	while (hp->queue[source + 1].count != 0) {
		destination = source ^ 2;
		hp->queue[destination].first = NULL;
		hp->queue[destination].count = 0;
		hp->queue[destination + 1].first = NULL;
		hp->queue[destination + 1].count = 0;
		for (;;) {
			end_of_block[0] = hp->queue[source].count == 0;
			end_of_block[1] = hp->queue[source + 1].count == 0;
			if (end_of_block[0] && end_of_block[1]) {
				break;
			}
			while (!end_of_block[0] || !end_of_block[1]) {
				if (end_of_block[0]) {
					move = 1;
				} else if (end_of_block[1]) {
					move = 0;
				} else {
					n = cob_file_sort_compare
						(hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
					move = n < 0 ? 0 : 1;
				}
				q = hp->queue[source + move].first;
				if (q->end_of_block) {
					end_of_block[move] = 1;
				}
				hp->queue[source + move].first = q->next;
				if (hp->queue[destination].first == NULL) {
					hp->queue[destination].first = q;
				} else {
					hp->queue[destination].last->next = q;
				}
				hp->queue[destination].last = q;
				hp->queue[source + move].count--;
				hp->queue[destination].count++;
				q->next = NULL;
				q->end_of_block = 0;
			}
			hp->queue[destination].last->end_of_block = 1;
			destination ^= 1;
		}
		source = destination & 2;
	}
	return source;
}

static int
cob_read_item (struct cobsort *hp, const int n)
{
	FILE	*fp;

	fp = hp->file[n].fp;
	if (getc (fp) != 0) {
		hp->queue[n].first->end_of_block = 1;
	} else {
		hp->queue[n].first->end_of_block = 0;
		/* LCOV_EXCL_START */
		if (unlikely (fread (hp->queue[n].first->unique,
				hp->r_size, (size_t)1, fp) != 1)) {
			return 1;
		}
		/* LCOV_EXCL_STOP */
	}
	return 0;
}

static int
cob_write_block (struct cobsort *hp, const int n)
{
	struct cobitem	*q;
	FILE		*fp;

	fp = hp->file[hp->destination_file].fp;
	for (;;) {
		q = hp->queue[n].first;
		if (q == NULL) {
			break;
		}
		/* LCOV_EXCL_START */
		if (unlikely (fwrite (&(q->block_byte),
				hp->w_size, (size_t)1, fp) != 1)) {
			return 1;
		}
		/* LCOV_EXCL_STOP */
		hp->queue[n].first = q->next;
		q->next = hp->empty;
		hp->empty = q;
	}
	hp->queue[n].count = 0;
	hp->file[hp->destination_file].count++;
	/* LCOV_EXCL_START */
	if (unlikely (putc (1, fp) != 1)) {
		return 1;
	}
	/* LCOV_EXCL_STOP */
	return 0;
}

static void
cob_copy_check (cob_file *to, cob_file *from)
{
	unsigned char	*toptr;
	unsigned char	*fromptr;
	size_t		tosize;
	size_t		fromsize;

	toptr = to->record->data;
	fromptr = from->record->data;
	tosize = to->record->size;
	fromsize = from->record->size;
	if (unlikely (tosize > fromsize)) {
		memcpy (toptr, fromptr, fromsize);
		memset (toptr + fromsize, ' ', tosize - fromsize);
	} else {
		memcpy (toptr, fromptr, tosize);
	}
}

static int
cob_file_sort_process (struct cobsort *hp)
{
	int	i;
	int	source;
	int	destination;
	int	n;
	int	move;
	int	res;

	hp->retrieving = 1;
	n = cob_sort_queues (hp);
#if	0	/* RXWRXW - Cannot be true */
	/* LCOV_EXCL_START */
	if (unlikely (n < 0)) {
		return COBSORTABORT;
	}
	/* LCOV_EXCL_STOP */
#endif
	if (likely(!hp->files_used)) {
		hp->retrieval_queue = n;
		return 0;
	}
	/* LCOV_EXCL_START */
	if (unlikely (cob_write_block (hp, n))) {
		return COBSORTFILEERR;
	}
	/* LCOV_EXCL_STOP */
	for (i = 0; i < 4; ++i) {
		hp->queue[i].first = hp->empty;
		hp->empty = hp->empty->next;
		hp->queue[i].first->next = NULL;
	}
	rewind (hp->file[0].fp);
	rewind (hp->file[1].fp);
	/* LCOV_EXCL_START */
	if (unlikely (cob_get_sort_tempfile (hp, 2))) {
		return COBSORTFILEERR;
	}
	if (unlikely (cob_get_sort_tempfile (hp, 3))) {
		return COBSORTFILEERR;
	}
	/* LCOV_EXCL_STOP */
	source = 0;
	while (hp->file[source].count > 1) {
		destination = source ^ 2;
		hp->file[destination].count = 0;
		hp->file[destination + 1].count = 0;
		while (hp->file[source].count > 0) {
			/* LCOV_EXCL_START */
			if (unlikely (cob_read_item (hp, source))) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			if (hp->file[source + 1].count > 0) {
				/* LCOV_EXCL_START */
				if (unlikely (cob_read_item (hp, source + 1))) {
					return COBSORTFILEERR;
				}
				/* LCOV_EXCL_STOP */
			} else {
				hp->queue[source + 1].first->end_of_block = 1;
			}
			while (!hp->queue[source].first->end_of_block ||
			       !hp->queue[source + 1].first->end_of_block) {
				if (hp->queue[source].first->end_of_block) {
					move = 1;
				} else if (hp->queue[source + 1].first->end_of_block) {
					move = 0;
				} else {
					res = cob_file_sort_compare
						(hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
					move = res < 0 ? 0 : 1;
				}
				/* LCOV_EXCL_START */
				if (unlikely (fwrite (
				    &(hp->queue[source + move].first->block_byte),
				    hp->w_size, (size_t)1,
				    hp->file[destination].fp) != 1)) {
					return COBSORTFILEERR;
				}
				if (unlikely(cob_read_item (hp, source + move))) {
					return COBSORTFILEERR;
				}
				/* LCOV_EXCL_STOP */
			}
			hp->file[destination].count++;
			/* LCOV_EXCL_START */
			if (unlikely (putc (1, hp->file[destination].fp) != 1)) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			hp->file[source].count--;
			hp->file[source + 1].count--;
			destination ^= 1;
		}
		source = destination & 2;
		rewind (hp->file[0].fp);
		rewind (hp->file[1].fp);
		rewind (hp->file[2].fp);
		rewind (hp->file[3].fp);
	}
	hp->retrieval_queue = source;
	/* LCOV_EXCL_START */
	if (unlikely (cob_read_item (hp, source))) {
		return COBSORTFILEERR;
	}
	if (unlikely (cob_read_item (hp, source + 1))) {
		return COBSORTFILEERR;
	}
	/* LCOV_EXCL_STOP */
	return 0;
}

static int
cob_file_sort_submit (cob_file *f, const unsigned char *p)
{
	struct cobsort		*hp;
	struct cobitem		*q;
	struct queue_struct	*z;
	int			n;

	hp = f->file;
	if (unlikely (!hp)) {
		return COBSORTNOTOPEN;
	}
	if (unlikely (hp->retrieving)) {
		return COBSORTABORT;
	}
	if (unlikely (hp->switch_to_file)) {
		if (!hp->files_used) {
			/* LCOV_EXCL_START */
			if (unlikely (cob_get_sort_tempfile (hp, 0))) {
				return COBSORTFILEERR;
			}
			if (unlikely (cob_get_sort_tempfile (hp, 1))) {
				return COBSORTFILEERR;
			}
			/* LCOV_EXCL_STOP */
			hp->files_used = 1;
			hp->destination_file = 0;
		}
		n = cob_sort_queues (hp);
#if	0	/* RXWRXW - Cannot be true */
		/* LCOV_EXCL_START */
		if (unlikely (n < 0)) {
			return COBSORTABORT;
		}
		/* LCOV_EXCL_STOP */
#endif
		/* LCOV_EXCL_START */
		if (unlikely (cob_write_block (hp, n))) {
			return COBSORTFILEERR;
		}
		/* LCOV_EXCL_STOP */
		hp->destination_file ^= 1;
	}
	q = cob_new_item (hp, sizeof (struct cobitem) + hp->size);
	q->end_of_block = 1;
	unique_copy (q->unique, (const unsigned char *)&(hp->unique));
	hp->unique++;
	memcpy (q->item, p, hp->size);
	if (hp->queue[0].count <= hp->queue[1].count) {
		z = &hp->queue[0];
	} else {
		z = &hp->queue[1];
	}
	q->next = z->first;
	z->first = q;
	z->count++;
	return 0;
}

static int
cob_file_sort_retrieve (cob_file *f, unsigned char *p)
{
	struct cobsort		*hp;
	struct cobitem		*next;
	struct queue_struct	*z;
	int			move;
	int			source;
	int			res;

	hp = f->file;
	if (unlikely (!hp)) {
		return COBSORTNOTOPEN;
	}
	if (unlikely (!hp->retrieving)) {
		res = cob_file_sort_process (hp);
		if (res) {
			return res;
		}
	}
	if (unlikely (hp->files_used)) {
		source = hp->retrieval_queue;
		if (hp->queue[source].first->end_of_block) {
			if (hp->queue[source + 1].first->end_of_block) {
				return COBSORTEND;
			}
			move = 1;
		} else if (hp->queue[source + 1].first->end_of_block) {
			move = 0;
		} else {
			res = cob_file_sort_compare (hp->queue[source].first,
						hp->queue[source + 1].first,
						hp->pointer);
			move = res < 0 ? 0 : 1;
		}
		memcpy (p, hp->queue[source + move].first->item, hp->size);
		/* LCOV_EXCL_START */
		if (unlikely (cob_read_item (hp, source + move))) {
			return COBSORTFILEERR;
		}
		/* LCOV_EXCL_STOP */
	} else {
		z = &hp->queue[hp->retrieval_queue];
		if (z->first == NULL) {
			return COBSORTEND;
		}
		memcpy (p, z->first->item, hp->size);
		next = z->first->next;
		z->first->next = hp->empty;
		hp->empty = z->first;
		z->first = next;
	}
	return 0;
}

void
cob_file_sort_using (cob_file *sort_file, cob_file *data_file)
{
	int		ret;

	cob_open (data_file, COB_OPEN_INPUT, 0, NULL);
	for (;;) {
		cob_read_next (data_file, NULL, COB_READ_NEXT);
		if (data_file->file_status[0] != '0') {
			break;
		}
		cob_copy_check (sort_file, data_file);
		ret = cob_file_sort_submit (sort_file, sort_file->record->data);
		if (ret) {
			break;
		}
	}
	cob_close (data_file, NULL, COB_CLOSE_NORMAL, 0);
}

void
cob_file_sort_giving (cob_file *sort_file, const size_t varcnt, ...)
{
	cob_file	**fbase;
	struct cobsort	*hp;
	size_t		i;
	int		ret;
	int		opt;
	va_list		args;

	fbase = cob_malloc (varcnt * sizeof (cob_file *));
	va_start (args, varcnt);
	for (i = 0; i < varcnt; ++i) {
		fbase[i] = va_arg (args, cob_file *);
	}
	va_end (args);
	for (i = 0; i < varcnt; ++i) {
		cob_open (fbase[i], COB_OPEN_OUTPUT, 0, NULL);
	}
	for (;;) {
		ret = cob_file_sort_retrieve (sort_file, sort_file->record->data);
		if (ret) {
			if (ret == COBSORTEND) {
				sort_file->file_status[0] = '1';
				sort_file->file_status[1] = '0';
			} else {
				hp = sort_file->file;
				if (hp->sort_return) {
					*(int *)(hp->sort_return) = 16;
				}
				sort_file->file_status[0] = '3';
				sort_file->file_status[1] = '0';
			}
			break;
		}
		for (i = 0; i < varcnt; ++i) {
			if (COB_FILE_SPECIAL (fbase[i]) ||
			    fbase[i]->organization == COB_ORG_LINE_SEQUENTIAL) {
				opt = COB_WRITE_BEFORE | COB_WRITE_LINES | 1;
			} else {
				opt = 0;
			}
			fbase[i]->record->size = fbase[i]->record_max;
			cob_copy_check (fbase[i], sort_file);
			cob_write (fbase[i], fbase[i]->record, opt, NULL, 0);
		}
	}
	for (i = 0; i < varcnt; ++i) {
		cob_close (fbase[i], NULL, COB_CLOSE_NORMAL, 0);
	}
	cob_free (fbase);
}

void
cob_file_sort_init (cob_file *f, const unsigned int nkeys,
		    const unsigned char *collating_sequence,
		    void *sort_return, cob_field *fnstatus)
{
	struct cobsort	*p;
	size_t		n;

	p = cob_malloc (sizeof (struct cobsort));
	p->fnstatus = fnstatus;
	p->size = f->record_max;
	p->r_size = f->record_max + sizeof (size_t);
	p->w_size = f->record_max + sizeof (size_t) + 1;
	n = sizeof (struct cobitem) - offsetof (struct cobitem, item);
	if (f->record_max <= n) {
		p->alloc_size = sizeof (struct cobitem);
	} else {
		p->alloc_size = offsetof (struct cobitem, item) + f->record_max;
	}
	if (p->alloc_size % sizeof (void *)) {
		p->alloc_size += sizeof (void *) - (p->alloc_size % sizeof (void *));
	}
	p->chunk_size = cobsetptr->cob_sort_chunk;
	if (p->chunk_size % p->alloc_size) {
		p->chunk_size += p->alloc_size - (p->chunk_size % p->alloc_size);
	}
	p->pointer = f;
	if (sort_return) {
		p->sort_return = sort_return;
		*(int *)sort_return = 0;
	}
	p->mem_base = cob_fast_malloc (sizeof (struct sort_mem_struct));
	p->mem_base->mem_ptr = cob_fast_malloc (p->chunk_size);
	p->mem_base->next = NULL;
	p->mem_size = p->chunk_size;
	p->mem_total = p->chunk_size;
	f->file = p;
	f->keys = cob_malloc (sizeof (cob_file_key) * nkeys);
	f->nkeys = 0;
	if (collating_sequence) {
		f->sort_collating = collating_sequence;
	} else {
		f->sort_collating = COB_MODULE_PTR->collating_sequence;
	}
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

void
cob_file_sort_init_key (cob_file *f, cob_field *field, const int flag,
			const unsigned int offset)
{
	f->keys[f->nkeys].field = field;
	f->keys[f->nkeys].flag = flag;
	f->keys[f->nkeys].offset = offset;
	f->nkeys++;
}

void
cob_file_sort_close (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	size_t		i;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
		cob_free_list (hp);
		for (i = 0; i < 4; ++i) {
			if (hp->file[i].fp != NULL) {
				fclose (hp->file[i].fp);
			}
		}
		cob_free (hp);
	}
	if (f->keys) {
		cob_free (f->keys);
	}
	f->file = NULL;
	save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
}

void
cob_file_release (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	int		ret;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
	}
	ret = cob_file_sort_submit (f, f->record->data);
	if (!ret) {
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	}
	if (likely(hp && hp->sort_return)) {
		*(int *)(hp->sort_return) = 16;
	}
	save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
}

void
cob_file_return (cob_file *f)
{
	struct cobsort	*hp;
	cob_field	*fnstatus;
	int		ret;

	fnstatus = NULL;
	hp = f->file;
	if (likely(hp)) {
		fnstatus = hp->fnstatus;
	}
	ret = cob_file_sort_retrieve (f, f->record->data);
	switch (ret) {
	case 0:
		save_status (f, fnstatus, COB_STATUS_00_SUCCESS);
		return;
	case COBSORTEND:
		save_status (f, fnstatus, COB_STATUS_10_END_OF_FILE);
		return;
	}
	if (likely(hp && hp->sort_return)) {
		*(int *)(hp->sort_return) = 16;
	}
	save_status (f, fnstatus, COB_STATUS_30_PERMANENT_ERROR);
}

/* Initialization/Termination
   cobsetpr-values with type ENV_PATH or ENV_STR
   like bdb_home and cob_file_path are taken care in cob_exit_common()!
*/

void
cob_exit_fileio (void)
{
	struct file_list	*l;
	struct file_list	*p;

	for (l = file_cache; l; l = l->next) {
		if (l->file && l->file->open_mode != COB_OPEN_CLOSED &&
		    l->file->open_mode != COB_OPEN_LOCKED &&
		    !l->file->flag_nonexistent) {
			if (COB_FILE_SPECIAL (l->file)) {
				continue;
			}
			cob_close (l->file, NULL, COB_CLOSE_NORMAL, 0);
			if (cobsetptr->cob_display_warn) {
				cob_field_to_string (l->file->assign,
						     runtime_buffer,
						     (size_t)COB_FILE_MAX);
				cob_runtime_warning (_("implicit CLOSE of %s ('%s')"),
					l->file->select_name, runtime_buffer);
			}
		}
	}
#ifdef	WITH_DB
	if (bdb_env) {
		bdb_env->lock_id_free (bdb_env, bdb_lock_id);
		bdb_env->close (bdb_env, 0);
		bdb_env = NULL;
	}
	if (record_lock_object) {
		cob_free (record_lock_object);
		record_lock_object = NULL;
	}
	if (bdb_buff) {
		cob_free (bdb_buff);
		bdb_buff = NULL;
	}

#elif	defined(WITH_ANY_ISAM)
#ifndef	WITH_DISAM
	(void)iscleanup ();
#endif
#endif

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
	extfh_cob_exit_fileio ();
#endif

	if (runtime_buffer) {
		cob_free (runtime_buffer);
		runtime_buffer = NULL;
	}

	for (l = file_cache; l;) {
		p = l;
		l = l->next;
		cob_free (p);
	}
	file_cache = NULL;
}

void
cob_init_fileio (cob_global *lptr, cob_settings *sptr)
{

#if defined(VB_RTD)
	if(vbisam_rtd == NULL) {	/* VB-ISAM 2.1.1 run-time pointer */
		vbisam_rtd = VB_GET_RTD; 
	}
#endif

	cobglobptr = lptr;
	cobsetptr  = sptr;
	file_cache = NULL;
	eop_status = 0;
	check_eop_status = 0;
	if (cobsetptr->cob_sort_chunk > (cobsetptr->cob_sort_memory / 2)) {
		cobsetptr->cob_sort_chunk = cobsetptr->cob_sort_memory / 2;
	}

	if(cobsetptr->cob_mf_files) {	/* Just use all MF format files */
		cobsetptr->cob_ls_nulls = 1;
		cobsetptr->cob_ls_split = 1;
		cobsetptr->cob_ls_validate = 0;
		if(cobsetptr->cob_varseq_type == COB_FILE_IS_GC
		|| cobsetptr->cob_varseq_type == 0)
			cobsetptr->cob_varseq_type = COB_FILE_IS_MF;
		if(cobsetptr->cob_varrel_type == COB_FILE_IS_GC)
			cobsetptr->cob_varrel_type = COB_FILE_IS_MF;
		if(cobsetptr->cob_fixrel_type == COB_FILE_IS_GC)
			cobsetptr->cob_fixrel_type = COB_FILE_IS_MF;
	}
	if(cobsetptr->cob_gc_files) {	/* Just use all GNUCobol format files */
		cobsetptr->cob_ls_nulls = 0;
		cobsetptr->cob_ls_split = 0;
		cobsetptr->cob_ls_validate = 0;
		if(cobsetptr->cob_varseq_type == COB_FILE_IS_MF)
			cobsetptr->cob_varseq_type = COB_FILE_IS_GC;
		if(cobsetptr->cob_varrel_type == COB_FILE_IS_MF)
			cobsetptr->cob_varrel_type = COB_FILE_IS_GC;
		if(cobsetptr->cob_fixrel_type == COB_FILE_IS_MF)
			cobsetptr->cob_fixrel_type = COB_FILE_IS_GC;
	}

	runtime_buffer = cob_fast_malloc ((size_t)(4 * COB_FILE_BUFF) + 4);
	file_open_env = runtime_buffer + COB_FILE_BUFF;
	file_open_name = runtime_buffer + (2 * COB_FILE_BUFF);
	file_open_buff = runtime_buffer + (3 * COB_FILE_BUFF);

#ifdef	WITH_DB
	bdb_env = NULL;
	bdb_data_dir = NULL;
	record_lock_object = cob_malloc ((size_t)1032);
	rlo_size = 1024;
	bdb_buff = cob_malloc ((size_t)COB_SMALL_BUFF+1);
#endif

#if	defined(WITH_INDEX_EXTFH) || defined(WITH_SEQRA_EXTFH)
	extfh_cob_init_fileio (&sequential_funcs, &lineseq_funcs,
			       &relative_funcs, &cob_file_write_opt);
#endif
}

/* Call this routine when a new process has been forked */
void
cob_fork_fileio (cob_global *lptr, cob_settings *sptr)
{
	COB_UNUSED (lptr);
	COB_UNUSED (sptr);
#ifdef	WITH_DB
	bdb_lock_id = 0;
	if (bdb_env) {
		bdb_env->lock_id (bdb_env, &bdb_lock_id);
		bdb_env->set_lk_detect (bdb_env, DB_LOCK_DEFAULT);
	}
#endif
}

/************************************************************************************/
/* Following routines are for the Micro Focus style External File Handler interface */
/************************************************************************************/
static struct fcd_file {
	struct fcd_file	*next;
	FCD3		*fcd;
	cob_file	*f;
	int		sts;
	int		free_fcd;
} *fcd_file_list = NULL;
static const cob_field_attr alnum_attr = {COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};

/*
 * Update FCD from cob_file
 */
static void
update_file_to_fcd (cob_file *f, FCD3 *fcd, unsigned char *fnstatus)
{
	if (f->file_status)
		memcpy (fcd->fileStatus,f->file_status,2);
	else if (fnstatus)
		memcpy (fcd->fileStatus, fnstatus, 2);
	else
		memcpy (fcd->fileStatus,"00",2);
	/* FIXME: use switch here */
	if (f->open_mode == COB_OPEN_CLOSED)
		fcd->openMode = OPEN_NOT_OPEN;
	else if( f->open_mode == COB_OPEN_INPUT)
		fcd->openMode = OPEN_INPUT;
	else if (f->open_mode == COB_OPEN_OUTPUT)
		fcd->openMode = OPEN_OUTPUT;
	else if (f->open_mode == COB_OPEN_I_O)
		fcd->openMode = OPEN_IO;
	else if (f->open_mode == COB_OPEN_EXTEND)
		fcd->openMode = OPEN_EXTEND;
	STCOMPX4(f->record_min,fcd->minRecLen);
	STCOMPX4(f->record_max, fcd->maxRecLen);
	if(f->record)
		STCOMPX4(f->record->size,fcd->curRecLen);
}

/*
 * Copy 'cob_file' to FCD based information
 */
static void
copy_file_to_fcd (cob_file *f, FCD3 *fcd)
{
	char	assignto[512];
	int	fnlen,kdblen,idx,keypos,keycomp,k,nkeys;
	KDB	*kdb;
	EXTKEY	*key;

	/* FIXME: use switch here */
	if(f->access_mode == COB_ACCESS_SEQUENTIAL)
		fcd->accessFlags = ACCESS_SEQ;
	else if(f->access_mode == COB_ACCESS_RANDOM)
		fcd->accessFlags = ACCESS_RANDOM;
	else if(f->access_mode == COB_ACCESS_DYNAMIC)
		fcd->accessFlags = ACCESS_DYNAMIC;
	if((f->flag_select_features & COB_SELECT_EXTERNAL))
		fcd->otherFlags |= OTH_EXTERNAL;
	if(f->flag_optional)
		fcd->otherFlags |= OTH_OPTIONAL;
	if(f->flag_line_adv)
		fcd->otherFlags |= OTH_LINE_ADVANCE;

	/* CHECKME: is this still needed? */
	if (f->assign) {
		cob_field_to_string (f->assign, assignto, sizeof(assignto)-1);
	} else {
		strcpy (assignto, f->select_name);
	}
	STCOMPX2(sizeof(FCD3),fcd->fcdLen);
	fcd->fcdVer = FCD_VER_64Bit;
	fcd->gcFlags |= MF_CALLFH_GNUCOBOL;
	if (f->trace_io)
		fcd->gcFlags |= MF_CALLFH_TRACE;
	else
		fcd->gcFlags &= ~MF_CALLFH_TRACE;
	if (f->io_stats)
		fcd->gcFlags |= MF_CALLFH_STATS;
	else
		fcd->gcFlags &= ~MF_CALLFH_STATS;
	if(f->record_min != f->record_max)
		fcd->recordMode = REC_MODE_VARIABLE;
	else
		fcd->recordMode = REC_MODE_FIXED;
	fnlen = strlen(assignto);
	if(fcd->fnamePtr != NULL) {
		cob_free ((void*)fcd->fnamePtr);
	}
	fcd->fnamePtr = strdup(assignto);
	fcd->openMode |= OPEN_NOT_OPEN;
	STCOMPX2(fnlen, fcd->fnameLen);
	STCOMPX2(0, fcd->refKey);
	if(f->lock_mode == COB_LOCK_EXCLUSIVE
	|| f->lock_mode == COB_LOCK_OPEN_EXCLUSIVE)
		fcd->lockMode = FCD_LOCK_EXCL_LOCK;
	else if(f->lock_mode == COB_LOCK_MANUAL)
		fcd->lockMode = FCD_LOCK_MANU_LOCK;
	else if(f->lock_mode == COB_LOCK_AUTOMATIC)
		fcd->lockMode = FCD_LOCK_AUTO_LOCK;
	fcd->recPtr = f->record->data;
	if (f->organization == COB_ORG_INDEXED) {
		STCOMPX2(0, fcd->refKey);
		fcd->fileOrg = ORG_INDEXED;
		fcd->fileFormat = MF_FF_CISAM;
		/* Copy Key information from cob_file to FCD */
		for (idx=keycomp=0; idx < f->nkeys; idx++) {
			if (f->keys[idx].count_components <= 1) {
				keycomp++;
			} else {
				keycomp += f->keys[idx].count_components;
			}
		}
		if (fcd->kdbPtr == NULL
		 && f->nkeys > 0) {
			nkeys = f->nkeys;
			kdblen = sizeof(KDB) - sizeof(kdb->key) + (sizeof(KDB_KEY) * nkeys) + (sizeof(EXTKEY) * keycomp);
			fcd->kdbPtr = kdb = cob_malloc(kdblen + sizeof(EXTKEY));
			STCOMPX2(kdblen, kdb->kdbLen);
			STCOMPX2(nkeys, kdb->nkeys);
		} else if (fcd->kdbPtr == NULL) {
			nkeys = 0;
			kdblen = sizeof(KDB) - sizeof(kdb->key) + (sizeof(KDB_KEY) * nkeys) + (sizeof(EXTKEY) * keycomp);
			fcd->kdbPtr = kdb = cob_malloc(kdblen + sizeof(EXTKEY));
			STCOMPX2(kdblen, kdb->kdbLen);
			STCOMPX2(nkeys, kdb->nkeys);
		} else {
			kdb = fcd->kdbPtr;
			nkeys = LDCOMPX2(kdb->nkeys);
			if (nkeys > f->nkeys) {
				nkeys = f->nkeys;
			}
		}
		keypos = (sizeof(KDB_KEY) * nkeys) + sizeof(KDB) - sizeof(kdb->key);
		for(idx=0; idx < nkeys; idx++) {
			key = (EXTKEY*)((char*)((char*)kdb) + keypos);
			STCOMPX2(keypos, kdb->key[idx].offset);
			kdb->key[idx].keyFlags = 0;
			if(f->keys[idx].tf_duplicates)
				kdb->key[idx].keyFlags |= KEY_DUPS;
			if(f->keys[idx].tf_suppress) {
				kdb->key[idx].keyFlags |= KEY_SPARSE;
				kdb->key[idx].sparse = (unsigned char)f->keys[idx].char_suppress;
			}
			if(f->keys[idx].count_components <= 1) {
				STCOMPX2(1,kdb->key[idx].count);
				STCOMPX4(f->keys[idx].offset, key->pos);
				STCOMPX4(f->keys[idx].field->size, key->len);
				keypos = keypos + sizeof(EXTKEY);
			} else {
				STCOMPX2(f->keys[idx].count_components, kdb->key[idx].count);
				for(k=0; k < f->keys[idx].count_components; k++) {
					key = (EXTKEY*)((char*)((char*)kdb) + keypos);
					STCOMPX4(f->keys[idx].component[k]->data - f->record->data, key->pos);
					STCOMPX4(f->keys[idx].component[k]->size, key->len);
					keypos = keypos + sizeof(EXTKEY);
				}
			}
		}

	} else if(f->organization == COB_ORG_SEQUENTIAL) {
		fcd->fileOrg = ORG_SEQ;
		STCOMPX2(0, fcd->refKey);
	} else if(f->organization == COB_ORG_LINE_SEQUENTIAL) {
		fcd->fileOrg = ORG_LINE_SEQ;
		STCOMPX2(0, fcd->refKey);
		if((f->file_features & COB_FILE_LS_CRLF))
			fcd->fstatusType |= MF_FST_CRdelim;
		if((f->file_features & COB_FILE_LS_NULLS))
			fcd->fstatusType |= MF_FST_InsertNulls;
		if((f->file_features & COB_FILE_LS_FIXED))
			fcd->fstatusType |= MF_FST_NoStripSpaces;
	} else if(f->organization == COB_ORG_RELATIVE) {
		fcd->fileOrg = ORG_RELATIVE;
		STCOMPX2(0, fcd->refKey);
	}
	update_file_to_fcd(f, fcd, NULL);
}

/*
 * Update 'cob_file' from 'FCD' information
 */
static void
update_fcd_to_file (FCD3* fcd, cob_file *f, cob_field *fnstatus, int wasOpen)
{
	int	status;
	if(f->file_status)
		memcpy(f->file_status, fcd->fileStatus, 2);
	if(fnstatus)
		memcpy(fnstatus->data, fcd->fileStatus, 2);
	if (wasOpen) {
		if((fcd->openMode & OPEN_NOT_OPEN))
			f->open_mode = 0;
		else if((fcd->openMode&0x7f) == OPEN_INPUT)
			f->open_mode = COB_OPEN_INPUT;
		else if((fcd->openMode&0x7f) == OPEN_OUTPUT)
			f->open_mode = COB_OPEN_OUTPUT;
		else if((fcd->openMode&0x7f) == OPEN_EXTEND)
			f->open_mode = COB_OPEN_EXTEND;
		else if((fcd->openMode&0x7f) == OPEN_IO)
			f->open_mode = COB_OPEN_I_O;
	}
	f->record_min = LDCOMPX4(fcd->minRecLen);
	f->record_max = LDCOMPX4(fcd->maxRecLen);
	f->record->size = LDCOMPX4(fcd->curRecLen);
	status = 0;
	if(isdigit(fcd->fileStatus[0])) {
		status = fcd->fileStatus[0] - '0';
	}
	status = status * 10;
	if(isdigit(fcd->fileStatus[1]))
		status += (fcd->fileStatus[1] - '0');

	/* Call save_status to get trace & stats done */
	save_status (f, fnstatus, status);
}

/*
 * Copy 'FCD' to 'cob_file' based information
 */
static void
copy_fcd_to_file (FCD3* fcd, cob_file *f)
{
	int		k, p, parts, off;
	char	fdname[48];
	EXTKEY	*key;

	if(fcd->accessFlags == ACCESS_SEQ)
		f->access_mode = COB_ACCESS_SEQUENTIAL;
	else if(fcd->accessFlags == ACCESS_RANDOM)
		f->access_mode = COB_ACCESS_RANDOM;
	else if(fcd->accessFlags == ACCESS_DYNAMIC)
		f->access_mode = COB_ACCESS_DYNAMIC;
	if((fcd->otherFlags & OTH_EXTERNAL))
		f->flag_select_features |= COB_SELECT_EXTERNAL;
	if((fcd->otherFlags & OTH_OPTIONAL))
		f->flag_optional = 1;
	else
		f->flag_optional = 0;
	if((fcd->otherFlags & OTH_LINE_ADVANCE))
		f->flag_line_adv = 1;
	else
		f->flag_line_adv = 0;

	if((fcd->lockMode & FCD_LOCK_EXCL_LOCK))
		f->lock_mode = COB_LOCK_EXCLUSIVE;
	else if((fcd->lockMode & FCD_LOCK_MANU_LOCK))
		f->lock_mode = COB_LOCK_MANUAL;
	else if((fcd->lockMode & FCD_LOCK_AUTO_LOCK))
		f->lock_mode = COB_LOCK_AUTOMATIC;

	if(fcd->fileOrg == ORG_INDEXED) {
		f->organization = COB_ORG_INDEXED;
	} else if(fcd->fileOrg == ORG_SEQ) {
		f->organization = COB_ORG_SEQUENTIAL;
	} else if(fcd->fileOrg == ORG_LINE_SEQ) {
		f->organization = COB_ORG_LINE_SEQUENTIAL;
#ifdef	_WIN32
		f->file_features |= COB_FILE_LS_CRLF;
#else
		if((fcd->fstatusType & MF_FST_CRdelim))
			f->file_features |= COB_FILE_LS_CRLF;
		else
			f->file_features |= COB_FILE_LS_LF;
#endif
		if((fcd->fstatusType & MF_FST_InsertNulls))
			f->file_features |= COB_FILE_LS_NULLS;
		if((fcd->fstatusType & MF_FST_NoStripSpaces))
			f->file_features |= COB_FILE_LS_FIXED;
	} else if(fcd->fileOrg == ORG_RELATIVE) {
		f->organization = COB_ORG_RELATIVE;
	}

	if (fcd->gcFlags & MF_CALLFH_TRACE)
		f->trace_io = 1;
	else
		f->trace_io = 0;
	if (fcd->gcFlags & MF_CALLFH_STATS)
		f->io_stats = 1;
	else
		f->io_stats = 0;

	/* Allocate cob_file fields as needed and copy from FCD */
	if (f->record == NULL) {
		f->record = cob_malloc(sizeof(cob_field));
		f->record->data = fcd->recPtr;
		f->record->size = LDCOMPX4(fcd->curRecLen);
		f->record->attr = &alnum_attr;
		f->record_min = LDCOMPX4(fcd->minRecLen);
		f->record_max = LDCOMPX4(fcd->maxRecLen);
	}
	if (f->file_status == NULL) {
		f->file_status = cob_malloc( 6 );
	}
	if (f->assign == NULL) {
		f->assign = cob_malloc(sizeof(cob_field));
		f->assign->data = (unsigned char*)fcd->fnamePtr;
		f->assign->size = LDCOMPX2(fcd->fnameLen);
		f->assign->attr = &alnum_attr;
	}
	if (f->select_name == NULL) {
		f->select_name = (char*)f->assign->data;
		for (k=0; k < f->assign->size; k++) {
			if (f->assign->data[k] == '/') {
				f->select_name = (char*)&f->assign->data[k+1];
			}
		}
		for (k=0; f->select_name[k] > ' ' && k < 48; k++) {
			fdname[k] = toupper(f->select_name[k]);
		}
		fdname[k] = 0;
		f->select_name = cob_strdup (fdname);
	}
	if (f->keys == NULL) {
		if (fcd->kdbPtr != NULL
		 && fcd->kdbPtr->nkeys > 0) {
			/* Copy Key information from FCD to cob_file,
			   CHECKME: possibly only for ORG_DETERMINE ? */
			f->nkeys = LDCOMPX2(fcd->kdbPtr->nkeys);
			f->keys = cob_malloc (sizeof(cob_file_key) * f->nkeys);
			for (k=0; k < f->nkeys; k++) {
				parts = LDCOMPX2(fcd->kdbPtr->key[k].count);
				off   = LDCOMPX2(fcd->kdbPtr->key[k].offset);
				key   = (EXTKEY*) ((char*)(fcd->kdbPtr) + off);
				if (fcd->kdbPtr->key[k].keyFlags & KEY_SPARSE) {
					f->keys[k].char_suppress = fcd->kdbPtr->key[k].sparse;
					f->keys[k].tf_suppress = 1;
				} else {
					f->keys[k].tf_suppress = 0;
				}
				if (fcd->kdbPtr->key[k].keyFlags & KEY_DUPS) {
					f->keys[k].tf_duplicates = 1;
				} else {
					f->keys[k].tf_duplicates = 0;
				}
				f->keys[k].count_components = parts;
				f->keys[k].field = cob_malloc(sizeof(cob_field));
				f->keys[k].field->data = f->record->data + LDCOMPX4(key->pos);
				f->keys[k].field->attr = &alnum_attr;
				f->keys[k].field->size = LDCOMPX4(key->len);
				f->keys[k].offset = LDCOMPX4(key->pos);
				for (p=0; p < parts; p++) {
					f->keys[k].component[p] = cob_malloc(sizeof(cob_field));
					f->keys[k].component[p]->data = f->record->data + LDCOMPX4(key->pos);
					f->keys[k].component[p]->attr = &alnum_attr;
					f->keys[k].component[p]->size = LDCOMPX4(key->len);
					key   = (EXTKEY*) ((char*)(key) + sizeof(EXTKEY));
				}
			}
		} else {
			f->keys = cob_malloc(sizeof(cob_file_key));
		}
	}
	update_fcd_to_file (fcd, f, NULL, 0);
}

/*
 * Construct FCD based on information from 'cob_file'
 */
static FCD3 *
find_fcd (cob_file *f)
{
	FCD3	*fcd;
	struct fcd_file	*ff;
	for(ff = fcd_file_list; ff; ff=ff->next) {
		if(ff->f == f)
			return ff->fcd;
	}
	fcd = cob_malloc(sizeof(FCD3));
	copy_file_to_fcd(f, fcd);
	ff = cob_malloc(sizeof(struct fcd_file));
	ff->next = fcd_file_list;
	ff->fcd = fcd;
	ff->f = f;
	ff->free_fcd = 1;
	fcd_file_list = ff;
	return fcd;
}

/*
 * Construct cob_file based on information from 'FCD'
 */
static cob_file *
find_file (FCD3 *fcd)
{
	cob_file	*f;
	struct fcd_file	*ff;
	for(ff = fcd_file_list; ff; ff=ff->next) {
		if(ff->fcd == fcd) {
			return ff->f;
		}
	}
	f = cob_malloc(sizeof(cob_file));
	f->file_version = COB_FILE_VERSION;
	copy_fcd_to_file(fcd, f);
	ff = cob_malloc(sizeof(struct fcd_file));
	ff->next = fcd_file_list;
	ff->fcd = fcd;
	ff->f = f;
	ff->free_fcd = 0;
	fcd_file_list = ff;
	return f;
}

static void
save_fcd_status (FCD3 *fcd, int sts)
{
	struct fcd_file	*ff;
	for(ff = fcd_file_list; ff; ff=ff->next) {
		if(ff->fcd == fcd) {
			ff->sts = sts;
			return;
		}
	}
}

/* Return index number for given key */
static int
cob_findkey (cob_file *f, cob_field *kf, int *fullkeylen, int *partlen)
{
	int 	k,part;
	*fullkeylen = *partlen = 0;

	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].field
		&&  f->keys[k].count_components <= 1
		&&  f->keys[k].field->data == kf->data) {
			*fullkeylen = f->keys[k].field->size;
			*partlen = kf->size;
			return k;
		}
	}
	for (k = 0; k < f->nkeys; ++k) {
		if (f->keys[k].count_components > 1) {
			if ((f->keys[k].field
			&&  f->keys[k].field->data == kf->data
			&&  f->keys[k].field->size == kf->size)
			||  (f->keys[k].component[0]->data == kf->data)) {
				for(part=0; part < f->keys[k].count_components; part++)
					*fullkeylen += f->keys[k].component[part]->size;
				if(f->keys[k].field && f->keys[k].field->data == kf->data)
					*partlen = kf->size;
				else
					*partlen = *fullkeylen;
				return k;
			}
		}
	}
	return 0;
}

/*
 * NOTES: It would be best if 'cob_file' had a pointer to the full/complete file name
 *        ISAM & BDB already keep this in a separate structure
 *        The filename should be passed via EXTFH interface
 */

/*
 * OPEN file
 */
void
cob_extfh_open (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, const int mode, const int sharing, cob_field *fnstatus)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	sts;

	f->last_operation = COB_LAST_OPEN;
	fcd = find_fcd(f);
	f->share_mode = sharing;
	f->last_open_mode = mode;
	if(mode == COB_OPEN_OUTPUT)
		STCOMPX2(OP_OPEN_OUTPUT, opcode);
	else if(mode == COB_OPEN_I_O)
		STCOMPX2(OP_OPEN_IO, opcode);
	else if(mode == COB_OPEN_EXTEND)
		STCOMPX2(OP_OPEN_EXTEND, opcode);
	else
		STCOMPX2(OP_OPEN_INPUT, opcode);

	/* Keep table of 'fcd' created */
	sts = callfh (opcode, fcd);
	if (f->file_status) {
		if (memcmp(f->file_status,"00",2) == 0
		|| memcmp(f->file_status,"05",2) == 0) 
			fcd->openMode &= ~OPEN_NOT_OPEN;
	} else {
		fcd->openMode &= ~OPEN_NOT_OPEN;
	}
	update_fcd_to_file (fcd, f, fnstatus, 1);
	save_fcd_status (fcd, sts);
}

/*
 * CLOSE file
 */
void
cob_extfh_close (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *fnstatus, const int opt, const int remfil)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	struct fcd_file	*ff,*pff;

	f->last_operation = COB_LAST_CLOSE;
	fcd = find_fcd(f);
	STCOMPX4(opt, fcd->opt);
	STCOMPX2(OP_CLOSE, opcode);

	/* Keep table of 'fcd' created */
	(void)callfh (opcode,fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);

	pff = NULL;
	for(ff = fcd_file_list; ff; ff=ff->next) {
		if(ff->fcd == fcd) {
			if(pff)
				pff->next = ff->next;
			else
				fcd_file_list = ff->next;
			if(ff->free_fcd)
				cob_free((void*)ff->fcd);
			else
				cob_free((void*)ff->f);
			cob_free((void*)ff);
			break;
		}
		pff = ff;
	}
}

/*
 * START
 */
void
cob_extfh_start (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, const int cond, cob_field *key, cob_field *keysize, cob_field *fnstatus)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;
	int	keyn,keylen,partlen;

	f->last_operation = COB_LAST_START;
	fcd = find_fcd(f);
	if (f->organization == COB_ORG_INDEXED) {
		keyn = cob_findkey(f,key,&keylen,&partlen);
		STCOMPX2(keyn, fcd->refKey);
		if (keysize)
			partlen = cob_get_int (keysize);
		STCOMPX2(partlen, fcd->effKeyLen);
		STCOMPX2(keyn, fcd->refKey);
		STCOMPX2(OP_READ_RAN, opcode);
	} else if(f->organization == COB_ORG_RELATIVE) {
		memset(fcd->relKey,0,sizeof(fcd->relKey));
		recn = cob_get_int(f->keys[0].field);
		STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
		STCOMPX2(OP_READ_RAN, opcode);
	}

	switch(cond) {
	case COB_EQ:	STCOMPX2(OP_START_EQ, opcode); break;
	case COB_GE:	STCOMPX2(OP_START_GE, opcode); break;
	case COB_LE:	STCOMPX2(OP_START_LE, opcode); break;
	case COB_GT:	STCOMPX2(OP_START_GT, opcode); break;
	case COB_LT:	STCOMPX2(OP_START_LT, opcode); break;
	case COB_FI:	STCOMPX2(OP_START_FI, opcode); break;
	case COB_LA:	STCOMPX2(OP_START_LA, opcode); break;
	default:
			STCOMPX2(OP_START_EQ_ANY, opcode); break;
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}

/*
 * READ
 */
void
cob_extfh_read (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *key, cob_field *fnstatus, const int read_opts)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;
	int	keyn,keylen,partlen;

	f->last_operation = COB_LAST_READ;
	fcd = find_fcd(f);
	STCOMPX4 (read_opts, fcd->opt);
	if(key == NULL) {
		f->last_operation = COB_LAST_READ_SEQ;
		if((read_opts & COB_READ_PREVIOUS)) {
			STCOMPX2(OP_READ_PREV, opcode);
		} else {
			STCOMPX2(OP_READ_SEQ, opcode);
		}
		if(f->organization == COB_ORG_RELATIVE) {
			memset(fcd->relKey,0,sizeof(fcd->relKey));
			recn = cob_get_int(f->keys[0].field);
			STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
			if (f->access_mode != COB_ACCESS_SEQUENTIAL)
				STCOMPX2(OP_READ_RAN, opcode);
		}
	} else if(f->organization == COB_ORG_INDEXED) {
		keyn = cob_findkey(f,key,&keylen,&partlen);
		STCOMPX2(keyn, fcd->refKey);
		STCOMPX2(keylen, fcd->effKeyLen);
		STCOMPX2(OP_READ_RAN, opcode);
	} else if(f->organization == COB_ORG_RELATIVE) {
		memset(fcd->relKey,0,sizeof(fcd->relKey));
		recn = cob_get_int(key);
		STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
		STCOMPX2(OP_READ_RAN, opcode);
	} else {
		STCOMPX2(OP_READ_SEQ, opcode);
	}

	callfh(opcode,fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}

/*
 * READ next
 */
void
cob_extfh_read_next (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *fnstatus, const int read_opts)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;

	f->last_operation = COB_LAST_READ_SEQ;
	fcd = find_fcd(f);
	STCOMPX4(read_opts, fcd->opt);
	if((read_opts & COB_READ_PREVIOUS)) {
		STCOMPX2(OP_READ_PREV, opcode);
	} else {
		STCOMPX2(OP_READ_SEQ, opcode);
	}
	if(f->organization == COB_ORG_RELATIVE) {
		memset(fcd->relKey,0,sizeof(fcd->relKey));
		recn = cob_get_int(f->keys[0].field);
		STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}
/*
 * WRITE
 */
void
cob_extfh_write (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus, const unsigned int check_eop)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;

	f->last_operation = COB_LAST_WRITE;
	fcd = find_fcd(f);
	STCOMPX2(OP_WRITE, opcode);
	STCOMPX2(check_eop, fcd->eop);
	STCOMPX4(opt, fcd->opt);
	if (f->variable_record) {
		f->record->size = (size_t)cob_get_int (f->variable_record);
		if (unlikely(f->record->size > rec->size)) {
			f->record->size = rec->size;
		}
	} else {
		f->record->size = rec->size;
	}
	STCOMPX4(f->record->size,fcd->curRecLen);
	fcd->recPtr = rec->data;
	if (f->organization == COB_ORG_RELATIVE) {
		memset (fcd->relKey, 0, sizeof(fcd->relKey));
		recn = cob_get_int(f->keys[0].field);
		STCOMPX4(recn, LSUCHAR(fcd->relKey+4));
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}

/*
 * REWRITE
 */
void
cob_extfh_rewrite (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *rec, const int opt, cob_field *fnstatus)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;

	f->last_operation = COB_LAST_REWRITE;
	fcd = find_fcd(f);
	STCOMPX2 (OP_REWRITE, opcode);
	STCOMPX4 (rec->size, fcd->curRecLen);
	STCOMPX4 (opt, fcd->opt);
	fcd->recPtr = rec->data;
	if (f->organization == COB_ORG_RELATIVE) {
		memset (fcd->relKey ,0, sizeof(fcd->relKey));
		recn = cob_get_int (f->keys[0].field);
		STCOMPX4 (recn, LSUCHAR (fcd->relKey + 4));
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}

/*
 * DELETE
 */
void
cob_extfh_delete (
	int (*callfh)(unsigned char *opcode, FCD3 *fcd),
	cob_file *f, cob_field *fnstatus)
{
	unsigned char opcode[2];
	FCD3	*fcd;
	int	recn;

	f->last_operation = COB_LAST_DELETE;
	fcd = find_fcd (f);
	STCOMPX2 (OP_DELETE, opcode);
	if (f->organization == COB_ORG_RELATIVE) {
		memset (fcd->relKey, 0, sizeof(fcd->relKey));
		recn = cob_get_int (f->keys[0].field);
		STCOMPX4 (recn, LSUCHAR(fcd->relKey + 4));
	}

	(void)callfh (opcode, fcd);
	update_fcd_to_file (fcd, f, fnstatus, 0);
}

/* COBOL wrapper for EXTFH call to prevent warnings about FCD3 structure
   with additional checks */
int
cob_sys_extfh (const void *opcode_ptr, void *fcd_ptr)
{
	FCD3 *fcd = (FCD3 *) fcd_ptr;

	COB_CHK_PARMS (EXTFH, 2);

	if (cobglobptr->cob_call_params < 2
	 || !COB_MODULE_PTR->cob_procedure_params[0]
	 || !COB_MODULE_PTR->cob_procedure_params[1]
	 || COB_MODULE_PTR->cob_procedure_params[1]->size < 5) {
		cob_set_exception (COB_EC_PROGRAM_ARG_MISMATCH);
		return 0;	/* correct? */
	}
	if (COB_MODULE_PTR->cob_procedure_params[1]->size < sizeof(FCD3)) {
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 161;
		if (fcd->fcdVer != FCD_VER_64Bit) {
#if 1
			cob_runtime_warning (_("ERROR: EXTFH called with FCD version %d"), fcd->fcdVer);
#else
			cob_set_exception (COB_EC_PROGRAM_ARG_MISMATCH);
			cob_runtime_error (_("ERROR: EXTFH called with FCD version %d"), fcd->fcdVer);
			exit(-1);
#endif
		}
		return 0;
	}

	return EXTFH ((unsigned char *)opcode_ptr, fcd);
}

/*
 * EXTFH: maybe called by user own 'callfh' routine
 *        to call normal fileio routine in fileio.c
 */
int
EXTFH (unsigned char *opcode, FCD3 *fcd)
{
	int	opcd,sts,opts,eop,k;
	unsigned char	fnstatus[2],keywrk[80];
	cob_field fs[1];
	cob_field key[1];
	cob_field rec[1];
	cob_file *f;

	if (fcd->fcdVer != FCD_VER_64Bit) {
#if 1
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 161;
		cob_runtime_warning (_("ERROR: EXTFH called with FCD version %d"), fcd->fcdVer);
		return 0;
#else
		cob_runtime_error (_("ERROR: EXTFH called with FCD version %d"), fcd->fcdVer);
		exit(-1);
#endif
	}
	sts = opts = 0;
	fs->data = fnstatus;
	fs->size = sizeof(fnstatus);
	fs->attr = &alnum_attr;
	memset (fnstatus, '0', 2);
	memcpy (fcd->fileStatus, "00", 2);

	if (cobglobptr == NULL) {	/* Auto Init GnuCOBOL runtime */
		cob_init (0, NULL);
		/* COB_MODULE_PTR (part of cobglobptr structure) was not set,
		   add to allow tracing and to get better messages on fileio errors */
		COB_MODULE_PTR = cob_malloc( sizeof(cob_module) );
		COB_MODULE_PTR->module_name = "GnuCOBOL-fileio";
		COB_MODULE_PTR->module_source = "GnuCOBOL-fileio";
		COB_MODULE_PTR->module_formatted_date = "2018/07/01 12:00:00";
	}

	if (*opcode == 0xFA) {
		opcd = 0xFA00 + opcode[1];
	} else {
		opcd = opcode[1];
	}

	/* Look for fcd in table and if found use associated 'cob_file' after copying values over */
	/* If fcd is not found, then 'callfh' created it, so create a new 'cob_file' and table that */
	f = find_file (fcd);

org_handling:
	switch (fcd->fileOrg) {
	case ORG_INDEXED:
		k = LDCOMPX2(fcd->refKey);
		if (k >= 0) {
			if (f->keys[k].count_components <= 1) {
				key->size = f->keys[k].field->size;
				key->attr = f->keys[k].field->attr;
				key->data = f->record->data + f->keys[k].offset;
			} else {
				key->size = f->keys[k].component[0]->size;
				key->attr = f->keys[k].component[0]->attr;
				key->data = f->keys[k].component[0]->data;
			}
		} else {
			memset(keywrk,0,sizeof(keywrk));
			key->size = sizeof(keywrk);
			key->attr = &alnum_attr;
			key->data = keywrk;
		}
		break;
	case ORG_RELATIVE:
		cob_set_int (f->keys[0].field, LDCOMPX4(LSUCHAR(fcd->relKey+4)));
		memcpy(&key, f->keys[0].field, sizeof(cob_field));
		break;
	case ORG_SEQ:
	case ORG_LINE_SEQ:
		break;
	case ORG_DETERMINE:
		if (opcd != OP_GETINFO) {
			/* if we already registered this FCD to a file we can copy the old type */
			if (f->organization == COB_ORG_INDEXED) {
				fcd->fileOrg = ORG_INDEXED;
			} else if (f->organization == COB_ORG_SEQUENTIAL) {
				fcd->fileOrg = ORG_SEQ;
			} else if (f->organization == COB_ORG_LINE_SEQUENTIAL) {
				fcd->fileOrg = ORG_LINE_SEQ;
			} else if (f->organization == COB_ORG_RELATIVE) {
				fcd->fileOrg = ORG_RELATIVE;
			}
			if (fcd->fileOrg != ORG_DETERMINE) {
				goto org_handling;
#if 0
			} else {
				/* TODO: magic to get file type, for example try to idx-open the file */
				if (fcd->fileOrg != ORG_DETERMINE) {
					goto org_handling;
				}
#endif
			}
		}
	default:
		fcd->fileStatus[0] = '9';
		fcd->fileStatus[1] = 161;
		cob_runtime_warning (_("ERROR: EXTFH called with wrong file organization %d"), fcd->fileOrg);
		return 0;
	}

	rec->data = fcd->recPtr;
	rec->size = LDCOMPX4(fcd->curRecLen);
	rec->attr = &alnum_attr;

	if(*opcode == 0xFA)
		opcd = 0xFA00 + opcode[1];
	else
		opcd = opcode[1];
	switch (opcd) {
	case OP_OPEN_INPUT:
	case OP_OPEN_INPUT_NOREWIND:
	case OP_OPEN_INPUT_REVERSED:
		cob_open(f, COB_OPEN_INPUT, 0, fs);
		if (f->organization == COB_ORG_INDEXED
		 && memcmp(f->file_status,"0",1) == 0) {	/* 00 or 05 are both ok */
			f->open_mode = COB_OPEN_INPUT;
		}
		update_file_to_fcd(f,fcd,fnstatus);
		if (f->organization == COB_ORG_INDEXED
		 && memcmp(f->file_status,"61",1) == 0) {/* 61 --> 9A for MF */
			memcpy(fcd->fileStatus,"9A",2);
		}
		break;

	case OP_OPEN_OUTPUT:
	case OP_OPEN_OUTPUT_NOREWIND:
		cob_open(f, COB_OPEN_OUTPUT, 0, fs);
		if (f->organization == COB_ORG_INDEXED
		 && memcmp(f->file_status,"0",1) == 0) {
			f->open_mode = COB_OPEN_OUTPUT;
		}
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_OPEN_IO:
		cob_open(f, COB_OPEN_I_O, 0, fs);
		if (f->organization == COB_ORG_INDEXED
		&& (memcmp(f->file_status,"00",2) == 0
		 || memcmp(f->file_status,"05",2) == 0
		 || memcmp(f->file_status,"35",2) == 0)) {
			f->open_mode = COB_OPEN_I_O;
		}
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_OPEN_EXTEND:
		cob_open(f, COB_OPEN_EXTEND, 0, fs);
		if (f->organization == COB_ORG_INDEXED
		 && memcmp(f->file_status,"0",1) == 0) {
			f->open_mode = COB_OPEN_EXTEND;
		}
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_CLOSE:
	case OP_CLOSE_REEL:
		cob_close(f, fs, COB_CLOSE_NORMAL, 0);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_CLOSE_LOCK:
		cob_close(f, fs, COB_CLOSE_LOCK, 0);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_CLOSE_REMOVE:
		cob_close(f, fs, COB_CLOSE_UNIT_REMOVAL, 0);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_CLOSE_NO_REWIND:
	case OP_CLOSE_NOREWIND:
		cob_close(f, fs, COB_CLOSE_NO_REWIND, 0);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_READ_PREV:
	case OP_READ_PREV_LOCK:
	case OP_READ_PREV_NO_LOCK:
	case OP_READ_PREV_KEPT_LOCK:
		opts = COB_READ_PREVIOUS;
		if (opcd == OP_READ_PREV_LOCK)
			opts |= COB_READ_LOCK;
		else if (opcd == OP_READ_PREV_NO_LOCK)
			opts |= COB_READ_NO_LOCK;
		else if (opcd == OP_READ_PREV_KEPT_LOCK)
			opts |= COB_READ_KEPT_LOCK;
		cob_read_next(f, fs, opts);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_READ_SEQ:
	case OP_READ_SEQ_LOCK:
	case OP_READ_SEQ_NO_LOCK:
	case OP_READ_SEQ_KEPT_LOCK:
		opts = COB_READ_NEXT;
		if (opcd == OP_READ_SEQ_LOCK)
			opts |= COB_READ_LOCK;
		else if (opcd == OP_READ_SEQ_NO_LOCK)
			opts |= COB_READ_NO_LOCK;
		else if (opcd == OP_READ_SEQ_KEPT_LOCK)
			opts |= COB_READ_KEPT_LOCK;
		cob_read_next(f, fs, opts);
		update_file_to_fcd(f,fcd,NULL);
		break;

	case OP_STEP_NEXT:
	case OP_STEP_NEXT_LOCK:
	case OP_STEP_NEXT_NO_LOCK:
	case OP_STEP_NEXT_KEPT_LOCK:
		opts = COB_READ_NEXT;
		if (opcd == OP_STEP_NEXT_LOCK)
			opts |= COB_READ_LOCK;
		else if (opcd == OP_STEP_NEXT_NO_LOCK)
			opts |= COB_READ_NO_LOCK;
		else if (opcd == OP_STEP_NEXT_KEPT_LOCK)
			opts |= COB_READ_KEPT_LOCK;
		cob_read_next(f, fs, opts);
		update_file_to_fcd(f,fcd,NULL);
		break;

	case OP_STEP_FIRST:
	case OP_STEP_FIRST_LOCK:
	case OP_STEP_FIRST_NO_LOCK:
	case OP_STEP_FIRST_KEPT_LOCK:
		opts = COB_READ_FIRST;
		if (opcd == OP_STEP_FIRST_LOCK)
			opts |= COB_READ_LOCK;
		else if (opcd == OP_STEP_FIRST_NO_LOCK)
			opts |= COB_READ_NO_LOCK;
		else if (opcd == OP_STEP_FIRST_KEPT_LOCK)
			opts |= COB_READ_KEPT_LOCK;
		cob_read_next(f, fs, opts);
		update_file_to_fcd(f,fcd,NULL);
		break;

	case OP_READ_RAN:
	case OP_READ_RAN_LOCK:
	case OP_READ_RAN_NO_LOCK:
	case OP_READ_RAN_KEPT_LOCK:
		opts = LDCOMPX4(fcd->opt);
		if (opcd == OP_READ_RAN_LOCK)
			opts |= COB_READ_LOCK;
		else if (opcd == OP_READ_RAN_NO_LOCK)
			opts |= COB_READ_NO_LOCK;
		else if (opcd == OP_READ_RAN_KEPT_LOCK)
			opts |= COB_READ_KEPT_LOCK;
		cob_read(f, key, fs, opts);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_WRITE:
		eop = LDCOMPX2(fcd->eop);
		opts = LDCOMPX4(fcd->opt);
		cob_write(f, rec, opts, fs, eop);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_REWRITE:
		opts = LDCOMPX4(fcd->opt);
		cob_rewrite(f, rec, opts, fs);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_DELETE:
		cob_delete(f, fs);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_START_EQ:
		cob_start(f, COB_EQ, key, NULL, fs);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_START_GE:
		cob_start(f, COB_GE, key, NULL, fs);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_START_LE:
		cob_start(f, COB_LE, key, NULL, fs);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_START_LT:
		cob_start(f, COB_LT, key, NULL, fs);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_START_GT:
		cob_start(f, COB_GT, key, NULL, fs);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_START_FI:
		cob_start(f, COB_FI, key, NULL, fs);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_START_LA:
		cob_start(f, COB_LA, key, NULL, fs);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_COMMIT:
		cob_commit();
		break;

	case OP_ROLLBACK:
		cob_rollback();
		break;

	case OP_DELETE_FILE:
		cob_delete_file(f, fs);
		memcpy(fcd->fileStatus, fnstatus, 2);
		break;

	case OP_FLUSH:
		cob_sync(f);
		break;

	case OP_UNLOCK_REC:
		cob_unlock_file(f, fs);
		update_file_to_fcd(f,fcd,fnstatus);
		break;

	case OP_GETINFO:			/* Nothing needed here */
		break;


	/* Similar for other possible 'opcode' values */
	default:
		/* Some sort of error message */
		break;
	}
	return sts;
}
