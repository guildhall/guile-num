/* Guile interface to libsndfile1
   Copyright (C) 2002   Arno W. Peters <a.w.peters@ieee.org>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*/

%module "sound/sndfile"

%header %{
#include <stdio.h>
#include <sndfile.h>

SF_INFO * sf_info_alloc(void)
{
	SF_INFO *info;

	info = (SF_INFO *) malloc(sizeof(SF_INFO));
	return info;
}

void sf_info_free(SF_INFO *info)
{
	free(info);
}

%}

typedef size_t  sf_count_t;

typedef struct {
	sf_count_t frames;     /* Used to be called samples. */
	int samplerate;
	int channels;
	int format;
	int sections;
	int seekable;
} SF_INFO;

enum {
	SFM_READ,
	SFM_WRITE,
	SFM_RDWR
};

enum {
	/* Major formats. */
	SF_FORMAT_WAV          = 0x010000,     /* Microsoft WAV format (little endian). */
	SF_FORMAT_AIFF         = 0x020000,     /* Apple/SGI AIFF format (big endian). */
	SF_FORMAT_AU           = 0x030000,     /* Sun/NeXT AU format (big endian). */
	SF_FORMAT_RAW          = 0x040000,     /* RAW PCM data. */
	SF_FORMAT_PAF          = 0x050000,     /* Ensoniq PARIS file format. */
	SF_FORMAT_SVX          = 0x060000,     /* Amiga IFF / SVX8 / SV16 format. */
	SF_FORMAT_NIST         = 0x070000,     /* Sphere NIST format. */
	SF_FORMAT_VOC          = 0x080000,     /* VOC files. */
	SF_FORMAT_IRCAM        = 0x0A0000,     /* Berkeley/IRCAM/CARL */
	SF_FORMAT_W64          = 0x0B0000,     /* Sonic Foundry's 64 bit RIFF/WAV */
          
	/* Subtypes from here on. */
      
	SF_FORMAT_PCM_S8       = 0x0001,       /* Signed 8 bit data */
	SF_FORMAT_PCM_16       = 0x0002,       /* Signed 16 bit data */
	SF_FORMAT_PCM_24       = 0x0003,       /* Signed 24 bit data */
	SF_FORMAT_PCM_32       = 0x0004,       /* Signed 32 bit data */
	
	SF_FORMAT_PCM_U8       = 0x0005,       /* Unsigned 8 bit data (WAV and RAW only) */
      
	SF_FORMAT_FLOAT        = 0x0006,       /* 32 bit float data */
	SF_FORMAT_DOUBLE       = 0x0007,       /* 64 bit float data */
	
	SF_FORMAT_ULAW         = 0x0010,       /* U-Law encoded. */
	SF_FORMAT_ALAW         = 0x0011,       /* A-Law encoded. */
	SF_FORMAT_IMA_ADPCM    = 0x0012,       /* IMA ADPCM. */
	SF_FORMAT_MS_ADPCM     = 0x0013,       /* Microsoft ADPCM. */

	SF_FORMAT_GSM610       = 0x0020,       /* GSM 6.10 encoding. */
      
	SF_FORMAT_G721_32      = 0x0030,       /* 32kbs G721 ADPCM encoding. */
	SF_FORMAT_G723_24      = 0x0031,       /* 24kbs G723 ADPCM encoding. */
	SF_FORMAT_G723_40      = 0x0032,       /* 40kbs G723 ADPCM encoding. */
      
	SF_FORMAT_DWVW_12      = 0x0040,       /* 12 bit Delta Width Variable Word encoding. */
	SF_FORMAT_DWVW_16      = 0x0041,       /* 16 bit Delta Width Variable Word encoding. */
	SF_FORMAT_DWVW_24      = 0x0042,       /* 24 bit Delta Width Variable Word encoding. */
	SF_FORMAT_DWVW_N       = 0x0043,       /* N bit Delta Width Variable Word encoding. */
      
	/* Endian-ness options. */
      
	SF_ENDIAN_FILE         = 0x00000000,   /* Default file endian-ness. */
	SF_ENDIAN_LITTLE       = 0x10000000,   /* Force little endian-ness. */
	SF_ENDIAN_BIG          = 0x20000000,   /* Force big endian-ness. */
	SF_ENDIAN_CPU          = 0x30000000,   /* Force CPU endian-ness. */
      
	SF_FORMAT_SUBMASK      = 0x0000FFFF,
	SF_FORMAT_TYPEMASK     = 0x0FFF0000,
	SF_FORMAT_ENDMASK      = 0x30000000
} ;

SF_INFO * sf_info_alloc(void);
void sf_info_free(SF_INFO *info);
extern SNDFILE* sf_open (const char *path, int mode, SF_INFO *sfinfo);
extern int sf_format_check (const SF_INFO *info);
extern sf_count_t sf_seek (SNDFILE *sndfile, sf_count_t frames, int whence);
// extern int sf_command (SNDFILE *sndfile, int cmd, void *data, int datasize);
extern int sf_perror (SNDFILE *sndfile);
extern int sf_error_str (SNDFILE *sndfile, char* str, size_t len);
extern int sf_close (SNDFILE *sndfile);

extern sf_count_t sf_read_short (SNDFILE *sndfile, short *ptr, sf_count_t items);
extern sf_count_t sf_read_int (SNDFILE *sndfile, int *ptr, sf_count_t items);
extern sf_count_t sf_read_float (SNDFILE *sndfile, float *ptr, sf_count_t items);
extern sf_count_t sf_read_double (SNDFILE *sndfile, double *ptr, sf_count_t items);

extern sf_count_t sf_readf_short (SNDFILE *sndfile, short *ptr, sf_count_t frames);
extern sf_count_t sf_readf_int (SNDFILE *sndfile, int *ptr, sf_count_t frames);
extern sf_count_t sf_readf_float (SNDFILE *sndfile, float *ptr, sf_count_t frames);
extern sf_count_t sf_readf_double (SNDFILE *sndfile, double *ptr, sf_count_t frames);

extern sf_count_t sf_write_short (SNDFILE *sndfile, short *ptr, sf_count_t items);
extern sf_count_t sf_write_int (SNDFILE *sndfile, int *ptr, sf_count_t items);
extern sf_count_t sf_write_float (SNDFILE *sndfile, float *ptr, sf_count_t items);
extern sf_count_t sf_write_double (SNDFILE *sndfile, double *ptr, sf_count_t items);

extern sf_count_t sf_writef_short (SNDFILE *sndfile, short *ptr, sf_count_t frames);
extern sf_count_t sf_writef_int (SNDFILE *sndfile, int *ptr, sf_count_t frames);
extern sf_count_t sf_writef_float (SNDFILE *sndfile, float *ptr, sf_count_t frames);
extern sf_count_t sf_writef_double (SNDFILE *sndfile, double *ptr, sf_count_t frames);

// extern sf_count_t sf_read_raw (SNDFILE *sndfile, void *ptr, sf_count_t bytes);
// extern sf_count_t sf_write_raw (SNDFILE *sndfile, void *ptr, sf_count_t bytes);




%scheme %{
(define my-so (dynamic-link "sound/libguile-sndfile.so"))
(dynamic-call "SWIG_init" my-so)
%}
