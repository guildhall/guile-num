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
	memset(info, sizeof(SF_INFO), 0);
	return info;
}

void sf_info_free(SF_INFO *info)
{
	free(info);
}

%}

SF_INFO * sf_info_alloc(void);
void sf_info_free(SF_INFO *info);

%include "sndfile_inc.i"

%scheme %{
(define my-so (dynamic-link "libguile-sndfile.la"))
(dynamic-call "SWIG_init" my-so)
(export with-sound-from-file with-sound-to-file)

(define (with-sound-from-file file func)
  (let ((sf-info (sf-info-alloc)))
    (SF-INFO-format-set sf-info 0)
    (let ((handle (sf-open file (SFM-READ) sf-info)))
      (cond ((null? handle)
	     (error "Can't open sound file for reading"))
	    (else
	     (let ((channels (SF-INFO-channels-get sf-info))
		   (frames (SF-INFO-frames-get sf-info))
		   (samplerate (SF-INFO-samplerate-get sf-info))
		   (fileformat (SF-INFO-format-get sf-info)))
	       (func handle fileformat samplerate channels frames)
	       (sf-close handle)))))
    (sf-info-free sf-info)))

(define (with-sound-to-file file fileformat samplerate channels func)
  (let ((sf-info (sf-info-alloc)))
    (SF-INFO-samplerate-set sf-info samplerate)
    (SF-INFO-channels-set sf-info channels)
    (SF-INFO-format-set sf-info fileformat)
    (let ((handle (sf-open file (SFM-WRITE) sf-info)))
      (cond ((null? handle)
	     (error "Can't open sound file for writing"))
	    (else
	     (func handle)
	     (sf-close handle))))
    (sf-info-free sf-info)))
%}
