;;; devanagari-qwerty.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Ajai Khatri Nelson
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  An input method based on the macOS "Devanagari - QWERTY" input source
;;
;;; Code:

;; emulate the Mac "Devanagari - QWERTY" keyboard
(quail-define-package
 "devanagari-qwerty" "Devanagari QWERTY" "DevQWERTY")

(quail-define-rules
 ("`" ?ॆ) ("1" ?१) ("2" ?२) ("3" ?३) ("4" ?४) ("5" ?५) ("6" ?६) ("7" ?७) ("8" ?८) ("9" ?९) ("0" ?०) ("-" ?-) ("=" ?=)
 ("q" ?ॅ) ("w" ?अ) ("e" ?े) ("r" ?र) ("t" ?त) ("y" ?य) ("u" ?ु) ("i" ?ि) ("o" ?ो) ("p" ?प) ("[" ?॰) ("]" ?₹) ("\\" ?ॊ)
 ("a" ?ा) ("s" ?स) ("d" ?द) ("f" ?्) ("g" ?ग) ("h" ?ह) ("j" ?ज) ("k" ?क) ("l" ?ल) (";" ?\;) ("'" ?')
 ("z" ?ङ) ("x" ?ष) ("c" ?च) ("v" ?व) ("b" ?ब) ("n" ?न) ("m" ?म) ("," ?,) ("." ?।) ("/" ?/)

 ("~" ?ऎ) ("!" ?!) ("@" ?@) ("#" ?#) ("$" ?$) ("%" ?%) ("^" ?^) ("&" ?&) ("*" ?*) ("(" ?\() (")" ?\)) ("_" ?\_) ("+" ?\+)
 ("Q" ?ॉ) ("W" ?आ) ("E" ?ै) ("R" ?ृ) ("T" ?थ) ("Y" ?य़) ("U" ?ू) ("I" ?ी) ("O" ?ौ) ("P" ?फ) ("{" ?\{) ("}" ?\]) ("|" ?ऒ)
 ("A" ?ा) ("S" ?श) ("D" ?ध) ("F" ?़) ("G" ?घ) ("H" ?ः) ("J" ?झ) ("K" ?ख) ("L" ?ळ) (":" ?:) ("\"" "\"")
 ("Z" ?ँ) ;("X" ?क्ष)
 ("C" ?छ) ("V" ?V) ("B" ?भ) ("N" ?ण) ("M" ?ं) ("<" ?<) (">" ?>) ("?" ?\?))

(provide 'devanagari-qwerty)
;;; devanagari-qwerty.el ends here
