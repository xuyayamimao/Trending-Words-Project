#lang racket
;; CSC-151-03 (Spring 2022)
;; Mini-Project 7: Trending Words
;; Ziya Xu, David Zhou, Mingguang Wang (the Trendy Team)
;; 2022-04-29
;; ACKNOWLEDGEMENTS: We referenced several procedures from the Racket Guide, see references below.

(require csc151)
(require csc151www)
(require net/url)
; We learned (require net/url) from the Racket Guide.
(require csc151/rex)
(require 2htdp/image)


; This project finds the current trending words in five western news websites,
; including Fox, the Guardian, the Economist, ABC, and Wall Street Journal.
; Enter (economist-trending), (wall-street-trending), (fox-trending), (abc-trending) or (guardian-trending) in the
; interaction pane to get the five words that are trending at this moment in the five news websites.
; The five words and their frequencies are displayed in a bar chart with five individual bars. 

;;; (take-website url) -> sxml?
;;;   url : string?
;;; Takes in the url of a website and outputs
;;; the page source of the website in sxml form.
(define take-website
  (lambda (url)
    (string->xml (port->string
                  (get-pure-port (string->url url))))))
; We referenced (get-pure-port) and (port->string) from the Racket Guide.

;;; regular expression of letter
(define rex-letter (rex-any-of (rex-char-range #\a #\z)))

;;; (words? str) -> boolean?
;;;   str : string?
;;; Determines whether a string is a word.
(define words?
  (lambda (str)
    (rex-matches? (rex-concat (rex-repeat rex-letter)
                              (rex-repeat-0 (rex-any-char)))
                  str)))

;;; (count-a-word word lst) -> list?
;;;   word : string?
;;;   lst : list?
;;; Counts the frequency of a word in a list of strings case-insensitively.
(define count-a-word
  (lambda (word lst)
    (list word
          (tally-value (map string-downcase lst)
                       (string-downcase word)))))

;;; (count-words list-of-words lst) -> list?
;;;   list-of-words : listof-string?
;;;   lst : listof-string?
;;; Counts the frequency of words in the list-of-words that appear in lst,
;;; case-insensitively.
(define count-words
  (lambda (list-of-words lst)
    (remove-duplicates (map (section count-a-word <> lst)
                            (filter
                             words?
                             list-of-words)))))
; We referenced (remove-duplicates) from the Racket Guide

;;; (take-last lst) -> any?
;;;   lst : list?
;;; Takes the last element of a list.
(define take-last
  (lambda (lst)
    (list-ref lst (- (length lst) 1))))

;;; (fox-news-home) -> sxml?
;;; Outputs the homepage of Fox news website in sxml format.
(define fox-news-home
  (lambda ()
    (take-website "https://www.foxnews.com/")))

; The pattern on fox website for titles of articles.
(define fox-pattern "//h2//a")

;;; (the-economist-home) -> sxml?
;;; Outputs the homepage of the Economist website in sxml format.
(define the-economist-home
  (lambda ()
    (take-website "https://www.economist.com/")))

; The pattern on the economist website for titles of articles.
(define economist-pattern "//h3//a")

;;; (wall-street-home) -> sxml?
;;; Outputs the homepage of the Wall Street Journal website in sxml format.
(define wall-street-home
  (lambda ()
    (take-website "https://www.wsj.com/")))

; The three patterns on the Wall Street Journal website for titles of articles.
(define wall-street-pattern1 "//span[@class='WSJTheme--headlineText--He1ANr9C ']")
(define wall-street-pattern2 "//h3[@class='WSJTheme--title--3LUGJqsJ ']")
(define wall-street-pattern3 "//span[@class='']")

;;; (guardian-home) -> sxml?
;;; Outputs the homepage of the Guardian news website in sxml format.
(define guardian-home
  (lambda ()
    (take-website "https://www.theguardian.com/us")))

;
(define guardian-pattern "//li//div//div//h3//a//span[@class='js-headline-text']")

;;; (abc-home) -> sxml?
;;; Outputs the homepage of ABC news website in sxml format.
(define abc-home
  (lambda ()
    (take-website "https://abcnews.go.com/")))

(define abc-pattern "//h3[@class='VideoTile__Title']//span")

;;; (take-titles pattern homepage) -> list-of strings?
;;;   pattern : string?
;;;   homepage : sxml?
;;; Takes the pattern we are looking for and the page source of a website in sxml,
;;; and searches for all of the elements with the given pattern.
(define take-titles
  (lambda (pattern homepage)
    (remove-duplicates
     (map take-last (sxpath-match pattern homepage)))))

; A list of punctuations that we want to remove
(define punctuation-list
  (list #\,
        #\.
        #\/
        #\'
        #\"
        #\:
        #\!
        #\?
        #\–
        #\‘
        #\’))

;;; (remove-punctuation str) -> string?
;;;   str : string?
;;; Takes in a string and removes
;;; the punctuation at the start and the end of the string.
(define remove-punctuation
  (lambda (str)
    (let ([last-position (- (string-length str) 1)])
      (cond
        [(< last-position 1)
         str]             
        [(member (string-ref str 0) punctuation-list)
         (remove-punctuation (substring str 1))]
        [(member (string-ref str last-position) punctuation-list)
         (remove-punctuation (substring str 0 last-position))]
        [else
         str]))))

;;; A list of insignificant words created by ourselves
(define useless-words
  (file->lines "useless-words.txt"))

;;; (remove-useless-words title-words)
;;;   title-words : list-of strings?
;;; Takes in a list of words and remove all the words
;;; that appears in the file "useless-words.txt".
(define remove-useless-words
  (lambda (title-words)
    (filter (o not (section member <> useless-words)) title-words)))

;;; (title-words pattern homepage) -> list-of strings?
;;;   pattern : string?
;;;   homepage : sxml?
;;; Takes the pattern we are looking for and the page source of a website in sxml,
;;; and convert all the elements matching the pattern into a list of string,
;;; while every string is a single word.
(define title-words
  (lambda (pattern homepage)
    (remove-useless-words
     (map remove-punctuation
          (map string-downcase
               (string-split
                (reduce (section string-append <> " " <>)
                        (filter string? (take-titles pattern homepage)))))))))

;;; (economist-title-words) -> list-of strings?
;;; Outputs all of the words in the titles of the Economist homepage
;;; in a list of strings.
(define economist-title-words
  (lambda ()
    (title-words economist-pattern (the-economist-home))))

;;; (fox-title-words) -> list-of strings?
;;; Outputs all of the words in the titles of Fox news homepage
;;; in a list of strings.
(define fox-title-words
  (lambda ()
    (title-words fox-pattern (fox-news-home))))

;;; (guardian-title-words) -> list-of strings?
;;; Outputs all of the words in the titles of the Guardian homepage
;;; in a list of strings.
(define guardian-title-words
  (lambda ()
    (title-words guardian-pattern (guardian-home))))

;;; (wall-street-title-words) -> list-of strings?
;;; Outputs all of the words in the titles of the Wall Street Journal homepage
;;; in a list of strings.
(define wall-street-title-words
  (lambda ()
    (let ([home (wall-street-home)])
      (append (title-words wall-street-pattern1 home)
              (title-words wall-street-pattern2 home)
              (title-words wall-street-pattern3 home)))))

;;; (abc-title-words) -> list-of strings?
;;; Outputs all of the words in the titles of ABC news homepage
;;; in a list of strings.
(define abc-title-words
  (lambda ()
    (title-words abc-pattern (abc-home))))



;;; (word-sort title-words) -> list?
;;;   title-words : list?
;;; Takes a list of words 
;;; and outputs the words and their frequency from large to small in a list.
(define word-sort
  (lambda (title-words)
    (sort (count-words title-words title-words)
          (lambda (a b)
            (> (cadr a) (cadr b))))))

;;; (take-first-five-words title-words)
;;;   title-words : list-of strings?
;;; Takes a list of strings and get the five words
;;; with the highest frequency. 
(define take-first-five-words
  (lambda (title-words)
    (take (word-sort title-words) 5)))


;;; (bar label height num color) -> image?
;;;   label : string?
;;;   height : real?
;;;   num : real?
;;;   color : string?
;;; Creates individual bar with the label at the bottom,
;;; height, number, and color. 
(define bar
  (lambda (label height num color)
    (above (overlay (text (number->string height) 12 "blue")
                    (rectangle 40 num 'outline "black" )
                    (rectangle 40 num 'solid color ))
           (text label 12 "black"))))

;;; (bar-chart height label1 num1 label2 num2 label3 num3  label4 num4 label5 num5) -> image?
;;;   height : real?
;;;   label1 : string?
;;;   num1 : real?
;;;   label2 : string?
;;;   num2 : real?
;;;   label3 : string?
;;;   num3 : real?
;;;   label4 : string?
;;;   num4 : real?
;;;   label5 : string?
;;;   num 5  : real?
;;; Draws a bar chart with 5 bars which show the 5 trending words and their frequency.
(define bar-chart
  (lambda(title-words)
    (let* ([get-label (lambda (num) (car (list-ref (take-first-five-words title-words) num)))]
           [get-num (lambda (num) (cadr (list-ref (take-first-five-words title-words) num)))]
           [label1 (get-label 0)]
           [num1 (get-num 0)]
           [label2 (get-label 1)]
           [num2 (get-num 1)]
           [label3 (get-label 2)]
           [num3 (get-num 2)]
           [label4 (get-label 3)]
           [num4 (get-num 3)]
           [label5 (get-label 4)]
           [num5 (get-num 4)]
           [highest  (max num1 num2 num3 num4 num5)])
      (beside/align "bottom"
                    (bar label1 num1 (* (/ num1 highest) 100) "light orange")
                    (rectangle 20 1 'outline "white" )
                    (bar label2 num2 (* (/ num2 highest) 100) "pink")
                    (rectangle 20 1 'outline "white" )
                    (bar label3 num3 (* (/ num3 highest) 100) "teal")
                    (rectangle 20 1 'outline "white" )
                    (bar label4 num4 (* (/ num4 highest) 100) "grey")
                    (rectangle 20 1 'outline "white" )
                    (bar label5 num5 (* (/ num5 highest) 100) "orange")))))

;;; (economist-trending) -> image?
;;; Outputs the five words with the highest frequency in titles of the Economist
;;; in a bar chart with five individual bar. 
(define economist-trending
  (lambda ()
    (bar-chart (economist-title-words))))

;;; (guardian-trending) -> image?
;;; Outputs the five words with the highest frequency in titles of the Guardian
;;; in a bar chart with five individual bar. 
(define guardian-trending
  (lambda ()
    (bar-chart (guardian-title-words))))

;;; (fox-trending) -> image?
;;; Outputs the five words with the highest frequency in titles of Fox news
;;; in a bar chart with five individual bar. 
(define fox-trending
  (lambda ()
    (bar-chart (fox-title-words))))

;;; (wall-street-trending) -> image?
;;; Outputs the five words with the highest frequency in titles of Wall Street Journal
;;; in a bar chart with five individual bar. 
(define wall-street-trending
  (lambda ()
    (bar-chart (wall-street-title-words))))

;;; (abc-trending) -> image?
;;; Outputs the five words with the highest frequency in titles of ABC news
;;; in a bar chart with five individual bar. 
(define abc-trending
  (lambda ()
    (bar-chart (abc-title-words))))
