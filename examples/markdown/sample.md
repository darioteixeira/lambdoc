Paragraphs and inline formatting
================================

You can format text in **bold**, or just *emphasised* if you prefer.  It is also
possible to enter `monospaced` text.  And of course, you may also enter links
such as [this one](http://en.wikipedia.org/wiki/Markdown).

Lists
=====

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.

1. This is the first reason;
2. This is the second reason; it is okay
   to break a line within each item.
3. The third reason has a number of sub-clauses:
   - Alpha
   - Beta
   - Gamma
4. And here is the fourth and final reason.

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.

Quotations
==========

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.

> Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula
> eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient
> montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu,
> pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel,
> aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis
> vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.

This is a section
=================

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.


This is a sub-section
---------------------

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.


### This is a sub-sub-section

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.


Source environments
===================

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.

````caml
type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree

let rec count = function
    | Leaf                     -> 0
    | Node (node, left, right) -> 1 + (count left) + (count right);;
````

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.


Verbatim environments
=====================

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.

````
       -------
       |  A  |
       -------
          |
          |
          |
         / \
        /   \
       /     \
      /       \
     /         \
    /           \
-------       -------
|  A  |       |  C  |
-------       -------
````

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget
dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes,
nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium
quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet
nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae,
justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt.

