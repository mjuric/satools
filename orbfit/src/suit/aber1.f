* Copyright (C) 1997 by Mario Carpino (carpino@brera.mi.astro.it)
* Version: February 24, 1997
*
*  ***************************************************************
*  *                                                             *
*  *                          A B E R 1                          *
*  *                                                             *
*  *          Correzione approssimata per aberrazione            *
*  *                  stellare e/o planetaria                    *
*  *                                                             *
*  ***************************************************************
*
*
* INPUT:    XREL(3)   -  Posizione relativa vera del corpo osservato (UA)
*           VREL(3)   -  Velocita` relativa (UA/d)
*
* OUTPUT:   XCOR(3)   -  Posizione relativa apparente (tenendo conto della
*                        aberrazione)
*
* NOTA: in generale per ottenere la correzione completa (comprendente le
*       cosiddette aberrazioni "stellare" + "planetaria") bisogna che VREL
*       sia la velocita` relativa del corpo osservato rispetto all'osserva-
*       tore:
*                 VREL  =   V(pianeta) - V(osservatore)
*
*       Se si vuole ottenere solo la correzione per l'aberrazione "stellare"
*       bisogna porre VREL =  - V(osservatore).
*
*
      subroutine aber1(xrel,vrel,xcor)
      implicit double precision (a-h,o-z)
      dimension xrel(3),vrel(3),xcor(3)
* Velocita` della luce in UA/d
      data c/173.14463272d0/
      ro=vsize(xrel)
c  effetto di ritardo
      dt=ro/c
      do 1 j=1,3
 1    xcor(j)=xrel(j)-dt*vrel(j)
      return
      end
