{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}
{                                              }
{   \\\                                        }
{  -(j)-                                       }
{    /juanca �                                 }
{    ~                                         }
{  Copyright � 1995-2002 Juancarlo A�ez        }
{  http://www.suigeneris.org/juanca            }
{  All rights reserved.                        }
{%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%}

{ $Id$ }
{: Collections: A Delphi port of the Java Collections library.
   @author  Juancarlo A�ez.
   @version $Revision$
}
unit JALAlgorithms;
interface
uses
   JALCollections;

const
  rcs_id :string = '@(#)$Id$';

type
   recurse_t = (rt_donotrecurse, rt_recurse);

   IUnaryFunction = interface
   ['{BBFBE12C-F88F-4C6A-946E-0E049FFC712E}']
      function run( obj: IUnknown ): IUnknown;
   end;

   IBinaryFunction = interface
   ['{9082D051-94F0-4008-86B2-F9DC92EA01CA}']
      function run( obj1, obj2: IUnknown ): IUnknown;
   end;

function forEach(coll :ICollection; func :IUnaryFunction) :IUnaryFunction; overload;
function inject( coll :ICollection; obj: IUnknown; func: IBinaryFunction ): IUnknown; overload;

function count( coll :ICollection; recurse :recurse_t = rt_donotrecurse) :Integer; overload;
function count( map  :IMap; recurse :recurse_t = rt_donotrecurse) :Integer; overload;


implementation

function forEach(coll :ICollection; func :IUnaryFunction) :IUnaryFunction; overload;
var
  i :IIterator;
begin
   i := coll.iterator;
   while i.hasNext do
       func.run(i.next);
   result := func
end;

function inject( coll :ICollection; obj: IUnknown; func: IBinaryFunction ): IUnknown; overload;
var
  i :IIterator;
begin
   i := coll.iterator;
   while i.hasNext do
       obj := func.run(obj, i.next);
   result := obj
end;

function count( coll :ICollection; recurse :recurse_t = rt_donotrecurse) :Integer;
var
  i   :IIterator;
  obj :IUnknown;
  sub :ICollection;
begin
   result := 0;
   i := coll.iterator;
   while i.hasNext do begin
       obj := i.next;
       if recurse = rt_donotrecurse then
          inc(result)
       else begin
           obj.queryInterface(ICollection, sub);
           if sub <> nil then
              inc(result, count(sub, recurse))
           else
              inc(result, 1);
       end
   end
end;

function count( map  :IMap; recurse :recurse_t = rt_donotrecurse) :Integer; overload;
var
  i   :IIterator;
  obj :IUnknown;
  sub :IMap;
begin
   result := 0;
   i := map.values.iterator;
   while i.hasNext do begin
       obj := i.next;
       if recurse = rt_donotrecurse then
          inc(result)
       else begin
           obj.queryInterface(IMap, sub);
           if sub <> nil then
              inc(result, count(sub, recurse))
           else
              inc(result, 1);
       end
   end
end;


end.
