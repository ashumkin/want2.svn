Dante Build Management Tool for Delphi
Copyright (c) 2001, Dante Authors -- See authors.txt for complete list
All rights reserved.
See license.txt for full license.
http://dante.sourceforge.net


What
----

Dante is a no-cost, open source build management tool for Borland's Delphi.
Dante wraps (or will wrap) common tasks like compiling Delphi code, basic shell
commands, version control for popular packages and the like. Dante executes a
build.xml file that directs its tasks.

*******
WARNING
*******
Dante is in a pre-release stage, so please use Dante with caution, especially
with destructive shell tasks like delete.


Why
---

Necessity is the mother of invention.

Delphi ships with Borland's make.exe. Juanco was using make to build DUnit
(http://dunit.sourceforge.net), but ran into some 'problem behavior' with the
tool and wanted something else. I was unfamiliar with make, had little luck
finding any documentation for it and decided to built a custom tool for my use
on the job.

Ant (http://jakarta.apache.org/ant) had become a popular make replacement for
the Java world and members of the DUnit team thought it would be great to have a
similar tool for Delphi.

After obtaining permission from my employer to donate some of my code to the
effort, I setup shop at SourceForge and work began.

Dante technically stands for Delphi Ant Environment (penned by Steve Tendon),
but the Ant license states we need to obtain approval to use the Ant name for
endorsements or promotions, so it doesn't get mentioned much. (A request for
approval is currently in progress).


Um, Documentation?
------------------

Don't have much yet. There is a great task writer's guide Juanco wrote up for
those interested in adding custom tasks. For some general info, you can hit the
Ant site as we've used their design as a starting point.


Acknowledgements
----------------

While my name is on the author list and I'm currently admining the project, I've
merely been a catalyst. Originally, I had code to donate to the project, but it
turned out to be easier to start from scratch. Juanco got on a tear early on and
has written most of the core code for Dante. Many, many thanks to him.

Additional thanks to the folks at SourceForge for hosting the project and making
it easy to do so.

Thanks to the Ant project for the inspiration.

Chris Morris
4/5/2001
$Id$