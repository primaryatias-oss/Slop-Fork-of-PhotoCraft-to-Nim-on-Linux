## Reader for native Affinity documents (port of the `photocraft-affinity` crate): `.af`, and
## `.afdesign`/`.afphoto`/`.afpub` from Affinity 1 and 2. The archive container, the tagged object
## stream in `doc.dat`, and the subset of the document model PhotoCraft imports.
##
## Layouts follow the MIT-licensed afread reference and were checked against public documents
## saved by Affinity 1.x to 3.x (container versions 8 to 12). Nothing here writes Affinity files.

import ./affinity/[container, stream, types, model, preview]
export container, stream, types, model, preview
