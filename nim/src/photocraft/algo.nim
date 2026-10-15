## PhotoCraft's image filters and imaging algorithms (port of `photocraft_algo`). Importing this
## module registers every ported filter with the dispatcher in `algo/filters` (`apply`,
## `outputArea`, `kernel`).

import algo/[filters, basic_filters, blur_filters, fx_filters]
export filters
