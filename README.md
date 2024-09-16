# Borders

Borders is a tool to help choose plants for a garden border. Users are first 
asked to enter their US ZIP Code and then taken to a page that allows them to 
explore border plants for their USDA hardiness zone (there is also an option to 
enter their hardiness zone directly in case they live outside the US). They 
can then add plants to their border, which can be viewed in a right-side 
Offcanvas panel.

There are three ways a user can update their border:

- By clicking on "Add to border" or "Remove" on a plant card.
- By searching for a plant using the "Find a plant" dropdown.
- By removing a plant from the Offcanvas panel directly.

## Design

This app was built using the [bslib](https://rstudio.github.io/bslib/) package 
and makes heavy use of Bootstrap 5 responsive classes. It was set up for both 
desktop and mobile displays.

In order to keep the UI light for mobile displays, this app makes use of 
Bootstrap's [Offcanvas](https://getbootstrap.com/docs/5.0/components/offcanvas/) 
for viewing your border and selecting filters on the **Choose plants** tab.

Plant icons used in this app are from the [Noun Project](https://thenounproject.com/). 
When possible, plants are mapped to an icon based on plant type.

## Structure

This app is structured as an R package primarily for organizational purposes. It 
isn't set up to be distributed and installed like a regular package. Although 
`golem` was not used for development, some of its naming conventions have been 
borrowed:

- Files with a `mod_` prefix are modules
- Files with a `fct_` prefix are functions or collections of functions

The modules used in this app are:

- `mod_choose_plants` - A module for the **Choose plants** tab
- `mod_border_info` - A modules for the **Border info** tab
- `mod_plant_card` - A module for the creating plant cards within the 
**Choose plants** tab. The UI part of this module is also used in the care 
guide.

## Data

### Plant data

The plant data in this app come from the [Perenual API](https://perenual.com/docs/api). Three different endpoints were used:

- A species list endpoint that allows for a limited set of filters to be passed 
via url arguments and returns a list of basic information about plants that match
the filters. Only one page of results is returned at a time.
- A species details endpoint that provides richer information about each plant
but can only return data for one plant per request and cannot be filtered 
directly.
- A care guide api, which also only returns data for one plant at a time.

The following sections go into detail about how results from these endpoints 
were combined.

#### Applying filters

Applying filters based on arguments accepted by requests to the species list
endpoint is straightforward, but a few filters that are important for the 
purposes of this app can't be passed as arguments to species list endpoint.

##### Excluding trees

Since the focus is on border plants, trees are filtered out by default. A lookup 
list of tree ids was constructed to help with this.

##### Excluding plants with no image

Image fields are null for a large number of plants in the database. To the 
extent possible, these plants are filtered out by default (though this can be
changed in the filters selection), but there are cases when an image link is 
included but the link is broken. A placeholder image is used to avoid ugly 
errors in the display when this occurs.

##### Filtering by flower color 

Flower color is of particular interest in designing a border, but there were 
a number of challenges with using the API to filter by flower color:

- Flower color is only available as a field in the details endpoint, which can
only return data one ID at a time
- The flower color field is an inconsistently formatted text string
- Flower color is listed for plants whose flowers are not of interest
- Filtering to __flower__ based on the plant type field is too restrictive and 
excludes many plants with attractive flowers

To address these challenges, a lookup table was created of flower color by plant 
ID, using a text string to match names with flowering plants. When the flower 
color field contained multiple colors, only the first color listed was used. 
This results in better correspondence between plant images and the color 
selected. There are still cases when flower color doesn't match the images 
shown, but most of the results are reasonable.

#### Returning API results from the app

When a user clicks __Apply filters__, what happens behind the scenes is rather complicated. Getting results involves a series of steps:

1. If new filters are applied, request the first page of results from the 
species list endpoint, using any filters that can be passed as arguments. Keep 
track of the total number of pages and then request another few random pages to 
mix in for variety.
2. Filter the resulting plant list by additional filters that can only be 
applied post hoc (removing trees, removing plants with no image, or filtering by 
color).
3. If any plants remain, get the details for those plants one at a time from the 
details endpoint. Otherwise, select a few more random pages and get more 
potential matches.
4. Repeat steps 2 and 3, stopping when you have enough results to show or the 
number of attempts is too high to avoid overwhelming the API.
5. Save any remaining page numbers in the results set and any remaining IDs from
the most recent batch of pages.

When a user clicks __Show more__, remaining pages are used until they run out, 
at which point the __Show more__ button is disabled and reads "End of 
results".

If an API error occurs at any point, intermediate results will be returned. If 
there are no intermediate results available, the user will be shown an API 
error modal.

Several functions have been created to aid in this process:

- `req_species_list`: Make a single request to the species list endpoint.
- `req_details`: Make a single request to the details endpoint.
- `req_care_guide`: Make a single request to the care guide endpoint.
- `process_page_resp`: Apply post hoc filters to a page of results from the 
species list. 
- `req_page_batch`: Request a batch of pages from the species endpoint.
- `req_details_batch`: Request a batch of details from the details endpoint.
- `req_more_plants`: A higher level function that combines `req_page_batch` and
`req_details_batch` to try to get more plants based on a set of filters.

### Hardiness zone data

The hardiness zone information in this app come from the [PRISM Climate Group](https://prism.oregonstate.edu/). Zones were mapped to US city names using 
data from [United States Zip Codes.org](https://www.unitedstateszipcodes.org/).

## Bookmarking

This app uses a simple form of bookmarking to allow users to save their border 
by creating a bookmark in their browser. The url stores a user's zone and the 
IDs of plants in their border. Filters are not saved.
