# LIST Localities Profiles - HSCP version

<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-10-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->

This is a fork of the main Localities Profiles repository [https://github.com/Public-Health-Scotland/list-localities-profiles]() it should be kept up to date with the changes in that repo, whilst maintaining the differences that enable us to produce the profiles at HSCP level.

### Directories

- The code for Locality Profiles can be found on GitHub at [Public-Health-Scotland/list-localities-profiles](https://github.com/Public-Health-Scotland/list-localities-profiles). The latest version, which is mostly tested, will be on the `main` branch. The specific code used in each release can be found by checking the releases section.

- The code for HSCP Profiles is available in the forked repository [Public-Health-Scotland/list-hscp-profiles](https://github.com/Public-Health-Scotland/list-hscp-profiles). This repository is maintained as a fork, allowing any relevant changes made to the Locality Profiles to be synchronised.

- Code for 'custom profiles', which are defined at boundaries other than Locality, is currently maintained on branches within the [Locality Profiles repo](https://github.com/Public-Health-Scotland/list-localities-profiles). A few draft Pull Requests are kept open to facilitate easy reference.

- Both input and output data can be located on the stats drive at: `\\stats\LIST_analytics\West Hub\02 - Scaled Up Work\RMarkdown\Locality Profiles\`

  - Each chapter has its folder used to save input data before a release. The folders include: `Demographics`, `General Health`, `Households`, `Lifestyle & Risk Factors`, `Services`, and `Unscheduled Care`.

  - The `Master RMarkdown Document & Render Code\Output` folder is designated for saving all outputs.

  - The `Final Profiles` folder (and its subdirectories) is used to store completed profiles and related files that are ready for access by LIST colleagues.

### Files

Here are some important files and scripts to be aware of:

- **`Master RMarkdown Document & Render Code/Global Script.R`**: This script loads the necessary packages and declares custom functions. It is sourced by most other scripts.
  
- **`Master RMarkdown Document & Render Code/Locality Profiles Render Code.R`**: This script runs the analyses and generates the indicators, as well as creates the main profile and summary table outputs. It is the simplest and most effective way to produce the profiles.

- **`Master RMarkdown Document & Render Code/Locality_Profiles_Master_Markdown.Rmd`**: This is the primary RMarkdown file that contains all the content for the profiles, excluding the summary table, which is generated separately.

  - Note that there are corresponding RMarkdown documents for each chapter. The contents of each 'testing markdown' should mirror the relevant content in the main RMarkdown file exactly. These documents, such as `Demographics/Demographics-Testing-Markdown.Rmd`, are useful for quickly testing changes to an individual chapter.

- **`Summary Table/Summary-Table-Markdown.Rmd`**: This script generates the Summary Table, which is produced separately because it is in landscape orientation, while the rest of the profiles are in portrait orientation.

- **`Master RMarkdown Document & Render Code/make_sdc_output.R`**: This script creates the Statistical Disclosure Control (SDC) Excel workbooks.

- **`Master RMarkdown Document & Render Code/excel_output.R`**: This script generates the background data Excel workbooks.

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tbody>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/Andrew-Saul"><img src="https://avatars.githubusercontent.com/u/94113765?v=4?s=100" width="100px;" alt="Andrew-Saul"/><br /><sub><b>Andrew-Saul</b></sub></a><br /><a href="https://github.com/Public-Health-Scotland/list-localities-profiles/issues?q=author%3AAndrew-Saul" title="Bug reports">🐛</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/CliveWG"><img src="https://avatars.githubusercontent.com/u/109799428?v=4?s=100" width="100px;" alt="Clive Greatorex"/><br /><sub><b>Clive Greatorex</b></sub></a><br /><a href="https://github.com/Public-Health-Scotland/list-localities-profiles/commits?author=CliveWG" title="Code">💻</a> <a href="https://github.com/Public-Health-Scotland/list-localities-profiles/pulls?q=is%3Apr+reviewed-by%3ACliveWG" title="Reviewed Pull Requests">👀</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/cfraser2020"><img src="https://avatars.githubusercontent.com/u/72806160?v=4?s=100" width="100px;" alt="Craig Fraser "/><br /><sub><b>Craig Fraser </b></sub></a><br /><a href="https://github.com/Public-Health-Scotland/list-localities-profiles/commits?author=cfraser2020" title="Code">💻</a> <a href="https://github.com/Public-Health-Scotland/list-localities-profiles/pulls?q=is%3Apr+reviewed-by%3Acfraser2020" title="Reviewed Pull Requests">👀</a> <a href="#projectManagement-cfraser2020" title="Project Management">📆</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://publichealthscotland.scot/"><img src="https://avatars.githubusercontent.com/u/5982260?v=4?s=100" width="100px;" alt="James McMahon"/><br /><sub><b>James McMahon</b></sub></a><br /><a href="https://github.com/Public-Health-Scotland/list-localities-profiles/commits?author=Moohan" title="Code">💻</a> <a href="https://github.com/Public-Health-Scotland/list-localities-profiles/pulls?q=is%3Apr+reviewed-by%3AMoohan" title="Reviewed Pull Requests">👀</a> <a href="#projectManagement-Moohan" title="Project Management">📆</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/JosieSteele"><img src="https://avatars.githubusercontent.com/u/166606214?v=4?s=100" width="100px;" alt="Josie Steele"/><br /><sub><b>Josie Steele</b></sub></a><br /><a href="https://github.com/Public-Health-Scotland/list-localities-profiles/commits?author=JosieSteele" title="Code">💻</a> <a href="https://github.com/Public-Health-Scotland/list-localities-profiles/pulls?q=is%3Apr+reviewed-by%3AJosieSteele" title="Reviewed Pull Requests">👀</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/kenneo03git"><img src="https://avatars.githubusercontent.com/u/153850030?v=4?s=100" width="100px;" alt="Kennedy Owusu-Afriyie"/><br /><sub><b>Kennedy Owusu-Afriyie</b></sub></a><br /><a href="https://github.com/Public-Health-Scotland/list-localities-profiles/pulls?q=is%3Apr+reviewed-by%3Akenneo03git" title="Reviewed Pull Requests">👀</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/khansh01"><img src="https://avatars.githubusercontent.com/u/114073136?v=4?s=100" width="100px;" alt="Khansa Hussain"/><br /><sub><b>Khansa Hussain</b></sub></a><br /><a href="https://github.com/Public-Health-Scotland/list-localities-profiles/commits?author=khansh01" title="Code">💻</a> <a href="https://github.com/Public-Health-Scotland/list-localities-profiles/pulls?q=is%3Apr+reviewed-by%3Akhansh01" title="Reviewed Pull Requests">👀</a></td>
    </tr>
    <tr>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/RyanDuffy01"><img src="https://avatars.githubusercontent.com/u/82060940?v=4?s=100" width="100px;" alt="RyanDuffy01"/><br /><sub><b>RyanDuffy01</b></sub></a><br /><a href="https://github.com/Public-Health-Scotland/list-localities-profiles/commits?author=RyanDuffy01" title="Code">💻</a> <a href="https://github.com/Public-Health-Scotland/list-localities-profiles/pulls?q=is%3Apr+reviewed-by%3ARyanDuffy01" title="Reviewed Pull Requests">👀</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/There717"><img src="https://avatars.githubusercontent.com/u/79308287?v=4?s=100" width="100px;" alt="Theresa Campbell"/><br /><sub><b>Theresa Campbell</b></sub></a><br /><a href="https://github.com/Public-Health-Scotland/list-localities-profiles/commits?author=There717" title="Code">💻</a> <a href="https://github.com/Public-Health-Scotland/list-localities-profiles/pulls?q=is%3Apr+reviewed-by%3AThere717" title="Reviewed Pull Requests">👀</a></td>
      <td align="center" valign="top" width="14.28%"><a href="https://github.com/robbiemac01"><img src="https://avatars.githubusercontent.com/u/109292606?v=4?s=100" width="100px;" alt="robbiemac01"/><br /><sub><b>robbiemac01</b></sub></a><br /><a href="https://github.com/Public-Health-Scotland/list-localities-profiles/issues?q=author%3Arobbiemac01" title="Bug reports">🐛</a></td>
    </tr>
  </tbody>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome!
