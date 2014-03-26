Feature: Search Packagist
  In order to find PHP libraries
  As a PHP developer
  I want to search Packagist

  Scenario: Searching Packages Puts Results In Correct Buffer
    When I run a search
    Then I should be in buffer "*Packagist*"
    And I should see "Packagist results for: phpunit"
    And I should see "Page 1"
    And I should not see "[Previous Page]"
    And I should see "[Next Page]"
    And I should see "phpunit/phpunit"

  Scenario: Going to Next Page Shows Next Page of Results
    When I run a search
    And I place the cursor between "[Next" and " Page]"
    And I press "<RET>"
    Then I should see "Page 2"
    And I should see "[Previous Page]"
    And I should see "[Next Page]"
    And I should not see "phpunit/phpunit"
    And I should see "whatthejeff/nyancat-phpunit-resultprinter"

