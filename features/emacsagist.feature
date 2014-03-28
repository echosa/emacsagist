Feature: Search Packagist
  In order to find PHP libraries
  As a PHP developer
  I want to search Packagist

  Scenario: Searching for packages
    When I run a search
    Then I should be in buffer "*Packagist*"
    And I should see "Packagist results for: phpunit"
    And I should see "Page 1"
    And I should not see "[Previous Page]"
    And I should see "[Next Page]"
    And I should see "phpunit/phpunit"

  Scenario: Going to next and previous pages
    When I run a search
    And I place the cursor between "[Next" and " Page]"
    And I press "<RET>"
    Then I should see "Packagist results for: phpunit"
    And I should see "Page 2"
    And I should see "[Previous Page]"
    And I should see "[Next Page]"
    And I should not see "phpunit/phpunit"
    And I should see "whatthejeff/nyancat-phpunit-resultprinter"
    When I place the cursor between "[Previous" and " Page]"
    Then I press "<RET>"
    And I should see "Packagist results for: phpunit"
    And I should see "Page 1"
    And I should not see "[Previous Page]"
    And I should see "[Next Page]"
    And I should see "phpunit/phpunit"

  Scenario: Jumping between links
    When I run a search
    Then the cursor should be at point "1"
    When I press "<backtab>"
    Then the cursor should be at point "1"
    When I press "<tab>"
    Then the cursor should be before "[Next Page]"
    When I press "<tab>"
    Then the cursor should be before "https"
    When I press "<backtab>"
    Then the cursor should be before "[Next Page]"
    When I press "<backtab>"
    Then the cursor should be before "[Next Page]"
    When I go to end of buffer
    And I press "<tab>"
    Then I should be at the end of buffer
    When I press "<backtab>"
    Then the cursor should be before "[Next Page]"


