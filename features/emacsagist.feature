Feature: Search Packagist
  In order to find PHP libraries
  As a PHP developer
  I want to search Packagist

  Scenario: Searching Packages Puts Results In Correct Buffer
    When I run a search
    Then I should be in buffer "*Packagist*"
    And I should see "[Next Page]"
    And I should see "phpunit/phpunit"
