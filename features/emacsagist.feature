Feature: Search Packagist
  In order to find PHP libraries
  As a PHP developer
  I want to search Packagist

  Scenario: Search Packagist for libraries
    When I run a search
    Then I should be in buffer "*Packagist*"
