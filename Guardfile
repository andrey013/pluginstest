guard :shell, all_on_start: true do
  watch /.*\.l?hs$/ do |m|
    puts "\n\n\nCompiling..."
    `runghc -package ghc #{m[0]} && echo "Compiled!"`
  end
end
