print(
  sum(
    [
      len(
        set(
            "".join(group.split())
        )
      ) 
      for group in open("input","r").read().split('\n\n')
    ]
  )
)
