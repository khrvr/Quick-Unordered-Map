#include <vector>
#include <iostream>
#include <cmath>
#include <tuple>

using std::pair;
using std::vector;

template<typename Key, typename Value, typename Hash = std::hash<Key>, typename Equal = std::equal_to<Key>, typename Allocator = std::allocator<pair<const Key, Value>>>
class UnorderedMap {

    class List;

    using NodeType = pair<const Key, Value>;
    using Node = typename List::Node;

    size_t max_sz = 10e6;
    float max_lf = 1;
    Hash hash;
    Equal equal;
    Allocator alloc;
    vector<Node*, typename std::allocator_traits<Allocator>:: template rebind_alloc<Node*>> innerVector;
    List innerList;

    void destroyPairs() {
        for (auto it = begin(); it != end(); ++it) {
            std::allocator_traits<Allocator>::destroy(alloc, it.getRef());
            std::allocator_traits<Allocator>::deallocate(alloc, it.getRef(), 1);
        }
    }

    void remove_all_elements() {
        destroyPairs();
        innerList.remove_all_elements();
        innerVector.clear();
    }

    void iterating_insert(const UnorderedMap& other) {
        try {
            for (auto it = other.begin(); it != other.end(); ++it) {
                insert(*it);
            }
        }
        catch (...) {
            remove_all_elements();
        }
    }

public:

    using iterator = typename List::iterator;
    using const_iterator = typename List::const_iterator;

    UnorderedMap();

    UnorderedMap(const Allocator& givenAlloc) : alloc(givenAlloc), innerVector(alloc), innerList(alloc) {}

    UnorderedMap(const UnorderedMap& other) : max_sz(other.max_sz), max_lf(other.max_lf), alloc(std::allocator_traits<Allocator>::select_on_container_copy_construction(other.alloc)) {
        for (auto it = other.begin(); it != other.end(); ++it) {
            insert(*it);
        }
    }

    void equal_allocator_swap(UnorderedMap& other) {
        std::swap(max_sz, other.max_sz);
        std::swap(max_lf, other.max_lf);
        std::swap(hash, other.hash);
        std::swap(equal, other.equal);
        std::swap(alloc, other.alloc);
        innerVector.swap(other.innerVector);
        innerList.equal_allocator_swap(other.innerList);
    }

    UnorderedMap& operator=(const UnorderedMap& other) {
        if (alloc == other.alloc) {
            UnorderedMap tmp = other;
            equal_allocator_swap(tmp);
        }
        else {
            remove_all_elements();
            max_sz = other.max_sz;
            max_lf = other.max_lf;
            hash = other.hash;
            equal = other.equal;
            if (std::allocator_traits<Allocator>::propagate_on_container_copy_assignment::value)
                alloc = other.alloc;
            iterating_insert(other);
        }
        return *this;
    }

    UnorderedMap(UnorderedMap&& other) : max_sz(other.max_sz), max_lf(other.max_lf), hash(other.hash), equal(other.equal), alloc(other.alloc), innerVector(std::move(other.innerVector)), innerList(std::move(other.innerList)) {}

    UnorderedMap& operator=(UnorderedMap&& other) {
        remove_all_elements();
        max_sz = other.max_sz;
        max_lf = other.max_lf;
        hash = other.hash;
        equal = other.equal;
        if (std::allocator_traits<Allocator>::propagate_on_container_move_assignment::value)
            alloc = other.alloc;
        innerVector = std::move(other.innerVector);
        innerList = std::move(other.innerList);
        return *this;
    }

    ~UnorderedMap() {
        destroyPairs();
    }

    size_t size() const noexcept {
        return innerList.size();
    }

    void rehash(size_t count) {
        if (max_lf * count < size()) return;
        vector<Node*, typename std::allocator_traits<Allocator>:: template rebind_alloc<Node*>> newInnerVector(count, nullptr);
        auto tmpBegin = begin();
        innerList.controllingNode.prev = innerList.controllingNode.next = &(innerList.controllingNode);
        for (auto itr = tmpBegin; itr != end();) {
            auto nextitr = itr;
            ++nextitr;
            size_t currentHash = itr.hash();
            if (newInnerVector[currentHash % count] == nullptr) {
                Node* replacedNode = itr.getNode();
                newInnerVector[currentHash % count] = replacedNode;
                replacedNode->next = innerList.controllingNode.next;
                innerList.controllingNode.next->prev = replacedNode;
                innerList.controllingNode.next = replacedNode;
                replacedNode->prev = &(innerList.controllingNode);
            }
            else {
                Node* node = newInnerVector[currentHash % count];
                while (true) {
                    if (node->next != &(innerList.controllingNode) && ((node->next)->hash % count) == (currentHash % count))
                        node = (node->next);
                    else
                        break;
                }
                Node* replacedNode = itr.getNode();
                replacedNode->next = node->next;
                node->next->prev = replacedNode;
                node->next = replacedNode;
                replacedNode->prev = node;
            }
            itr = nextitr;
        }
        innerVector.swap(newInnerVector);
    }

    void reserve(size_t count) {
        rehash(std::ceil(count / max_lf));
    }

    iterator begin() noexcept {
        return iterator(innerList.controllingNode.next);
    }

    iterator end() noexcept {
        return iterator(&innerList.controllingNode);
    }

    const_iterator begin() const noexcept {
        return const_iterator(innerList.controllingNode.next);
    }

    const_iterator end() const noexcept {
        return const_iterator(&innerList.controllingNode);
    }

    const_iterator cbegin() const noexcept {
        return const_iterator(innerList.controllingNode.next);
    }

    const_iterator cend() const noexcept {
        return const_iterator(&innerList.controllingNode);
    }

    const_iterator find(const Key& key) const {
        if (innerVector.size() == 0) return end();
        size_t currentHash = hash(key);
        if (innerVector[currentHash % innerVector.size()] != nullptr) {
            const_iterator itr(innerVector[currentHash % innerVector.size()]);
            while (itr != end() && (itr.hash() % innerVector.size()) == (currentHash % innerVector.size())) {
                if (equal(itr.key(), key)) return itr;
                ++itr;
            }
        }
        return end();
    }

    iterator find(const Key& key) {
        if (innerVector.size() == 0) return end();
        size_t currentHash = hash(key);
        if (innerVector[currentHash % innerVector.size()] != nullptr) {
            iterator itr(innerVector[currentHash % innerVector.size()]);
            while (itr != end() && (itr.hash() % innerVector.size()) == (currentHash % innerVector.size())) {
                if (equal(itr.key(), key)) return itr;
                ++itr;
            }
        }
        return end();
    }

    template<typename ...Args>
    pair<iterator, bool> emplace(Args&& ... args) {
        if (max_lf * innerVector.size() <= innerList.size() + 1) rehash(innerVector.size() * 2 + 1);
        NodeType* constructedPair = std::allocator_traits<Allocator>::allocate(alloc, 1);
        try {
            std::allocator_traits<Allocator>::construct(alloc, constructedPair, std::forward<Args>(args)...);
        }
        catch (...) {
            std::allocator_traits<Allocator>::deallocate(alloc, constructedPair, 1);
            throw;
        }
        innerList.push_front(constructedPair);
        iterator inserted = begin();
        size_t currentHash = hash(inserted.key());
        if (innerVector[currentHash % innerVector.size()] != nullptr) {
            iterator bucketItr(innerVector[currentHash % innerVector.size()]);
            while (bucketItr != end() && (bucketItr.hash() % innerVector.size()) == (currentHash % innerVector.size())) {
                if (equal(bucketItr.key(), inserted.key())) {
                    std::allocator_traits<Allocator>::destroy(alloc, constructedPair);
                    std::allocator_traits<Allocator>::deallocate(alloc, constructedPair, 1);
                    innerList.pop_front();
                    return { bucketItr, false };
                }
                ++bucketItr;
            }
            Node* nextNode = bucketItr.getNode();
            --bucketItr;
            Node* prevNode = bucketItr.getNode();
            Node* frontNode = innerList.controllingNode.next;
            frontNode->next->prev = &(innerList.controllingNode);
            innerList.controllingNode.next = frontNode->next;
            nextNode->prev = frontNode;
            prevNode->next = frontNode;
            frontNode->next = nextNode;
            frontNode->prev = prevNode;
            inserted.getNode()->hash = currentHash;
            return { inserted, true };
        }
        else {
            innerVector[currentHash % innerVector.size()] = inserted.getNode();
            inserted.getNode()->hash = currentHash;
            return { inserted, true };
        }
    }

    template<typename ...Args>
    pair<iterator, bool> try_emplace(const Key& key, Args&& ... args) {
        iterator itr = find(key);
        if (itr == end()) {
            return emplace(std::piecewise_construct,
                std::forward_as_tuple(key),
                std::forward_as_tuple(std::forward<Args>(args)...));
        }
        else {
            return { itr, false };
        }
    }

    template<typename ...Args>
    pair<iterator, bool> try_emplace(Key&& key, Args&& ... args) {
        iterator itr = find(key);
        if (itr == end()) {
            return emplace(std::piecewise_construct,
                std::forward_as_tuple(std::move(key)),
                std::forward_as_tuple(std::forward<Args>(args)...));
        }
        else {
            return { itr, false };
        }
    }

    Value& operator[](const Key& key) {
        return try_emplace(key).first->second;
    }

    Value& operator[](Key&& key) {
        return try_emplace(std::move(key)).first->second;
    }


    pair<iterator, bool> insert(const NodeType& node) {
        if (max_lf * innerVector.size() <= innerList.size() + 1) rehash(innerVector.size() * 2 + 1);
        size_t currentHash = hash(node.first);
        if (innerVector[currentHash % innerVector.size()] != nullptr) {
            iterator itr(innerVector[currentHash % innerVector.size()]);
            while (itr != end() && (itr.hash() % innerVector.size()) == (currentHash % innerVector.size())) {
                if (equal(itr.key(), node.first)) return { itr, false };
                ++itr;
            }
            NodeType* constructedPair = std::allocator_traits<Allocator>::allocate(alloc, 1);
            try {
                std::allocator_traits<Allocator>::construct(alloc, constructedPair, node);
            }
            catch (...) {
                std::allocator_traits<Allocator>::deallocate(alloc, constructedPair, 1);
                throw;
            }
            innerList.insert(itr, constructedPair);
            itr.getNode()->prev->hash = currentHash;
            return { itr, true };
        }
        else {
            NodeType* constructedPair = std::allocator_traits<Allocator>::allocate(alloc, 1);
            try {
                std::allocator_traits<Allocator>::construct(alloc, constructedPair, node);
            }
            catch (...) {
                std::allocator_traits<Allocator>::deallocate(alloc, constructedPair, 1);
                throw;
            }
            innerList.push_front(constructedPair);
            innerVector[currentHash % innerVector.size()] = (begin()).getNode();
            innerVector[currentHash % innerVector.size()]->hash = currentHash;
            return { begin(), true };
        }
    }

    template<typename P>
    pair<iterator, bool> insert(P&& node) {
        return emplace(std::forward<P>(node));
    }

    template<typename InputIt>
    void insert(InputIt first, InputIt last) {
        while (first != last) {
            insert(*first);
            ++first;
        }
    }

    void erase(const_iterator itr) {
        auto toBeDeleted = const_cast<NodeType*>(itr.getRef());
        if (innerVector[itr.getNode()->hash % innerVector.size()] == itr.getNode()) {
            if (itr.getNode()->next->hash % innerVector.size() == itr.getNode()->hash % innerVector.size())
                innerVector[itr.getNode()->hash % innerVector.size()] = itr.getNode()->next;
            else
                innerVector[itr.getNode()->hash % innerVector.size()] = nullptr;
        }
        innerList.erase(itr);
        std::allocator_traits<Allocator>::destroy(alloc, toBeDeleted);
        std::allocator_traits<Allocator>::deallocate(alloc, toBeDeleted, 1);
    }

    void erase(const_iterator first, const_iterator last) {
        while (first != last) {
            ++first;
            const_iterator next = first;
            --first;
            erase(first);
            first = next;
        }
    }

    Value& at(const Key& key) {
        iterator itr = find(key);
        if (itr == end())
            throw std::invalid_argument("No element with this key");
        else
            return itr.value();
    }

    const Value& at(const Key& key) const {
        const_iterator itr = find(key);
        if (itr == end())
            throw std::invalid_argument("No element with this key");
        else
            return itr.value();
    }

    float max_load_factor() const noexcept {
        return max_lf;
    }

    float load_factor() const noexcept {
        return innerVector.size() ? innerList.size() / innerVector.size() : 0;
    }

    void max_load_factor(float newmax_lf) {
        max_lf = newmax_lf;
        rehash(std::ceil(newmax_lf / max_lf));
    }

    size_t max_size() const noexcept {
        return max_sz;
    }

};


template<typename Key, typename Value, typename Hash, typename Equal, typename Allocator>
class UnorderedMap<Key, Value, Hash, Equal, Allocator>::List {
    using T = pair<const Key, Value>;

    struct Node {
        size_t hash = 0;
        T* value = nullptr;
        Node* prev = nullptr;
        Node* next = nullptr;
        Node() = default;
        Node(T* ptr) : value(ptr) {}
        Node(const Node& other) : hash(other.hash), value(other.value), prev(other.prev), next(other.next) {}

        void swap(Node& other) {
            std::swap(hash, other.hash);
            std::swap(value, other.value);
            std::swap(prev, other.prev);
            std::swap(next, other.next);
        }
    };

    using rebindedAlloc = typename std::allocator_traits<Allocator>::template rebind_alloc<Node>;
    using AllocTraits = std::allocator_traits<rebindedAlloc>;

    Node controllingNode;
    size_t sz = 0;
    rebindedAlloc alloc;

    void equal_allocator_swap(List& other) {
        controllingNode.swap(other.controllingNode);
        if (controllingNode.next == &other.controllingNode)
            controllingNode.next = controllingNode.prev = &controllingNode;
        controllingNode.prev->next = controllingNode.next->prev = &controllingNode;
        other.controllingNode.next->prev = &other.controllingNode;
        other.controllingNode.prev->next = &other.controllingNode;
        std::swap(sz, other.sz);
        std::swap(alloc, other.alloc);
    }

    void remove_all_elements() {
        while (sz > 0) {
            pop_front();
        }
    }

    void iterating_push_front(const List& other) {
        if (sz == 0) {
            controllingNode.prev = controllingNode.next = &controllingNode;
            try {
                for (auto it = other.begin(); it != other.end(); ++it) {
                    push_front(it.getRef());
                }
            }
            catch (...) {
                remove_all_elements();
            }
        }
    }

public:

    List();

    List(const Allocator& alloc) : alloc(alloc) {
        controllingNode.prev = controllingNode.next = &controllingNode;
    }

    List(const List& other) : alloc(AllocTraits::select_on_container_copy_construction(other.alloc)) {
        iterating_push_front(other);
    }

    List(List&& other) : controllingNode(other.controllingNode), sz(other.sz), alloc(other.alloc) {
        if (controllingNode.next == &other.controllingNode)
            controllingNode.next = controllingNode.prev = &controllingNode;
        controllingNode.next->prev = controllingNode.prev->next = &controllingNode;
        other.controllingNode = Node();
        other.controllingNode.next = other.controllingNode.prev = &(other.controllingNode);
        other.sz = 0;
    }

    List& operator=(List&& other) {
        remove_all_elements();
        controllingNode = other.controllingNode;
        if (controllingNode.next == &other.controllingNode)
            controllingNode.next = controllingNode.prev = &controllingNode;
        controllingNode.next->prev = controllingNode.prev->next = &controllingNode;
        sz = other.sz;
        if (Allocator::propagate_on_container_move_assignment::value)
            alloc = other.alloc;
        other.controllingNode = Node();
        other.controllingNode.next = other.controllingNode.prev = &(other.controllingNode);
        other.sz = 0;
        return *this;
    }

    List& operator=(const List& other) {
        if (alloc == other.alloc) {
            List tmp = other;
            equal_allocator_swap(tmp);
        }
        if(this != &other) {
            remove_all_elements();
            if (AllocTraits::propagate_on_container_copy_assignment::value)
                alloc = other.alloc;
            iterating_push_front(other);
        }
        return *this;
    }

    ~List() {
        remove_all_elements();
    }

    size_t size() const {
        return sz;
    }

    void push_front(T* const ptr) {
        Node* newFirstElement = AllocTraits::allocate(alloc, 1);
        try {
            AllocTraits::construct(alloc, newFirstElement, ptr);
        }
        catch (std::exception& exc) {
            AllocTraits::deallocate(alloc, newFirstElement, 1);
            throw;
        }
        newFirstElement->prev = &controllingNode;
        newFirstElement->next = controllingNode.next;
        controllingNode.next->prev = newFirstElement;
        controllingNode.next = newFirstElement;
        ++sz;
    }

    void pop_front() {
        Node* newFirstElement = controllingNode.next->next;
        Node* toBeDeleted = (controllingNode.next);
        AllocTraits::destroy(alloc, toBeDeleted);
        AllocTraits::deallocate(alloc, toBeDeleted, 1);
        controllingNode.next = newFirstElement;
        controllingNode.next->prev = &controllingNode;
        --sz;
    }

    template<bool isConst>
    class iteratorT {
        using node_pointer = std::conditional_t<isConst, const Node*, Node*>;

        node_pointer node;

    public:

        using value_type = std::conditional_t<isConst, const T, T>;
        using pointer = std::conditional_t<isConst, const T*, T*>;
        using reference = std::conditional_t<isConst, const T&, T&>;
        using difference_type = std::ptrdiff_t;
        using iterator_category = std::forward_iterator_tag;
        using hash_size_reference = std::conditional_t<isConst, const size_t&, size_t&>;

        iteratorT() = delete;

        iteratorT(const iteratorT<false>& iter) : node(iter.node) {}

        iteratorT(node_pointer otherNode) : node(otherNode) {}

        bool operator==(const iteratorT& other) {
            return node == other.node;
        }

        bool operator!=(const iteratorT& other) {
            return node != other.node;
        }

        iteratorT& operator++() {
            node = node->next;
            return *this;
        }

        iteratorT operator++(int) {
            iteratorT tmp = *this;
            operator++();
            return tmp;
        }

        iteratorT& operator--() {
            node = node->prev;
            return *this;
        }

        iteratorT operator--(int) {
            iteratorT tmp = *this;
            operator++();
            return tmp;
        }

        hash_size_reference hash() const {
            return node->hash;
        }

        Value& value() const {
            return node->value->second;
        }

        const Key& key() const {
            return node->value->first;
        }

        node_pointer getNode() const {
            return node;
        }

        reference operator*() const {
            return *(node->value);
        }

        pointer operator->() const {
            return node->value;
        }

        T* getRef() const {
            return node->value;
        }


        friend class iteratorT<false>;
        friend class iteratorT<true>;
    };

    using iterator = iteratorT<false>;
    using const_iterator = iteratorT<true>;


    void insert(const_iterator it, T* const ptr) {
        const_iterator nextNode = it;
        --it;
        const_iterator prevNode = it;
        Node* newNode = AllocTraits::allocate(alloc, 1);
        try {
            AllocTraits::construct(alloc, newNode, ptr);
        }
        catch (std::exception& exc) {
            AllocTraits::deallocate(alloc, newNode, 1);
            throw;
        }
        Node* prevNodeNode = const_cast<Node*>(prevNode.getNode());
        Node* nextNodeNode = const_cast<Node*>(nextNode.getNode());
        prevNodeNode->next = newNode;
        nextNodeNode->prev = newNode;
        newNode->next = nextNodeNode;
        newNode->prev = prevNodeNode;
        ++sz;
    }

    void erase(const_iterator it) {
        ++it;
        const_iterator nextNode = it;
        --it;
        --it;
        const_iterator prevNode = it;
        ++it;
        Node* toBeDeleted = const_cast<Node*>(it.getNode());
        AllocTraits::destroy(alloc, toBeDeleted);
        AllocTraits::deallocate(alloc, toBeDeleted, 1);
        Node* prevNodeNode = const_cast<Node*>(prevNode.getNode());
        Node* nextNodeNode = const_cast<Node*>(nextNode.getNode());
        nextNodeNode->prev = prevNodeNode;
        prevNodeNode->next = nextNodeNode;
        --sz;
    }

    friend class UnorderedMap<Key, Value, Hash, Equal, Allocator>;
};

template<typename Key, typename Value, typename Hash, typename Equal, typename Allocator>
UnorderedMap<Key, Value, Hash, Equal, Allocator>::List::List() {
    controllingNode.prev = controllingNode.next = &controllingNode;
}

template<typename Key, typename Value, typename Hash, typename Equal, typename Allocator>
UnorderedMap<Key, Value, Hash, Equal, Allocator>::UnorderedMap() : innerVector(alloc), innerList(alloc) {}

template<typename T, size_t N>
class StackAllocator {
    StackStorage<N>* memory;
public:
    using value_type = T;

    StackAllocator() = delete;

    StackAllocator(StackStorage<N>& storage): memory(&storage) {}

    StackAllocator(const StackAllocator& other): memory(other.memory) {}

    template<typename U>
    StackAllocator(const StackAllocator<U, N>& other): memory(other.getMemory()) {}

    StackAllocator& operator=(StackAllocator other) {
        std::swap(memory, other.memory);
        return *this;
    }

    T* allocate(size_t sz) {
        uint8_t* bytePtr = memory->getMemory(sz * sizeof(T));
        return reinterpret_cast<T*>(bytePtr);
    }

    void deallocate(T* ptr, size_t sz) {
        memory->clear(reinterpret_cast<uint8_t*>(ptr), sz * sizeof(T));
    }

    StackStorage<N>* getMemory() const {
        return memory;
    }

    template<typename U>
    struct rebind {
        using other = StackAllocator<U, N>;
    };

    bool operator==(const StackAllocator& other) {
        return memory == other.memory;
    }

    bool operator!=(const StackAllocator& other) {
        return memory != other.memory;
    }

};
